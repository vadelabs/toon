(ns com.vadelabs.toon.decode.decoders
  "Root dispatchers for TOON format decoding.

  This namespace provides the main entry points for decoding TOON content,
  dispatching to specialized decoders for arrays, objects, and primitives."
  (:require
    [clojure.string :as str]
    [com.vadelabs.toon.decode.arrays :as arrays]
    [com.vadelabs.toon.decode.items :as items]
    [com.vadelabs.toon.decode.objects :as objects]
    [com.vadelabs.toon.decode.parser :as parser]
    [com.vadelabs.toon.decode.scanner :as scanner]
    [com.vadelabs.toon.utils :as str-utils]))


;; ============================================================================
;; Root Dispatchers
;; ============================================================================

(defn array-from-header
  "Decodes an array based on its header format.

  Dispatches to the appropriate decoder:
  - Inline values → arrays/inline-primitive-array
  - Fields present → arrays/tabular-array
  - Otherwise → arrays/list-array

  Parameters:
    - header-info: Parsed array header map
    - cursor: LineCursor positioned after header
    - depth: Expected depth for array content
    - strict: Enable strict mode validation

  Returns:
    [decoded-array, new-cursor]"
  ([header-info cursor depth]
   (array-from-header header-info cursor depth true))
  ([header-info cursor depth strict]
   (cond
     ;; Inline array: [3]: a,b,c
     (:inline-values header-info)
     [(arrays/inline-primitive-array header-info strict) cursor]

     ;; Tabular array: [2]{id,name}: 1,Alice / 2,Bob
     (:fields header-info)
     (arrays/tabular-array header-info cursor depth strict)

     ;; List array: [2]: - item1 / - item2
     :else
     (arrays/list-array header-info cursor depth strict items/list-item))))


(defn value-from-lines
  "Decodes TOON content from parsed lines (root dispatcher).

  Determines the root form type and dispatches to appropriate decoder:
  - Array header → array-from-header
  - Single non-key-value line → parser/primitive-token
  - Otherwise → objects/object

  Parameters:
    - cursor: LineCursor starting at first line
    - indent: Indent size for parsing
    - strict: Enable strict mode validation

  Returns:
    Decoded Clojure value"
  ([cursor _indent strict]
   (let [first-line (scanner/peek-cursor cursor)]
     (if-not first-line
       ;; Empty input: return empty map
       {}
       (let [content (:content first-line)]
         (cond
           ;; Empty array at root: [0]
           (and (str/starts-with? (str/trim content) "[")
                (re-matches #"^\[0[,\t|]?\]$" (str/trim content)))
           []

           ;; Array header at root (no key before bracket)
           (and (str/includes? content "[")
                (str/includes? content "]")
                (str/includes? content ":")
                (str/starts-with? (str/trim content) "["))
           (let [header-info (parser/array-header-line content)
                 cursor-after-header (scanner/advance-cursor cursor)
                 depth 1
                 [result _] (array-from-header header-info cursor-after-header depth strict)]
             result)

           ;; Check if it's a single-line primitive (no colon)
           (and (nil? (scanner/peek-at-depth cursor 1))
                (not (str-utils/unquoted-char content \:)))
           (parser/primitive-token content strict)

           ;; Otherwise: root object
           :else
           (let [[result _] (objects/object cursor 0 "," strict items/list-item)]
             result)))))))
