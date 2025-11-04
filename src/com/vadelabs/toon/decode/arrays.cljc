(ns com.vadelabs.toon.decode.arrays
  "Array decoding for TOON format.

  Handles inline, tabular, and list array formats."
  (:require
    [clojure.string :as str]
    [com.vadelabs.toon.constants :as const]
    [com.vadelabs.toon.decode.parser :as parser]
    [com.vadelabs.toon.decode.scanner :as scanner]
    [com.vadelabs.toon.shared.string-utils :as str-utils]))


;; Forward declaration for mutual recursion
(declare list-item)


;; ============================================================================
;; Inline Primitive Array Decoding
;; ============================================================================

(defn inline-primitive-array
  "Decodes an inline primitive array.

  Format: [3]: a,b,c

  Parameters:
    - header-info: Map from parse-array-header-line
    - strict: Enable strict mode validation (default true)

  Returns:
    Vector of parsed primitive values

  Strict mode validates:
    - Array length matches header"
  ([header-info]
   (inline-primitive-array header-info true))
  ([header-info strict]
   (let [{:keys [inline-values delimiter length]} header-info]
     (if-not inline-values
       []
       (let [tokens (parser/delimited-values inline-values delimiter)
             values (mapv #(parser/primitive-token % strict) tokens)]
         (when (and strict (not= (count values) length))
           (throw (ex-info (str "Array length mismatch: expected " length ", got " (count values))
                           {:type :array-length-mismatch
                            :expected length
                            :actual (count values)})))
         values)))))


;; ============================================================================
;; Tabular Array Decoding
;; ============================================================================

(defn- parse-tabular-row
  "Parses a single tabular row into values.

  Parameters:
    - row-content: String content of row
    - delimiter: Delimiter character
    - strict: Enable strict mode

  Returns:
    Vector of parsed primitive values"
  [row-content delimiter strict]
  (let [tokens (parser/delimited-values row-content delimiter)]
    (mapv #(parser/primitive-token % strict) tokens)))


(defn- row-or-key-value?
  "Determines if a line is a data row or key-value line.

  Uses character position analysis with optional look-ahead for disambiguation:
  - No colon → data row
  - No delimiter → key-value line
  - Delimiter before colon → data row
  - Colon before delimiter → check look-ahead if provided

  Look-ahead strategy (when cursor and depth provided):
  If next line at same depth also has delimiter-before-colon pattern → data row
  Otherwise → key-value line

  Parameters:
    - content: Line content string
    - delimiter: Delimiter character
    - cursor: Optional LineCursor for look-ahead
    - depth: Optional depth for look-ahead

  Returns:
    :row or :key-value"
  ([content delimiter]
   (row-or-key-value? content delimiter nil nil))
  ([content delimiter cursor depth]
   (let [colon-pos (str-utils/unquoted-char content \:)
         delim-char (first delimiter)
         delim-pos (str-utils/unquoted-char content delim-char)]
     (cond
       ;; No colon: must be row
       (nil? colon-pos)
       :row

       ;; No delimiter: must be key-value
       (nil? delim-pos)
       :key-value

       ;; Delimiter before colon: data row
       (< delim-pos colon-pos)
       :row

       ;; Colon before delimiter: ambiguous case, use look-ahead if available
       (and cursor depth)
       (let [next-line (scanner/peek-at-depth cursor depth)]
         (if next-line
           (let [next-colon-pos (str-utils/unquoted-char (:content next-line) \:)
                 next-delim-pos (str-utils/unquoted-char (:content next-line) delim-char)]
             ;; If next line also has delimiter before colon, current line is likely a row
             (if (and next-delim-pos next-colon-pos (< next-delim-pos next-colon-pos))
               :row
               :key-value))
           ;; No next line, default to key-value
           :key-value))

       ;; No look-ahead context: default to key-value
       :else
       :key-value))))


(defn tabular-array
  "Decodes a tabular array into objects.

  Format:
    [2]{id,name}:
      1,Alice
      2,Bob

  Parameters:
    - header-info: Map from parse-array-header-line
    - cursor: LineCursor positioned after header
    - depth: Expected depth for rows
    - strict: Enable strict mode validation

  Returns:
    [decoded-objects, new-cursor]

  Strict mode validates:
    - Row count matches header length
    - No extra rows after expected count"
  ([header-info cursor depth]
   (tabular-array header-info cursor depth true))
  ([header-info cursor depth strict]
   (let [{:keys [fields delimiter length]} header-info]
     (loop [remaining-cursor cursor
            objects []
            row-count 0]
       (let [line (scanner/peek-at-depth remaining-cursor depth)]
         (if-not line
           ;; No more lines at depth
           (do
             (when (and strict (not= row-count length))
               (throw (ex-info (str "Tabular array length mismatch: expected " length " rows, got " row-count)
                               {:type :tabular-array-length-mismatch
                                :expected length
                                :actual row-count})))
             [objects remaining-cursor])
           ;; Check if this is a data row or key-value line with look-ahead
           (let [next-cursor (scanner/advance-cursor remaining-cursor)
                 line-type (row-or-key-value? (:content line) delimiter next-cursor depth)]
             (if (= line-type :key-value)
               ;; End of rows (key-value line follows)
               (do
                 (when (and strict (not= row-count length))
                   (ex/info! (str "Tabular array length mismatch: expected " length " rows, got " row-count)
                                   {:type :tabular-array-length-mismatch
                                    :expected length
                                    :actual row-count}))
                 [objects remaining-cursor])
               ;; Parse data row
               (let [values (parse-tabular-row (:content line) delimiter strict)
                     obj (zipmap fields values)
                     new-cursor (scanner/advance-cursor remaining-cursor)]
                 (recur new-cursor
                        (conj objects obj)
                        (inc row-count)))))))))))


;; ============================================================================
;; List Array Decoding
;; ============================================================================

(defn list-array
  "Decodes a list-format array.

  Format:
    [3]:
      - item1
      - item2
      - item3

  Parameters:
    - header-info: Map from parse-array-header-line
    - cursor: LineCursor positioned after header
    - depth: Expected depth for items
    - strict: Enable strict mode validation
    - list-item-fn: Function to decode list items (for dependency injection)

  Returns:
    [decoded-items, new-cursor]

  Strict mode validates:
    - Item count matches header length
    - No extra items after expected count"
  ([header-info cursor depth strict list-item-fn]
   (let [{:keys [length delimiter]} header-info]
     (loop [remaining-cursor cursor
            items []
            item-count 0]
       (let [line (scanner/peek-at-depth remaining-cursor depth)]
         (if-not line
           ;; No more lines at depth
           (do
             (when (and strict (not= item-count length))
               (throw (ex-info (str "List array length mismatch: expected " length " items, got " item-count)
                               {:type :list-array-length-mismatch
                                :expected length
                                :actual item-count})))
             [items remaining-cursor])
           ;; Check if line starts with list marker
           (if-not (str/starts-with? (:content line) const/list-item-prefix)
             ;; No list marker: end of list
             (do
               (when (and strict (not= item-count length))
                 (ex/info! (str "List array length mismatch: expected " length " items, got " item-count)
                                 {:type :list-array-length-mismatch
                                  :expected length
                                  :actual item-count}))
               [items remaining-cursor])
             ;; Decode list item using provided function
             (let [[item new-cursor] (list-item-fn line remaining-cursor depth delimiter strict)]
               (recur new-cursor
                      (conj items item)
                      (inc item-count))))))))))
