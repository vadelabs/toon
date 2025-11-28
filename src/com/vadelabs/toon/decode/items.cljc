(ns com.vadelabs.toon.decode.items
  "List item type detection and decoding for TOON format."
  (:require
    [clojure.string :as str]
    [com.vadelabs.toon.constants :as const]
    [com.vadelabs.toon.decode.arrays :as arrays]
    [com.vadelabs.toon.decode.objects :as objects]
    [com.vadelabs.toon.decode.parser :as parser]
    [com.vadelabs.toon.decode.scanner :as scanner]
    [com.vadelabs.toon.utils :as str-utils]))


;; ============================================================================
;; List Item Type Detection
;; ============================================================================

(defn- list-item-type
  "Type of list item: :array, :object, or :primitive."
  [content]
  (cond
    (and (str/includes? content "[")
         (str/includes? content "]"))
    :array

    (str-utils/unquoted-char content \:)
    :object

    :else
    :primitive))


;; ============================================================================
;; List Item Decoding
;; ============================================================================

(defn list-item
  "Decodes a single list item, detecting its type.

  Parameters:
    - line: ParsedLine with list marker
    - cursor: LineCursor positioned at this line
    - depth: Current depth
    - delimiter: Delimiter for parsing
    - strict: Enable strict mode

  Returns:
    [decoded-value, new-cursor]"
  [line cursor depth delimiter strict]
  (let [content (:content line)
        ;; Remove list marker prefix ("- ")
        after-marker (subs content (count const/list-item-prefix))
        item-type (list-item-type after-marker)]
    (case item-type
      :array
      ;; Inline array: "- [3]: a,b,c"
      (let [header-info (parser/array-header-line after-marker)
            items (arrays/inline-primitive-array header-info strict)
            new-cursor (scanner/advance-cursor cursor)]
        [items new-cursor])

      :object
      ;; Object with first field on hyphen line
      (objects/object-from-list-item line cursor depth delimiter strict list-item)

      :primitive
      ;; Simple primitive value
      (let [value (parser/primitive-token after-marker strict)
            new-cursor (scanner/advance-cursor cursor)]
        [value new-cursor]))))
