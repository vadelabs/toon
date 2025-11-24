(ns com.vadelabs.toon.decode.objects
  "Object (map) decoding for TOON format."
  (:require
    [clojure.string :as str]
    [com.vadelabs.toon.constants :as const]
    [com.vadelabs.toon.decode.arrays :as arrays]
    [com.vadelabs.toon.decode.parser :as parser]
    [com.vadelabs.toon.decode.scanner :as scanner]
    [com.vadelabs.toon.utils :as str-utils]))


;; Forward declaration for mutual recursion with items namespace
(declare object)


;; ============================================================================
;; Helper Functions for Object Decoding
;; ============================================================================

(defn- decode-nested-array
  "Decodes a nested array (tabular or list format).

  Parameters:
    - content: Line content with array header
    - cursor: Current cursor
    - depth: Current depth
    - strict: Strict mode flag
    - list-item-fn: List item decoder function

  Returns:
    [array-key, decoded-array, new-cursor]"
  [content cursor depth strict list-item-fn]
  (let [header-info (parser/array-header-line content)
        array-key (:key header-info)
        cursor-after-header (scanner/advance-cursor cursor)
        nested-depth (inc depth)
        [decoded-array final-cursor] (cond
                                       ;; Tabular array
                                       (:fields header-info)
                                       (arrays/tabular-array header-info cursor-after-header nested-depth strict)

                                       ;; List array
                                       :else
                                       (arrays/list-array header-info cursor-after-header nested-depth strict list-item-fn))]
    [array-key decoded-array final-cursor]))


(defn- decode-nested-object-or-nil
  "Decodes a nested object or returns nil if no nested content.

  Parameters:
    - k: The key for this value
    - cursor: Current cursor
    - depth: Current depth
    - delimiter: Delimiter character
    - strict: Strict mode flag
    - list-item-fn: List item decoder function

  Returns:
    [key, value, new-cursor]"
  [k cursor depth delimiter strict list-item-fn]
  (let [cursor-after-key (scanner/advance-cursor cursor)
        next-line (scanner/peek-cursor cursor-after-key)]
    (if (and next-line (> (:depth next-line) depth))
      ;; Has nested content: decode as nested object
      (let [nested-depth (inc depth)
            [nested-obj final-cursor] (object cursor-after-key nested-depth delimiter strict list-item-fn)]
        [k nested-obj final-cursor])
      ;; No nested content: empty value
      [k nil cursor-after-key])))


(defn- decode-inline-array
  "Decodes an inline primitive array.

  Parameters:
    - content: Line content with inline array
    - cursor: Current cursor
    - strict: Strict mode flag

  Returns:
    [array-key, decoded-array, new-cursor]"
  [content cursor strict]
  (let [header-info (parser/array-header-line content)
        array-key (:key header-info)
        decoded-array (arrays/inline-primitive-array header-info strict)
        new-cursor (scanner/advance-cursor cursor)]
    [array-key decoded-array new-cursor]))


(defn- decode-inline-primitive
  "Decodes an inline primitive value.

  Parameters:
    - k: The key for this value
    - value-part: The value string
    - cursor: Current cursor
    - strict: Strict mode flag

  Returns:
    [key, value, new-cursor]"
  [k value-part cursor strict]
  (let [value (parser/primitive-token value-part strict)
        new-cursor (scanner/advance-cursor cursor)]
    [k value new-cursor]))


;; ============================================================================
;; Object Decoding
;; ============================================================================

(defn object
  "Decodes an object from key-value lines at the given depth.

  Parameters:
    - cursor: LineCursor
    - depth: Expected depth for key-value lines
    - delimiter: Delimiter for parsing values
    - strict: Enable strict mode
    - list-item-fn: Function to decode list items (for dependency injection)

  Returns:
    [decoded-object, new-cursor]"
  ([cursor depth delimiter strict list-item-fn]
   (loop [remaining-cursor cursor
          obj {}]
     (let [line (scanner/peek-at-depth remaining-cursor depth)]
       (if-not line
         ;; No more lines at this depth
         [obj remaining-cursor]
         (let [content (:content line)
               colon-pos (str-utils/unquoted-char content \:)]
           (if-not colon-pos
             ;; No colon: not a key-value line, end of object
             [obj remaining-cursor]
             (let [key-part (subs content 0 colon-pos)
                   value-part (str/trim (subs content (inc colon-pos)))
                   k (:key (parser/key-token key-part))
                   has-array-header? (str/includes? key-part "[")
                   has-inline-value? (not (empty? value-part))]
               (cond
                 ;; Nested array (no inline value, has array header)
                 (and (not has-inline-value?) has-array-header?)
                 (let [[array-key decoded-array final-cursor]
                       (decode-nested-array content remaining-cursor depth strict list-item-fn)]
                   (recur final-cursor
                          (assoc obj array-key decoded-array)))

                 ;; Nested object or nil (no inline value, no array header)
                 (not has-inline-value?)
                 (let [[key value final-cursor]
                       (decode-nested-object-or-nil k remaining-cursor depth delimiter strict list-item-fn)]
                   (recur final-cursor
                          (assoc obj key value)))

                 ;; Inline array (has inline value, has array header)
                 has-array-header?
                 (let [[array-key decoded-array new-cursor]
                       (decode-inline-array content remaining-cursor strict)]
                   (recur new-cursor
                          (assoc obj array-key decoded-array)))

                 ;; Inline primitive (has inline value, no array header)
                 :else
                 (let [[key value new-cursor]
                       (decode-inline-primitive k value-part remaining-cursor strict)]
                   (recur new-cursor
                          (assoc obj key value))))))))))))


;; ============================================================================
;; Object as List Item Decoding
;; ============================================================================

(defn object-from-list-item
  "Decodes an object that starts on a list item line.

  Format:
    - first-key: first-value
      second-key: second-value

  The first key-value is on the hyphen line.
  Remaining key-values are at depth+1.

  Parameters:
    - line: ParsedLine with list marker
    - cursor: LineCursor positioned at this line
    - depth: Current list depth
    - delimiter: Delimiter for parsing
    - strict: Enable strict mode
    - list-item-fn: Function to decode list items (for dependency injection)

  Returns:
    [decoded-object, new-cursor]"
  [line cursor depth delimiter strict list-item-fn]
  (let [content (:content line)
        ;; Remove list marker prefix
        after-marker (subs content (count const/list-item-prefix))
        ;; Find colon to split key:value
        colon-pos (str-utils/unquoted-char after-marker \:)]
    (when-not colon-pos
      (throw (ex-info "Object in list must have key:value format"
                      {:type :invalid-object-list-item
                       :line (:line-number line)
                       :content content
                       :suggestion "Add a colon between key and value: - key: value"
                       :examples ["- name: Alice" "- id: 123" "- active: true"]})))
    (let [key-part (subs after-marker 0 colon-pos)
          value-part (str/trim (subs after-marker (inc colon-pos)))
          first-key (:key (parser/key-token key-part))
          first-value (if (empty? value-part)
                        nil
                        (parser/primitive-token value-part strict))
          ;; Advance past the hyphen line
          cursor-after-first (scanner/advance-cursor cursor)
          ;; Decode remaining key-values at depth+1
          [rest-obj remaining-cursor] (object cursor-after-first (inc depth) delimiter strict list-item-fn)]
      ;; Merge first key-value with rest
      [(assoc rest-obj first-key first-value) remaining-cursor])))
