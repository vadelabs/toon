(ns com.vadelabs.toon.decode.objects
  "Object (map) decoding for TOON format."
  (:require
    [clojure.string :as str]
    [com.vadelabs.toon.decode.arrays :as arrays]
    [com.vadelabs.toon.decode.parser :as parser]
    [com.vadelabs.toon.decode.scanner :as scanner]
    [com.vadelabs.toon.utils :as str-utils]))


;; Forward declaration for mutual recursion with items namespace
(declare object)


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
                   k (parser/key-token key-part)]
               (if (empty? value-part)
                 ;; No inline value: check if key contains array header
                 (if (str/includes? key-part "[")
                   ;; Array with nested items
                   (let [header-info (parser/array-header-line content)
                         array-key (:key header-info)
                         cursor-after-header (scanner/advance-cursor remaining-cursor)
                         nested-depth (inc depth)
                         [decoded-array final-cursor] (cond
                                                        ;; Tabular array
                                                        (:fields header-info)
                                                        (arrays/tabular-array header-info cursor-after-header nested-depth strict)

                                                        ;; List array
                                                        :else
                                                        (arrays/list-array header-info cursor-after-header nested-depth strict list-item-fn))]
                     (recur final-cursor
                            (assoc obj array-key decoded-array)))
                   ;; Regular key: check for nested object content
                   (let [cursor-after-key (scanner/advance-cursor remaining-cursor)
                         next-line (scanner/peek-cursor cursor-after-key)]
                     (if (and next-line (> (:depth next-line) depth))
                       ;; Has nested content: decode as nested object
                       (let [nested-depth (inc depth)
                             [nested-obj final-cursor] (object cursor-after-key nested-depth delimiter strict list-item-fn)]
                         (recur final-cursor
                                (assoc obj k nested-obj)))
                       ;; No nested content: empty value
                       (recur cursor-after-key
                              (assoc obj k nil)))))
                 ;; Has inline value: check if key contains array header
                 (if (str/includes? key-part "[")
                   ;; Inline array
                   (let [header-info (parser/array-header-line content)
                         array-key (:key header-info)
                         decoded-array (arrays/inline-primitive-array header-info strict)
                         new-cursor (scanner/advance-cursor remaining-cursor)]
                     (recur new-cursor
                            (assoc obj array-key decoded-array)))
                   ;; Regular inline primitive value
                   (let [value (parser/primitive-token value-part strict)
                         new-cursor (scanner/advance-cursor remaining-cursor)]
                     (recur new-cursor
                            (assoc obj k value)))))))))))))


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
        const-list-item-prefix "- "
        after-marker (subs content (count const-list-item-prefix))
        ;; Find colon to split key:value
        colon-pos (str-utils/unquoted-char after-marker \:)]
    (when-not colon-pos
      (throw (ex-info "Object in list must have key:value format"
                      {:type :invalid-object-list-item
                       :line (:line-number line)
                       :content content})))
    (let [key-part (subs after-marker 0 colon-pos)
            value-part (str/trim (subs after-marker (inc colon-pos)))
            first-key (parser/key-token key-part)
            first-value (if (empty? value-part)
                          nil
                          (parser/primitive-token value-part strict))
            ;; Advance past the hyphen line
            cursor-after-first (scanner/advance-cursor cursor)
            ;; Decode remaining key-values at depth+1
            [rest-obj remaining-cursor] (object cursor-after-first (inc depth) delimiter strict list-item-fn)]
        ;; Merge first key-value with rest
        [(assoc rest-obj first-key first-value) remaining-cursor])))
