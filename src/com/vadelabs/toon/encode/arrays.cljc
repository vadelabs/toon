(ns com.vadelabs.toon.encode.arrays
  "Array encoding for TOON format.

  Handles encoding of vectors with support for:
  - Inline primitive arrays (key[N]: val1,val2,val3)
  - Tabular arrays of objects (key[N]{col1,col2}: ...)
  - Nested arrays of arrays"
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [com.vadelabs.toon.constants :as const]
   [com.vadelabs.toon.encode.normalize :as norm]
   [com.vadelabs.toon.encode.primitives :as prim]
   [com.vadelabs.toon.encode.writer :as writer]
   [com.vadelabs.toon.utils :as quote]))

;; Forward declarations for mutual references
(declare mixed-items)

;; ============================================================================
;; Array Header Utilities
;; ============================================================================

(defn array-header
  "Generates an array header with length information.

  When delimiter is NOT comma, includes the delimiter in the header.

  Parameters:
    - length: Number of elements in the array
    - delimiter: Delimiter character being used

  Returns:
    String like \"[3]\" or \"[3|]\" or \"[3	]\"

  Examples:
    (array-header 3 \",\")  ;=> \"[3]\"
    (array-header 3 \"|\")  ;=> \"[3|]\"
    (array-header 3 \"\\t\") ;=> \"[3\\t]\""
  [length delimiter]
  (let [delimiter-part (if (not= delimiter ",") delimiter "")]
    (str const/open-bracket length delimiter-part const/close-bracket)))

(defn extract-common-keys
  "Extracts common keys from an array of objects.

  Returns keys that appear in all objects, in the order they appear
  in the first object.

  Uses set intersection for O(n×k) performance instead of O(n×k²).

  Parameters:
    - objects: Vector of maps

  Returns:
    Vector of common keys (as strings)

  Examples:
    (extract-common-keys [{\"a\" 1 \"b\" 2} {\"a\" 3 \"b\" 4}])
    ;=> [\"a\" \"b\"]"
  [objects]
  (when (seq objects)
    (let [first-obj (first objects)
          first-keys (keys first-obj)
          ;; Compute intersection of all key sets
          common-set (reduce (fn [acc obj]
                               (set/intersection acc (set (keys obj))))
                             (set first-keys)
                             (rest objects))]
      ;; Preserve first object's key order
      (filterv common-set first-keys))))

(defn- encode-delimited-values
  "Encodes a collection of values as a delimited string.

  Parameters:
    - values: Collection of primitive values to encode
    - delimiter: Delimiter character (\",\", \"|\", or \"\\t\")

  Returns:
    String with encoded values joined by delimiter

  Examples:
    (encode-delimited-values [1 2 3] \",\")
    ;=> \"1,2,3\"

    (encode-delimited-values [\"a\" \"b\" \"c\"] \"|\")
    ;=> \"a|b|c\""
  [values delimiter]
  (str/join delimiter (map #(prim/encode % delimiter) values)))

;; ============================================================================
;; Inline Array Encoding
;; ============================================================================

(defn inline
  "Encodes an array of primitives as an inline comma-separated list.

  Format (depth > 0): val1,val2,val3
  Format (depth = 0): [N]: val1,val2,val3

  Parameters:
    - values: Vector of primitive values
    - delimiter: Delimiter character
    - depth: Current indentation depth
    - writer: LineWriter instance

  Returns:
    Updated LineWriter with inline array content.

  Examples:
    [1,2,3] at depth > 0 → \"1,2,3\"
    [1,2,3] at depth = 0 → \"[3]: 1,2,3\"
    [\"a\",\"b\",\"c\"] → \"a,b,c\""
  [values delimiter depth writer]
  (let [values-str (encode-delimited-values values delimiter)
        ;; For root-level arrays (depth 0), include array header
        line (if (zero? depth)
               (str (array-header (count values) delimiter) const/colon const/space values-str)
               values-str)]
    (writer/push writer depth line)))

;; ============================================================================
;; Tabular Array Encoding (Arrays of Objects)
;; ============================================================================

(defn tabular-header
  "Encodes the header for a tabular array.

  Format (depth > 0): [N]{col1,col2,...}:
  Format (depth = 0): [N]{col1,col2,...}:

  Parameters:
    - count: Number of objects in array
    - keys: Vector of column names
    - delimiter: Delimiter character
    - depth: Current indentation depth
    - writer: LineWriter instance

  Returns:
    Updated LineWriter."
  [cnt ks delimiter depth writer]
  (let [header-suffix (array-header cnt delimiter)
        keys-part (str const/open-brace
                       (str/join delimiter (map quote/maybe-quote-key ks))
                       const/close-brace
                       const/colon)]
    (writer/push writer depth (str header-suffix keys-part))))

(defn tabular-row
  "Encodes a single row in a tabular array.

  Extracts values for the specified keys and encodes them.

  Parameters:
    - obj: Map representing one object
    - keys: Vector of keys to extract
    - delimiter: Delimiter character
    - depth: Current indentation depth
    - writer: LineWriter instance

  Returns:
    Updated LineWriter."
  [obj ks delimiter depth writer]
  (let [values (map #(get obj %) ks)
        line (encode-delimited-values values delimiter)]
    (writer/push writer depth line)))

(defn tabular
  "Encodes an array of objects in tabular format.

  Format:
  [N]{col1,col2}:
    val1,val2
    val3,val4

  Parameters:
    - objects: Vector of maps with common keys
    - delimiter: Delimiter character
    - depth: Current indentation depth
    - writer: LineWriter instance

  Returns:
    Updated LineWriter."
  [objects delimiter depth writer]
  (let [common-keys (extract-common-keys objects)]
    (if (empty? common-keys)
      ;; No common keys - caller should fall back to mixed array encoding
      writer
      (let [w (tabular-header (count objects) common-keys delimiter depth writer)]
        (reduce (fn [w obj]
                  (tabular-row obj common-keys delimiter (inc depth) w))
                w
                objects)))))

;; ============================================================================
;; Object as List Item Encoding
;; ============================================================================

(defn- tabular-array?
  "Checks if an array is tabular (uniform objects with common primitive-only keys).

  Parameters:
    - arr: Vector to check

  Returns:
    Vector of common keys if tabular, nil otherwise."
  [arr]
  (when (and (vector? arr)
             (seq arr)
             (every? map? arr))
    (let [common-keys (extract-common-keys arr)]
      (when (and (seq common-keys)
                 ;; All values for common keys must be primitives
                 (every? (fn [obj]
                           (every? (fn [k]
                                     (norm/primitive? (get obj k)))
                                   common-keys))
                         arr))
        common-keys))))

(defn- encode-first-field-tabular
  "Encodes list-item object when first field is a tabular array (v3.0 spec).

  Format:
    - key[N]{fields}:
        row1
        row2
      other-key: value

  Parameters:
    - first-key: Key of the first field
    - first-value: Tabular array value
    - common-keys: Vector of common keys in the tabular array
    - rest-keys: Remaining keys to encode
    - obj: Full object
    - delimiter: Delimiter character
    - depth: Current indentation depth
    - writer: LineWriter instance

  Returns:
    Updated LineWriter."
  [first-key first-value common-keys rest-keys obj delimiter depth writer]
  (let [;; Build header: - key[N]{fields}:
        quoted-key (quote/maybe-quote-key first-key)
        header-suffix (array-header (count first-value) delimiter)
        fields-part (str const/open-brace
                         (str/join delimiter (map quote/maybe-quote-key common-keys))
                         const/close-brace
                         const/colon)
        header-line (str const/list-item-prefix quoted-key header-suffix fields-part)
        w (writer/push writer depth header-line)
        ;; Write tabular rows at depth+2
        w (reduce (fn [w row]
                    (tabular-row row common-keys delimiter (+ depth 2) w))
                  w
                  first-value)]
    ;; Write remaining fields at depth+1
    (reduce (fn [w k]
              (let [v (get obj k)
                    line (str (quote/maybe-quote-key k) const/colon const/space (prim/encode v delimiter))]
                (writer/push w (inc depth) line)))
            w
            rest-keys)))

(defn- encode-first-field-primitive
  "Encodes list-item object when first field is a primitive.

  Format:
    - key: value
      other-key: other-value

  Parameters:
    - first-key: Key of the first field
    - first-value: Primitive value
    - rest-keys: Remaining keys to encode
    - obj: Full object
    - delimiter: Delimiter character
    - depth: Current indentation depth
    - writer: LineWriter instance

  Returns:
    Updated LineWriter."
  [first-key first-value rest-keys obj delimiter depth writer]
  (let [quoted-key (quote/maybe-quote-key first-key)
        first-line (str const/list-item-prefix quoted-key const/colon const/space (prim/encode first-value delimiter))
        w (writer/push writer depth first-line)]
    ;; Encode remaining keys at depth+1
    (reduce (fn [w k]
              (let [v (get obj k)
                    line (str (quote/maybe-quote-key k) const/colon const/space (prim/encode v delimiter))]
                (writer/push w (inc depth) line)))
            w
            rest-keys)))

(defn- encode-first-field-empty-array
  "Encodes list-item object when first field is an empty array.

  Format:
    - key[0]:
      other-key: value

  Parameters:
    - first-key: Key of the first field
    - rest-keys: Remaining keys to encode
    - obj: Full object
    - delimiter: Delimiter character
    - depth: Current indentation depth
    - writer: LineWriter instance

  Returns:
    Updated LineWriter."
  [first-key rest-keys obj delimiter depth writer]
  (let [quoted-key (quote/maybe-quote-key first-key)
        header (str const/list-item-prefix quoted-key (array-header 0 delimiter))
        w (writer/push writer depth header)]
    ;; Encode remaining keys at depth+1
    (reduce (fn [w k]
              (let [v (get obj k)
                    line (str (quote/maybe-quote-key k) const/colon const/space (prim/encode v delimiter))]
                (writer/push w (inc depth) line)))
            w
            rest-keys)))

(defn- encode-first-field-inline-array
  "Encodes list-item object when first field is an inline primitive array.

  Format:
    - key[N]: val1,val2,val3
      other-key: value

  Parameters:
    - first-key: Key of the first field
    - first-value: Array of primitives
    - rest-keys: Remaining keys to encode
    - obj: Full object
    - delimiter: Delimiter character
    - depth: Current indentation depth
    - writer: LineWriter instance

  Returns:
    Updated LineWriter."
  [first-key first-value rest-keys obj delimiter depth writer]
  (let [quoted-key (quote/maybe-quote-key first-key)
        header (str const/list-item-prefix quoted-key (array-header (count first-value) delimiter) const/colon const/space)
        values-str (str/join delimiter (map #(prim/encode % delimiter) first-value))
        first-line (str header values-str)
        w (writer/push writer depth first-line)]
    ;; Encode remaining keys at depth+1
    (reduce (fn [w k]
              (let [v (get obj k)
                    line (str (quote/maybe-quote-key k) const/colon const/space (prim/encode v delimiter))]
                (writer/push w (inc depth) line)))
            w
            rest-keys)))

(defn- encode-first-field-complex-array
  "Encodes list-item object when first field is a non-inline array.

  Format:
    - key[N]:
        - item1
        - item2
      other-key: value

  Parameters:
    - first-key: Key of the first field
    - first-value: Complex array
    - rest-keys: Remaining keys to encode
    - obj: Full object
    - delimiter: Delimiter character
    - depth: Current indentation depth
    - writer: LineWriter instance

  Returns:
    Updated LineWriter."
  [first-key first-value rest-keys obj delimiter depth writer]
  (let [quoted-key (quote/maybe-quote-key first-key)
        header (str const/list-item-prefix quoted-key (array-header (count first-value) delimiter) const/colon)
        w (writer/push writer depth header)
        ;; Items at depth+2
        w (mixed-items first-value delimiter (+ depth 2) w)]
    ;; Encode remaining keys at depth+1
    (reduce (fn [w k]
              (let [v (get obj k)
                    line (str (quote/maybe-quote-key k) const/colon const/space (prim/encode v delimiter))]
                (writer/push w (inc depth) line)))
            w
            rest-keys)))

(defn- encode-first-field-object
  "Encodes list-item object when first field is a nested object.

  Format:
    - key:
        nested-key: nested-value
      other-key: value

  Parameters:
    - first-key: Key of the first field
    - first-value: Nested object
    - rest-keys: Remaining keys to encode
    - obj: Full object
    - delimiter: Delimiter character
    - depth: Current indentation depth
    - writer: LineWriter instance
    - encode-object-fn: Function to encode nested objects

  Returns:
    Updated LineWriter."
  [first-key first-value rest-keys obj delimiter depth writer encode-object-fn]
  (let [quoted-key (quote/maybe-quote-key first-key)
        header (str const/list-item-prefix quoted-key const/colon)
        w (writer/push writer depth header)
        ;; Nested object at depth+2
        w (if (empty? first-value)
            w
            (encode-object-fn first-value delimiter (+ depth 2) w))]
    ;; Encode remaining keys at depth+1
    (reduce (fn [w k]
              (let [v (get obj k)
                    line (str (quote/maybe-quote-key k) const/colon const/space (prim/encode v delimiter))]
                (writer/push w (inc depth) line)))
            w
            rest-keys)))

(defn object-as-list-item
  "Encodes an object as a list item (v3.0 spec compliant).

  For objects where first field is a tabular array:
    - key[N]{fields}:
        row1
        row2
      other-key: value

  For objects with primitive first field:
    - key: value
      other-key: other-value

  For empty objects:
    -

  Parameters:
    - obj: Map to encode
    - delimiter: Delimiter character
    - depth: Current indentation depth
    - writer: LineWriter instance
    - encode-object-fn: (optional) Function to encode nested objects

  Returns:
    Updated LineWriter."
  ([obj delimiter depth writer]
   (object-as-list-item obj delimiter depth writer nil))
  ([obj delimiter depth writer encode-object-fn]
   (let [ks (vec (keys obj))]
     (if (empty? ks)
       ;; Empty object: just "-"
       (writer/push writer depth const/list-item-marker)
       (let [first-key (first ks)
             first-value (get obj first-key)
             rest-keys (rest ks)
             ;; Check for tabular array first (v3.0 spec)
             tabular-keys (tabular-array? first-value)]
         (cond
           ;; First field is a tabular array (v3.0 spec)
           tabular-keys
           (encode-first-field-tabular first-key first-value tabular-keys rest-keys obj delimiter depth writer)

           ;; First field is primitive
           (norm/primitive? first-value)
           (encode-first-field-primitive first-key first-value rest-keys obj delimiter depth writer)

           ;; First field is empty array
           (and (vector? first-value) (empty? first-value))
           (encode-first-field-empty-array first-key rest-keys obj delimiter depth writer)

           ;; First field is inline primitive array
           (norm/array-of-primitives? first-value)
           (encode-first-field-inline-array first-key first-value rest-keys obj delimiter depth writer)

           ;; First field is complex array (non-tabular or mixed)
           (vector? first-value)
           (encode-first-field-complex-array first-key first-value rest-keys obj delimiter depth writer)

           ;; First field is nested object
           (map? first-value)
           (if encode-object-fn
             (encode-first-field-object first-key first-value rest-keys obj delimiter depth writer encode-object-fn)
             ;; Fallback for nested objects without encode-object-fn
             (let [quoted-key (quote/maybe-quote-key first-key)
                   header (str const/list-item-prefix quoted-key const/colon)
                   w (writer/push writer depth header)]
               (reduce (fn [w k]
                         (let [v (get obj k)
                               line (str (quote/maybe-quote-key k) const/colon const/space (prim/encode v delimiter))]
                           (writer/push w (inc depth) line)))
                       w
                       rest-keys)))

           ;; Default fallback
           :else
           (writer/push writer depth const/list-item-marker)))))))

;; ============================================================================
;; Mixed Array Encoding (with list markers)
;; ============================================================================

(defn mixed-items
  "Encodes mixed array items without a header.

  Format (items only, no header):
    - value1
    - value2
    - [N]: nested array

  Parameters:
    - array: Vector with mixed types
    - delimiter: Delimiter character
    - depth: Current indentation depth
    - writer: LineWriter instance

  Returns:
    Updated LineWriter."
  [arr delimiter depth writer]
  (reduce (fn [w v]
            (cond
              ;; Primitive: - value
              (norm/primitive? v)
              (writer/push w depth (str const/list-item-prefix (prim/encode v delimiter)))

              ;; Array of primitives: - [N]: val1,val2,val3
              (norm/array-of-primitives? v)
              (let [inline-header (str const/list-item-prefix (array-header (count v) delimiter) const/colon const/space)
                    values-str (encode-delimited-values v delimiter)
                    line (str inline-header values-str)]
                (writer/push w depth line))

              ;; Object: encode as list item
              (map? v)
              (object-as-list-item v delimiter depth w)

              ;; Nested arrays and other complex types fall through to default
              :else
              w))
          writer
          arr))

(defn mixed
  "Encodes a mixed array with header using list item markers.

  Format:
  [N]:
    - value1
    - value2

  Parameters:
    - array: Vector with mixed types
    - delimiter: Delimiter character
    - depth: Current indentation depth
    - writer: LineWriter instance

  Returns:
    Updated LineWriter."
  [arr delimiter depth writer]
  (let [header (str (array-header (count arr) delimiter) const/colon)
        w (writer/push writer depth header)]
    (mixed-items arr delimiter (inc depth) w)))

;; ============================================================================
;; Array of Arrays Encoding
;; ============================================================================

(defn of-arrays-items
  "Encodes array of arrays items without a header.

  Format (items only, no header):
    - [N]: val1,val2
    - [N]: val3,val4

  Parameters:
    - arrays: Vector of vectors
    - delimiter: Delimiter character
    - depth: Current indentation depth
    - writer: LineWriter instance

  Returns:
    Updated LineWriter."
  [arrays delimiter depth writer]
  (reduce (fn [w arr]
            (let [inline-header (str const/list-item-prefix (array-header (count arr) delimiter) const/colon const/space)
                  values-str (encode-delimited-values arr delimiter)
                  line (str inline-header values-str)]
              (writer/push w depth line)))
          writer
          arrays))

(defn of-arrays
  "Encodes a nested array (array of arrays) with header using list format.

  Format:
  [N]:
    - [N]: val1,val2
    - [N]: val3,val4

  Parameters:
    - arrays: Vector of vectors
    - delimiter: Delimiter character
    - depth: Current indentation depth
    - writer: LineWriter instance

  Returns:
    Updated LineWriter."
  [arrays delimiter depth writer]
  (let [header (str (array-header (count arrays) delimiter) const/colon)
        w (writer/push writer depth header)]
    (of-arrays-items arrays delimiter (inc depth) w)))

;; ============================================================================
;; Main Array Encoding Dispatch
;; ============================================================================

(defn encode
  "Encodes a vector to TOON format based on its contents.

  Dispatch logic:
  - Empty array: []
  - Array of primitives: inline format
  - Array of objects: tabular format
  - Array of arrays: nested format

  Parameters:
    - array: Vector to encode
    - delimiter: Delimiter character
    - depth: Current indentation depth
    - writer: LineWriter instance

  Returns:
    Updated LineWriter."
  [arr delimiter depth writer]
  (cond
    ;; Empty array
    (empty? arr)
    (let [empty-str (if (zero? depth) const/empty-array-with-length const/empty-array)]
      (writer/push writer depth empty-str))

    ;; Array of primitives
    (norm/array-of-primitives? arr)
    (inline arr delimiter depth writer)

    ;; Array of objects with common keys: tabular format
    (norm/array-of-objects? arr)
    (let [common-keys (extract-common-keys arr)]
      (if (empty? common-keys)
        ;; No common keys: fall back to mixed array with list markers
        (mixed arr delimiter depth writer)
        ;; Has common keys: use tabular format
        (tabular arr delimiter depth writer)))

    ;; Array of arrays
    (norm/array-of-arrays? arr)
    (of-arrays arr delimiter depth writer)

    ;; Mixed/complex array - use list markers
    :else
    (mixed arr delimiter depth writer)))
