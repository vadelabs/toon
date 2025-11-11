(ns com.vadelabs.toon.encode.arrays
  "Array encoding for TOON format.

  Handles encoding of vectors with support for:
  - Inline primitive arrays (key[N]: val1,val2,val3)
  - Tabular arrays of objects (key[N]{col1,col2}: ...)
  - Nested arrays of arrays"
  (:require
    [clojure.string :as str]
    [com.vadelabs.toon.constants :as const]
    [com.vadelabs.toon.encode.normalize :as norm]
    [com.vadelabs.toon.encode.primitives :as prim]
    [com.vadelabs.toon.encode.writer :as writer]
    [com.vadelabs.toon.utils :as quote]))


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

  Parameters:
    - objects: Vector of maps

  Returns:
    Vector of common keys (as strings)

  Examples:
    (extract-common-keys [{\"a\" 1 \"b\" 2} {\"a\" 3 \"b\" 4}])
    ;=> [\"a\" \"b\"]"
  [objects]
  (when (seq objects)
    (let [first-keys (keys (first objects))
          all-key-sets (map (comp set keys) objects)]
      (vec (filter (fn [k]
                     (every? #(contains? % k) all-key-sets))
                   first-keys)))))


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
  (let [encoded-values (map #(prim/encode % delimiter) values)
        values-str (str/join delimiter encoded-values)
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
        quoted-keys (map quote/maybe-quote-key ks)
        keys-part (str const/open-brace
                       (str/join delimiter quoted-keys)
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
        encoded-values (map #(prim/encode % delimiter) values)
        line (str/join delimiter encoded-values)]
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

(defn object-as-list-item
  "Encodes an object as a list item with first key-value on hyphen line.

  Format (single key):
    - key: value

  Format (multiple keys):
    - key1: value1
      key2: value2
      key3: value3

  Parameters:
    - obj: Map to encode
    - delimiter: Delimiter character
    - depth: Current indentation depth
    - writer: LineWriter instance

  Returns:
    Updated LineWriter."
  [obj delimiter depth writer]
  (let [ks (vec (keys obj))]
    (if (empty? ks)
      ;; Empty object: just "-"
      (writer/push writer depth const/list-item-marker)
      (let [first-key (first ks)
            first-value (get obj first-key)
            first-line (str const/list-item-prefix first-key const/colon const/space (prim/encode first-value delimiter))
            w (writer/push writer depth first-line)]
        ;; Encode remaining keys at depth+1 (aligned with content after "- ")
        (reduce (fn [w k]
                  (let [v (get obj k)
                        line (str k const/colon const/space (prim/encode v delimiter))]
                    (writer/push w (inc depth) line)))
                w
                (rest ks))))))


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
                    encoded-values (map #(prim/encode % delimiter) v)
                    values-str (str/join delimiter encoded-values)
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
                  encoded-values (map #(prim/encode % delimiter) arr)
                  values-str (str/join delimiter encoded-values)
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
