(ns com.vadelabs.toon.encode.encoders
  "Object (map) encoding for TOON format.

  Handles encoding of maps with:
  - Simple key-value pairs
  - Nested objects with indentation
  - Arrays as values
  - Key collapsing for nested single-key objects"
  (:require
    [clojure.string :as str]
    [com.vadelabs.toon.constants :as const]
    [com.vadelabs.toon.encode.arrays :as array]
    [com.vadelabs.toon.encode.keys :as keys]
    [com.vadelabs.toon.encode.normalize :as norm]
    [com.vadelabs.toon.encode.primitives :as prim]
    [com.vadelabs.toon.encode.writer :as writer]
    [com.vadelabs.toon.utils :as quote]))


;; Forward declarations for mutual recursion
(declare value object)


;; ============================================================================
;; Key-Value Pair Encoding (Helper Functions)
;; ============================================================================

(defn- primitive-pair
  "Encodes a key-value pair where value is a primitive.

  Format: key: value

  Parameters:
    - k: String key
    - v: Primitive value
    - options: Encoding options
    - depth: Indentation depth
    - writer: LineWriter

  Returns:
    Updated LineWriter"
  [k v {:keys [delimiter]} depth writer]
  (let [quoted-key (quote/maybe-quote-key k)
        encoded-value (prim/encode v delimiter)
        line (str quoted-key const/colon const/space encoded-value)]
    (writer/push writer depth line)))


(defn- empty-array-pair
  "Encodes a key-value pair where value is an empty array.

  Format: key[0]

  Parameters:
    - k: String key
    - depth: Indentation depth
    - writer: LineWriter

  Returns:
    Updated LineWriter"
  [k depth writer]
  (let [quoted-key (quote/maybe-quote-key k)]
    (writer/push writer depth (str quoted-key const/empty-array-with-length))))


(defn- primitive-array-pair
  "Encodes a key-value pair where value is array of primitives.

  Format: key[N]: val1,val2,val3

  Parameters:
    - k: String key
    - v: Array of primitives
    - options: Encoding options
    - depth: Indentation depth
    - writer: LineWriter

  Returns:
    Updated LineWriter"
  [k v {:keys [delimiter]} depth writer]
  (let [quoted-key (quote/maybe-quote-key k)
        header (str quoted-key (array/array-header (count v) delimiter) const/colon const/space)
        encoded-values (map #(prim/encode % delimiter) v)
        values-str (str/join delimiter encoded-values)
        line (str header values-str)]
    (writer/push writer depth line)))


(defn- complex-array-pair
  "Encodes a key-value pair where value is array of objects/arrays.

  Dispatches to appropriate array format:
  - Tabular for uniform objects with common keys
  - List format for arrays of arrays or mixed content

  Parameters:
    - k: String key
    - v: Array of complex values
    - options: Encoding options
    - depth: Indentation depth
    - writer: LineWriter

  Returns:
    Updated LineWriter"
  [k v {:keys [delimiter] :as options} depth writer]
  (let [quoted-key (quote/maybe-quote-key k)]
    (cond
      ;; Uniform array of objects with common keys: tabular format
      (and (norm/array-of-objects? v)
           (seq (array/extract-common-keys v)))
      (let [header (str quoted-key (array/array-header (count v) delimiter))
            w (writer/push writer depth header)]
        (array/encode v delimiter depth w))

      ;; Array of arrays: list format
      (norm/array-of-arrays? v)
      (let [header (str quoted-key (array/array-header (count v) delimiter) const/colon)
            w (writer/push writer depth header)]
        (array/of-arrays-items v delimiter (inc depth) w))

      ;; Mixed arrays or non-uniform objects: list format
      :else
      (let [header (str quoted-key (array/array-header (count v) delimiter) const/colon)
            w (writer/push writer depth header)]
        (array/mixed-items v delimiter (inc depth) w)))))


(defn- object-pair
  "Encodes a key-value pair where value is a nested object.

  Format: key:\n  nested-content

  Parameters:
    - k: String key
    - v: Map value
    - options: Encoding options
    - depth: Indentation depth
    - writer: LineWriter

  Returns:
    Updated LineWriter"
  [k v options depth writer]
  (let [quoted-key (quote/maybe-quote-key k)
        w (writer/push writer depth (str quoted-key const/colon))]
    (object v options (inc depth) w)))


;; ============================================================================
;; Object Encoding
;; ============================================================================

(defn key-value-pair
  "Encodes a single key-value pair by dispatching to appropriate encoder.

  Supports key collapsing for nested single-key objects when enabled.

  Dispatches based on value type:
  - Primitives → primitive-pair
  - Empty arrays → empty-array-pair
  - Primitive arrays → primitive-array-pair
  - Complex arrays → complex-array-pair
  - Objects → object-pair (or collapsed key path)

  Parameters:
    - k: String key
    - v: Any value (primitive, array, or object)
    - options: Encoding options map
    - depth: Current indentation depth
    - writer: LineWriter instance
    - siblings: Optional vector of sibling keys (for collapsing collision detection)

  Returns:
    Updated LineWriter"
  ([k v options depth writer]
   (key-value-pair k v options depth writer nil))
  ([k v options depth writer siblings]
   ;; Try key collapsing if enabled and siblings provided
   (if-let [collapse-result (and siblings (keys/collapse k v siblings options))]
     ;; Collapsing succeeded - use collapsed key
     (let [{:keys [collapsed-key remainder leaf-value]} collapse-result
           quoted-key (quote/maybe-quote-key collapsed-key)]
       (cond
         ;; Case 1: Fully collapsed to a leaf value
         (nil? remainder)
         (cond
           ;; Leaf is a primitive
           (norm/primitive? leaf-value)
           (let [encoded-value (prim/encode leaf-value (:delimiter options))
                 line (str quoted-key const/colon const/space encoded-value)]
             (writer/push writer depth line))

           ;; Leaf is an array
           (vector? leaf-value)
           (cond
             (empty? leaf-value)
             (writer/push writer depth (str quoted-key const/empty-array-with-length))

             (norm/array-of-primitives? leaf-value)
             (let [header (str quoted-key (array/array-header (count leaf-value) (:delimiter options)) const/colon const/space)
                   encoded-values (map #(prim/encode % (:delimiter options)) leaf-value)
                   values-str (str/join (:delimiter options) encoded-values)
                   line (str header values-str)]
               (writer/push writer depth line))

             :else
             (let [header (str quoted-key (array/array-header (count leaf-value) (:delimiter options)) const/colon)
                   w (writer/push writer depth header)]
               (array/encode leaf-value (:delimiter options) (inc depth) w)))

           ;; Leaf is empty object
           (and (map? leaf-value) (empty? leaf-value))
           (writer/push writer depth (str quoted-key const/colon))

           :else
           writer)

         ;; Case 2: Partially collapsed with remainder
         (map? remainder)
         (let [w (writer/push writer depth (str quoted-key const/colon))]
           (object remainder options (inc depth) w))

         :else
         writer))

     ;; No collapsing - dispatch normally
     (cond
       (norm/primitive? v)
       (primitive-pair k v options depth writer)

       (and (vector? v) (empty? v))
       (empty-array-pair k depth writer)

       (norm/array-of-primitives? v)
       (primitive-array-pair k v options depth writer)

       (vector? v)
       (complex-array-pair k v options depth writer)

       (map? v)
       (object-pair k v options depth writer)

       ;; Unknown type - should not happen after normalization
       :else
       writer))))


(defn object
  "Encodes a map to TOON format.

  Collects all keys first to enable collapsing collision detection.

  Parameters:
    - obj: Map to encode
    - options: Encoding options map with :delimiter and :key-collapsing
    - depth: Current indentation depth
    - writer: LineWriter instance

  Returns:
    Updated LineWriter."
  [obj options depth writer]
  (let [all-keys (vec (keys obj))]
    (reduce-kv (fn [w k v]
                 (key-value-pair k v options depth w all-keys))
               writer
               obj)))


(defn value
  "Encodes any value to TOON format.

  Dispatch function that routes to appropriate encoder:
  - Primitives → primitive encoder
  - Arrays → array encoder
  - Objects → object encoder

  Parameters:
    - v: Any value to encode
    - options: Encoding options map
    - depth: Current indentation depth
    - writer: LineWriter instance

  Returns:
    Updated LineWriter."
  [v {:keys [delimiter] :as options} depth writer]
  (cond
    (norm/primitive? v)
    (writer/push writer depth (prim/encode v delimiter))

    (vector? v)
    (array/encode v delimiter depth writer)

    (map? v)
    (object v options depth writer)

    :else
    writer))
