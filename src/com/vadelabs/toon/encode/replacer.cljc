(ns com.vadelabs.toon.encode.replacer
  "Replacer function support for TOON encoding.

  Provides transformation and filtering capabilities during encoding,
  similar to JSON.stringify's replacer parameter but with path tracking."
  (:require
    [com.vadelabs.toon.encode.normalize :as norm]))


;; Forward declarations for mutual recursion
(declare transform-children transform-map transform-vector)


(defn apply-replacer
  "Applies a replacer function to a value and all its descendants.

  The replacer is called for:
  - The root value (with key=\"\", path=[])
  - Every map property (with the property name as key)
  - Every vector element (with the string index as key: \"0\", \"1\", etc.)

  Parameters:
    - root: The normalized value to transform
    - replacer: Function (fn [key value path] ...) that returns:
                - The replacement value (will be normalized again)
                - nil to omit the property/element (root cannot be omitted)

  Returns:
    The transformed value.

  Examples:
    ;; Remove password fields
    (apply-replacer data (fn [k v _] (when-not (= k \"password\") v)))

    ;; Transform string values to uppercase
    (apply-replacer data (fn [k v _] (if (string? v) (clojure.string/upper-case v) v)))

    ;; Add timestamp to root object
    (apply-replacer data (fn [k v path]
                           (if (empty? path)
                             (assoc v :timestamp (System/currentTimeMillis))
                             v)))"
  [root replacer]
  (let [;; Call replacer on root with empty string key and empty path
        replaced-root (replacer "" root [])]
    ;; For root, nil means "no change" (don't omit the root)
    (if (nil? replaced-root)
      (transform-children root replacer [])
      ;; Normalize the replaced value and recursively transform children
      (let [normalized-root (norm/normalize-value replaced-root)]
        (transform-children normalized-root replacer [])))))


(defn- transform-children
  "Recursively transforms the children of a value using the replacer.

  Parameters:
    - value: The value whose children should be transformed
    - replacer: The replacer function to apply
    - path: Current path from root

  Returns:
    The value with transformed children."
  [value replacer path]
  (cond
    (map? value)
    (transform-map value replacer path)

    (vector? value)
    (transform-vector value replacer path)

    ;; Primitives have no children
    :else
    value))


(defn- transform-map
  "Transforms a map by applying the replacer to each property.

  Parameters:
    - m: The map to transform
    - replacer: The replacer function to apply
    - path: Current path from root

  Returns:
    A new map with transformed properties."
  [m replacer path]
  (reduce-kv
    (fn [result k v]
      (let [child-path (conj path k)
            ;; Call replacer with the property key and current path
            replaced-value (replacer k v child-path)]
        ;; nil means omit this property
        (if (nil? replaced-value)
          result
          ;; Normalize the replaced value and recursively transform children
          (let [normalized-value (norm/normalize-value replaced-value)]
            (assoc result k (transform-children normalized-value replacer child-path))))))
    {}
    m))


(defn- transform-vector
  "Transforms a vector by applying the replacer to each element.

  Parameters:
    - v: The vector to transform
    - replacer: The replacer function to apply
    - path: Current path from root

  Returns:
    A new vector with transformed elements."
  [v replacer path]
  (into []
        (keep-indexed
          (fn [idx elem]
            (let [child-path (conj path idx)
                  ;; Call replacer with string index to match JSON.stringify behavior
                  replaced-value (replacer (str idx) elem child-path)]
              ;; nil means omit this element
              (when (some? replaced-value)
                ;; Normalize the replaced value and recursively transform children
                (let [normalized-value (norm/normalize-value replaced-value)]
                  (transform-children normalized-value replacer child-path))))))
        v))
