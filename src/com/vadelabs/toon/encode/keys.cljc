(ns com.vadelabs.toon.encode.keys
  "Key manipulation utilities for TOON encoding.

  Provides functions to collapse nested single-key objects into dotted paths."
  (:require
    [clojure.string :as str]
    [com.vadelabs.toon.constants :as const]
    [com.vadelabs.toon.utils :as utils]))


;; ============================================================================
;; Key Collapsing Helpers
;; ============================================================================

(defn- chain
  "Returns chain information for a single-key object traversal.

  Traverses nested objects, collecting keys until:
  - A non-single-key object is found
  - An array is encountered
  - A primitive is reached
  - An empty object is reached
  - The depth limit is reached

  Parameters:
    - start-key: The initial key to start the chain
    - start-value: The value to traverse
    - max-depth: Maximum number of segments to collect

  Returns:
    Map with :segments (vector of keys), :tail (remainder value), :leaf-value (final value)"
  [start-key start-value max-depth]
  (loop [segments [start-key]
         current-value start-value]
    (let [depth-reached? (>= (count segments) max-depth)
          is-object? (map? current-value)
          ks (when is-object? (keys current-value))
          single-key? (and is-object? (= (count ks) 1))]

      (if (or depth-reached? (not is-object?) (not single-key?))
        ;; Terminal conditions - stop traversal
        {:segments segments
         :tail (when (and is-object? (seq ks)) current-value)
         :leaf-value current-value}
        ;; Continue traversal for single-key object
        (let [[next-key] ks
              next-value (current-value next-key)]
          (recur (conj segments next-key) next-value))))))


(defn- dotted-key
  "Returns a dotted key string from segments.

  Parameters:
    - segments: Vector of key segments

  Returns:
    Dotted key string (e.g., \"data.config.server\")"
  [segments]
  (str/join const/dot segments))


(defn collapse
  "Collapses a key-value pair into a dotted path, or returns nil if collapsing not possible.

  Collapsing traverses nested objects with single keys, merging them into a dotted path.
  It stops when:
  - A non-single-key object is encountered
  - An array is encountered
  - A primitive value is reached
  - The flatten depth limit is reached
  - Any segment fails safe mode validation

  Safe mode requirements:
  - key-collapsing must be :safe
  - Every segment must be a valid identifier (no dots, no special chars)
  - The collapsed key must not collide with existing sibling keys
  - The collapsed key must not collide with root-level literal dotted keys

  Parameters:
    - key: The starting key to collapse
    - value: The value associated with the key
    - siblings: Vector of all sibling keys at this level (for collision detection)
    - options: Map with :key-collapsing and :flatten-depth
    - root-literal-keys: Optional set of dotted keys that exist at root level
    - path-prefix: Optional string prefix for building absolute path

  Returns:
    Map with :collapsed-key, :remainder, :leaf-value, :segment-count if collapsing is possible,
    nil otherwise"
  ([key value siblings options]
   (collapse key value siblings options nil nil))
  ([key value siblings options root-literal-keys path-prefix]
   (when (and (= (:key-collapsing options) :safe)
              (map? value))
     (let [effective-flatten-depth (:flatten-depth options ##Inf)
           {:keys [segments tail leaf-value]} (chain key value effective-flatten-depth)]

       (when (and (>= (count segments) 2)
                  (every? utils/identifier-segment? segments))
         (let [collapsed-key (dotted-key segments)
               absolute-path (if path-prefix
                               (str path-prefix const/dot collapsed-key)
                               collapsed-key)
               sibling-collision? (some #{collapsed-key} siblings)
               root-collision? (and root-literal-keys
                                   (contains? root-literal-keys absolute-path))]

           (when-not (or sibling-collision? root-collision?)
             {:collapsed-key collapsed-key
              :remainder tail
              :leaf-value leaf-value
              :segment-count (count segments)})))))))
