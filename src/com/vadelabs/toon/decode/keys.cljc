(ns com.vadelabs.toon.decode.keys
  "Key manipulation utilities for TOON decoding.

  Provides functions to expand dotted keys into nested objects."
  (:require
    [clojure.string :as str]
    [com.vadelabs.toon.constants :as const]
    [com.vadelabs.toon.utils :as utils]))


;; ============================================================================
;; Path Expansion Helpers
;; ============================================================================

(def ^:private dot-pattern
  "Pre-compiled regex pattern for splitting dotted keys."
  (re-pattern (str "\\" const/dot)))


(defn- can-merge?
  "Returns true if two values can be deep merged (both are objects)."
  [a b]
  (and (map? a) (map? b)))


(defn- handle-conflict
  "Handles merge conflicts in strict/non-strict mode.

  Parameters:
    - strict: Whether to throw on conflicts
    - context: Map with conflict context (:key, :path, etc.)
    - existing-value: The existing value at the path
    - new-value: The new value being inserted
    - overwrite-fn: Function to call for non-strict overwrite (returns new value)

  Returns:
    Result of overwrite-fn in non-strict mode

  Throws:
    ex-info if strict mode is enabled"
  [strict context existing-value new-value overwrite-fn]
  (if strict
    (throw (ex-info
             (str "Path expansion conflict"
                  (when-let [k (:key context)]
                    (str " at key \"" k "\""))
                  (when-let [p (:path context)]
                    (str " at path \"" p "\""))
                  ": cannot merge "
                  (type existing-value) " with " (type new-value))
             (merge {:type :path-expansion-conflict
                     :existing-type (type existing-value)
                     :new-type (type new-value)}
                    context)))
    (overwrite-fn)))


(defn- merge-objects
  "Deep merges properties from source into target (mutates target).

  For each key in source:
  - If key doesn't exist in target: copy it
  - If both values are objects: recursively merge
  - Otherwise: conflict (strict throws, non-strict overwrites)

  Parameters:
    - target: Target map to merge into
    - source: Source map to merge from
    - strict: Whether to throw on conflicts

  Returns:
    Merged map (same as target)

  Throws:
    ex-info if a conflict occurs in strict mode"
  [target source strict]
  (reduce-kv
    (fn [acc key source-value]
      (let [target-value (get acc key)]
        (cond
          ;; Key doesn't exist in target - copy it
          (nil? target-value)
          (assoc acc key source-value)

          ;; Both are objects - recursively merge
          (can-merge? target-value source-value)
          (assoc acc key (merge-objects target-value source-value strict))

          ;; Conflict: incompatible types
          :else
          (handle-conflict strict
                           {:key key}
                           target-value
                           source-value
                           #(assoc acc key source-value)))))
    target
    source))


(defn- insert-path
  "Inserts a value at a nested path, creating intermediate objects as needed.

  Walks the segment path, creating nested objects as needed.
  When an existing value is encountered:
  - If both are objects: deep merge (continue insertion)
  - If values differ: conflict
    - strict=true: throw ex-info
    - strict=false: overwrite with new value (LWW)

  Parameters:
    - target: Map to insert into
    - segments: Vector of path segments (e.g., [\"data\" \"metadata\" \"items\"])
    - value: Value to insert at the end of the path
    - strict: Whether to throw on conflicts

  Returns:
    Updated target map

  Throws:
    ex-info if a conflict occurs in strict mode"
  [target segments value strict]
  {:pre [(map? target) (seq segments)]}
  (let [last-idx (dec (count segments))
        last-seg (get segments last-idx)]
    ;; Build the nested path
    (if (zero? last-idx)
      ;; Only one segment - insert directly
      (let [existing (get target last-seg)]
        (cond
          (nil? existing)
          (assoc target last-seg value)

          (can-merge? existing value)
          (assoc target last-seg (merge-objects existing value strict))

          :else
          (handle-conflict strict
                           {:key last-seg}
                           existing
                           value
                           #(assoc target last-seg value))))

      ;; Multiple segments - walk the path
      (let [path-to-parent (subvec segments 0 last-idx)]
        (update-in target
                   path-to-parent
                   (fn [parent]
                     (let [parent-obj (or parent {})]
                       (if-not (map? parent-obj)
                         (handle-conflict strict
                                          {:path (str/join "." path-to-parent)
                                           :found-type (type parent-obj)}
                                          parent-obj
                                          {}
                                          (fn [] {last-seg value}))
                         ;; Parent is object - insert at last segment
                         (let [existing (get parent-obj last-seg)]
                           (cond
                             (nil? existing)
                             (assoc parent-obj last-seg value)

                             (can-merge? existing value)
                             (assoc parent-obj last-seg (merge-objects existing value strict))

                             :else
                             (handle-conflict strict
                                              {:key last-seg}
                                              existing
                                              value
                                              #(assoc parent-obj last-seg value))))))))))))


;; Forward declaration for mutual recursion
(declare expand)


(defn- expandable-key?
  "Checks if a key can be expanded into a path.

  Returns true if key contains dots and all segments are valid identifiers."
  [key]
  (and (str/includes? key const/dot)
       (let [segments (str/split key dot-pattern)]
         (every? utils/identifier-segment? segments))))


(defn- expanded-key
  "Expands a single expandable key into the accumulator.

  Splits the key on dots and inserts the expanded value at that path.

  Returns updated accumulator."
  [acc key value strict]
  (let [segments (str/split key dot-pattern)]
    (insert-path acc segments value strict)))


(defn- literal-key
  "Handles a non-expandable literal key.

  Checks for conflicts and merges if possible, otherwise inserts directly.

  Returns updated accumulator."
  [acc key value strict]
  (if (contains? acc key)
    ;; Key already exists - check if we can merge
    (let [existing (get acc key)]
      (if (can-merge? existing value)
        (assoc acc key (merge-objects existing value strict))
        (handle-conflict strict
                         {:key key}
                         existing
                         value
                         #(assoc acc key value))))
    ;; No conflict - insert directly
    (assoc acc key value)))


(defn- expanded-object
  "Expands dotted keys in a single object/map.

  Iterates through all keys and expands those that are expandable,
  leaving others as literal keys.

  Returns object with expanded keys."
  [obj strict expand-paths]
  (reduce-kv
    (fn [acc key value]
      (let [expanded-value (expand value strict expand-paths)]
        (if (expandable-key? key)
          (expanded-key acc key expanded-value strict)
          (literal-key acc key expanded-value strict))))
    {}
    obj))


(defn expand
  "Expands dotted keys into nested objects in safe mode.

  Recursively traverses a decoded TOON value and expands any keys
  containing dots (`.`) into nested object structures, provided all segments
  are valid identifiers.

  Expansion rules:
  - Keys containing dots are split into segments
  - All segments must pass identifier validation
  - Non-eligible keys (with special characters) are left as literal dotted keys
  - Deep merge: When multiple dotted keys expand to the same path, their values are merged if both are objects
  - Conflict handling:
    - strict=true: Throws ex-info on conflicts (non-object collision)
    - strict=false: LWW (silent overwrite)

  Parameters:
    - value: Decoded value to expand
    - strict: Whether to throw errors on conflicts
    - expand-paths: Whether to expand paths (:off or :safe)

  Returns:
    Expanded value with dotted keys reconstructed as nested objects

  Throws:
    ex-info if conflicts occur in strict mode"
  [value strict expand-paths]
  (cond
    ;; Path expansion disabled
    (not= expand-paths :safe)
    value

    ;; Array - recursively expand elements
    (vector? value)
    (mapv #(expand % strict expand-paths) value)

    ;; Object - expand dotted keys
    (map? value)
    (expanded-object value strict expand-paths)

    ;; Primitive - return as-is
    :else
    value))
