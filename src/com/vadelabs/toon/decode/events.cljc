(ns com.vadelabs.toon.decode.events
  "Event types and builders for streaming TOON decode.

  Events represent parse actions instead of building full value trees.
  This enables memory-efficient processing of large TOON documents.")


;; ============================================================================
;; Event Type Constructors
;; ============================================================================

(defn start-object
  "Event emitted when object parsing begins."
  []
  {:type :start-object})


(defn end-object
  "Event emitted when object parsing completes."
  []
  {:type :end-object})


(defn start-array
  "Event emitted when array parsing begins.

  Parameters:
    - length: Array length (for feature parity with TypeScript implementation)"
  [length]
  {:type :start-array
   :length length})


(defn end-array
  "Event emitted when array parsing completes."
  []
  {:type :end-array})


(defn key-event
  "Event emitted when object key is encountered.

  Parameters:
    - k: Key string
    - was-quoted: Optional boolean indicating if key was quoted in source (default: false)"
  ([k]
   (key-event k false))
  ([k was-quoted]
   {:type :key
    :key k
    :was-quoted was-quoted}))


(defn primitive
  "Event emitted for primitive values.

  Parameters:
    - value: Primitive value (string, number, boolean, nil)"
  [value]
  {:type :primitive
   :value value})


;; ============================================================================
;; Event Predicates
;; ============================================================================

(defn start-object?
  [event]
  (= :start-object (:type event)))


(defn end-object?
  [event]
  (= :end-object (:type event)))


(defn start-array?
  [event]
  (= :start-array (:type event)))


(defn end-array?
  [event]
  (= :end-array (:type event)))


(defn key-event?
  [event]
  (= :key (:type event)))


(defn primitive?
  [event]
  (= :primitive (:type event)))


;; ============================================================================
;; Event Utilities
;; ============================================================================

(defn value
  "Extract value from primitive event.

  Following Stuart Sierra's naming: pure function returning a value uses a noun."
  [event]
  (when (primitive? event)
    (:value event)))


(defn key
  "Extract key from key event.

  Following Stuart Sierra's naming: pure function returning a value uses a noun."
  [event]
  (when (key-event? event)
    (:key event)))


(defn was-quoted
  "Extract wasQuoted flag from key event.

  Following Stuart Sierra's naming: pure function returning a value uses a noun.
  Returns true if the key was quoted in the original TOON source, false otherwise."
  [event]
  (when (key-event? event)
    (:was-quoted event)))


(defn length
  "Extract length from start-array event.

  Following Stuart Sierra's naming: pure function returning a value uses a noun."
  [event]
  (when (start-array? event)
    (:length event)))
