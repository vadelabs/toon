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
  "Event emitted when array parsing begins."
  []
  {:type :start-array})


(defn end-array
  "Event emitted when array parsing completes."
  []
  {:type :end-array})


(defn key-event
  "Event emitted when object key is encountered.

  Parameters:
    - k: Key string"
  [k]
  {:type :key
   :key k})


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

(defn start-object? [event]
  (= :start-object (:type event)))


(defn end-object? [event]
  (= :end-object (:type event)))


(defn start-array? [event]
  (= :start-array (:type event)))


(defn end-array? [event]
  (= :end-array (:type event)))


(defn key-event? [event]
  (= :key (:type event)))


(defn primitive? [event]
  (= :primitive (:type event)))


;; ============================================================================
;; Event Utilities
;; ============================================================================

(defn event-value
  "Extract value from primitive event."
  [event]
  (when (primitive? event)
    (:value event)))


(defn event-key
  "Extract key from key event."
  [event]
  (when (key-event? event)
    (:key event)))
