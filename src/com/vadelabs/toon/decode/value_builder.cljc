(ns com.vadelabs.toon.decode.value-builder
  "Reconstructs values from event streams.

  Provides functions to build complete value trees from parse events,
  enabling reconstruction of the original data structure after streaming decode."
  (:require
    [com.vadelabs.toon.decode.events :as events]))


;; ============================================================================
;; Build Context Types
;; ============================================================================

(defn- object-context
  "Create an object build context."
  []
  {:type :object
   :building {}
   :current-key nil})


(defn- array-context
  "Create an array build context."
  []
  {:type :array
   :building []})


;; ============================================================================
;; Stack Operations
;; ============================================================================

(defn- push-value-to-parent
  "Push a completed value to its parent context."
  [stack value]
  (if (empty? stack)
    stack
    (let [parent (peek stack)
          parent-type (:type parent)]
      (case parent-type
        :object
        (if-let [k (:current-key parent)]
          (-> stack
              pop
              (conj (-> parent
                        (update :building assoc k value)
                        (dissoc :current-key))))
          (throw (ex-info "Object value without preceding key" {:parent parent :value value})))

        :array
        (-> stack
            pop
            (conj (update parent :building conj value)))

        (throw (ex-info "Invalid parent type" {:parent parent}))))))


;; ============================================================================
;; Event Handlers
;; ============================================================================

(defn- handle-start-object
  "Handle start-object event."
  [stack]
  (conj stack (object-context)))


(defn- handle-end-object
  "Handle end-object event."
  [stack]
  (when (empty? stack)
    (throw (ex-info "Unexpected end-object event" {})))
  (let [context (peek stack)
        remaining (pop stack)]
    (when (not= :object (:type context))
      (throw (ex-info "Mismatched end-object event" {:context context})))
    (if (empty? remaining)
      ;; Root object - keep as final result
      [(assoc context :type :done)]
      ;; Nested object - push to parent
      (push-value-to-parent remaining (:building context)))))


(defn- handle-start-array
  "Handle start-array event."
  [stack]
  (conj stack (array-context)))


(defn- handle-end-array
  "Handle end-array event."
  [stack]
  (when (empty? stack)
    (throw (ex-info "Unexpected end-array event" {})))
  (let [context (peek stack)
        remaining (pop stack)]
    (when (not= :array (:type context))
      (throw (ex-info "Mismatched end-array event" {:context context})))
    (if (empty? remaining)
      ;; Root array - keep as final result
      [(assoc context :type :done)]
      ;; Nested array - push to parent
      (push-value-to-parent remaining (:building context)))))


(defn- handle-key
  "Handle key event."
  [stack event]
  (when (empty? stack)
    (throw (ex-info "Key event outside of object context" {:event event})))
  (let [parent (peek stack)]
    (when (not= :object (:type parent))
      (throw (ex-info "Key event in non-object context" {:parent parent :event event})))
    (-> stack
        pop
        (conj (assoc parent :current-key (events/key event))))))


(defn- handle-primitive
  "Handle primitive event."
  [stack event]
  (let [value (events/value event)]
    (if (empty? stack)
      ;; Root primitive
      [{:type :done :building value}]
      ;; Add to parent context
      (push-value-to-parent stack value))))


;; ============================================================================
;; Public API
;; ============================================================================

(defn events->value
  "Reconstruct value from event stream.

  Takes a sequence of parse events (from decode-stream-sync or decode-stream)
  and reconstructs the original data structure.

  Parameters:
    - events: Sequence or iterable of parse events

  Returns:
    Reconstructed value (map, vector, or primitive)

  Example:
    (let [events [{:type :start-object}
                  {:type :key :key \"name\"}
                  {:type :primitive :value \"Alice\"}
                  {:type :key :key \"age\"}
                  {:type :primitive :value 30}
                  {:type :end-object}]]
      (events->value events))
    ;=> {\"name\" \"Alice\", \"age\" 30}

  Throws:
    - ex-info on malformed event streams (unmatched brackets, missing keys, etc.)"
  [events]
  (loop [events-seq (seq events)
         stack []]
    (if (empty? events-seq)
      ;; End of events - validate and extract result
      (cond
        (empty? stack)
        nil

        ;; Stack should have exactly one item with :done type
        (and (= 1 (count stack))
             (= :done (:type (first stack))))
        (:building (first stack))

        ;; Otherwise, incomplete stream
        :else
        (throw (ex-info "Incomplete event stream: stack not empty at end"
                        {:stack stack})))
      ;; Process next event
      (let [event (first events-seq)
            event-type (:type event)
            new-stack (case event-type
                        :start-object (handle-start-object stack)
                        :end-object   (handle-end-object stack)
                        :start-array  (handle-start-array stack)
                        :end-array    (handle-end-array stack)
                        :key          (handle-key stack event)
                        :primitive    (handle-primitive stack event)
                        (throw (ex-info "Unknown event type" {:event event})))]
        (recur (rest events-seq) new-stack)))))
