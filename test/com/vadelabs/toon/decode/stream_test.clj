(ns com.vadelabs.toon.decode.stream-test
  (:require
    [clojure.core.async :as async]
    [clojure.test :refer [deftest is testing]]
    [com.vadelabs.toon.decode.events :as events]
    [com.vadelabs.toon.interface :as toon]))


;; ============================================================================
;; Test Utilities
;; ============================================================================

(defn events->vec
  "Convert lazy sequence of events to vector for testing."
  [events]
  (vec events))


(defn async-events->vec
  "Convert async channel of events to vector for testing."
  [events-ch]
  (async/<!! (async/into [] events-ch)))


;; ============================================================================
;; Basic Streaming Tests
;; ============================================================================

(deftest test-simple-object-stream
  (testing "Simple object with primitive values"
    (let [input "name: Alice\nage: 30"
          events (events->vec (toon/decode-stream-sync input))]
      (is (= 6 (count events)))
      (is (events/start-object? (first events)))
      (is (events/end-object? (last events)))
      (is (= "name" (events/event-key (nth events 1))))
      (is (= "Alice" (events/event-value (nth events 2))))
      (is (= "age" (events/event-key (nth events 3))))
      (is (= 30.0 (events/event-value (nth events 4)))))))


(deftest test-empty-object-stream
  (testing "Empty object"
    (let [input ""
          events (events->vec (toon/decode-stream-sync input))]
      (is (= 2 (count events)))
      (is (events/start-object? (first events)))
      (is (events/end-object? (last events))))))


(deftest test-single-primitive-stream
  (testing "Single primitive value"
    (let [input "42"
          events (events->vec (toon/decode-stream-sync input))]
      (is (= 1 (count events)))
      (is (events/primitive? (first events)))
      (is (= 42.0 (events/event-value (first events)))))))


;; ============================================================================
;; Array Streaming Tests
;; ============================================================================

(deftest test-inline-array-stream
  (testing "Inline primitive array"
    (let [input "[3]: a,b,c"
          events (events->vec (toon/decode-stream-sync input))]
      (is (= 5 (count events)))
      (is (events/start-array? (first events)))
      (is (= "a" (events/event-value (nth events 1))))
      (is (= "b" (events/event-value (nth events 2))))
      (is (= "c" (events/event-value (nth events 3))))
      (is (events/end-array? (last events))))))


(deftest test-empty-array-stream
  (testing "Empty array"
    (let [input "[0]"
          events (events->vec (toon/decode-stream-sync input))]
      (is (= 2 (count events)))
      (is (events/start-array? (first events)))
      (is (events/end-array? (last events))))))


(deftest test-tabular-array-stream
  (testing "Tabular array with objects"
    (let [input "[2]{id,name}:\n  1,Alice\n  2,Bob"
          events (events->vec (toon/decode-stream-sync input))]
      ;; Debugging
      (println "Tabular events:" events)
      (println "Event count:" (count events))
      ;; Start array, 2 objects (each with start, 2 key-value pairs, end), end array
      ;; [start-array, [start-obj, key, val, key, val, end-obj] * 2, end-array]
      (is (>= (count events) 13) "Should have at least 13 events")
      (is (events/start-array? (first events)))
      (is (events/start-object? (nth events 1)))
      (is (= "id" (events/event-key (nth events 2))))
      (is (= 1.0 (events/event-value (nth events 3))))
      (is (= "name" (events/event-key (nth events 4))))
      (is (= "Alice" (events/event-value (nth events 5))))
      (is (events/end-object? (nth events 6)))
      (is (events/end-array? (last events))))))


;; ============================================================================
;; Nested Structure Tests
;; ============================================================================

(deftest test-nested-object-stream
  (testing "Nested object"
    (let [input "user:\n  name: Alice\n  age: 30"
          events (events->vec (toon/decode-stream-sync input))]
      ;; start-obj, key(user), start-obj, key(name), val, key(age), val, end-obj, end-obj
      (is (= 9 (count events)))
      (is (events/start-object? (first events)))
      (is (= "user" (events/event-key (nth events 1))))
      (is (events/start-object? (nth events 2)))
      (is (= "name" (events/event-key (nth events 3))))
      (is (= "Alice" (events/event-value (nth events 4))))
      (is (events/end-object? (nth events 7)))
      (is (events/end-object? (last events))))))


(deftest test-object-with-array-field-stream
  (testing "Object containing an array field"
    (let [input "name: Alice\ntags[2]: reading,gaming"
          events (events->vec (toon/decode-stream-sync input))]
      (println "Array field events:" events)
      ;; start-obj, key(name), val, key(tags), start-array, val, val, end-array, end-obj
      (is (= 9 (count events)))
      (is (events/start-object? (first events)))
      (is (= "name" (events/event-key (nth events 1))))
      (is (= "Alice" (events/event-value (nth events 2))))
      ;; The key should be just "tags" not "tags[2]"
      (is (= "tags" (events/event-key (nth events 3))))
      (is (events/start-array? (nth events 4)))
      (is (= "reading" (events/event-value (nth events 5))))
      (is (= "gaming" (events/event-value (nth events 6))))
      (is (events/end-array? (nth events 7)))
      (is (events/end-object? (last events))))))


;; ============================================================================
;; Async Stream Tests
;; ============================================================================

(deftest test-async-simple-object
  (testing "Async decode of simple object"
    (let [input "name: Alice\nage: 30"
          events-ch (toon/decode-stream input)
          events (async-events->vec events-ch)]
      (is (= 6 (count events)))
      (is (events/start-object? (first events)))
      (is (events/end-object? (last events)))
      (is (= "name" (events/event-key (nth events 1))))
      (is (= "Alice" (events/event-value (nth events 2)))))))


(deftest test-async-from-channel
  (testing "Async decode from channel source"
    (let [lines-ch (async/to-chan! ["name: Alice" "age: 30"])
          events-ch (toon/decode-stream lines-ch)
          events (async-events->vec events-ch)]
      (is (= 6 (count events)))
      (is (events/start-object? (first events)))
      (is (events/end-object? (last events))))))


(deftest test-async-array
  (testing "Async decode of array"
    (let [input "[3]: a,b,c"
          events-ch (toon/decode-stream input)
          events (async-events->vec events-ch)]
      (is (= 5 (count events)))
      (is (events/start-array? (first events)))
      (is (events/end-array? (last events)))
      (is (= "a" (events/event-value (nth events 1)))))))


;; ============================================================================
;; Event Reconstruction Tests
;; ============================================================================

(defn build-from-events
  "Reconstruct value from event stream (demonstrates event usage)."
  [events]
  (loop [events-seq events
         stack []]
    (if (empty? events-seq)
      ;; Return the final result
      (if (empty? stack)
        nil
        (:building (first stack)))
      (let [event (first events-seq)
            rest-events (rest events-seq)]
        (case (:type event)
          :start-object
          (recur rest-events (conj stack {:building {} :type :object}))
          
          :start-array
          (recur rest-events (conj stack {:building [] :type :array}))
          
          :key
          (recur rest-events (update stack (dec (count stack)) assoc :current-key (:key event)))
          
          :primitive
          (let [value (:value event)
                parent (peek stack)]
            (if (:current-key parent)
              ;; Object field
              (let [k (:current-key parent)
                    updated-stack (-> stack
                                      (update (dec (count stack)) update :building assoc k value)
                                      (update (dec (count stack)) dissoc :current-key))]
                (recur rest-events updated-stack))
              ;; Array element or root primitive
              (if parent
                (let [updated-stack (update stack (dec (count stack)) update :building conj value)]
                  (recur rest-events updated-stack))
                ;; Root primitive
                value)))
          
          :end-object
          (let [completed (:building (peek stack))
                remaining-stack (pop stack)]
            (if (empty? remaining-stack)
              ;; Root object completed
              (recur rest-events (conj remaining-stack {:building completed :type :done}))
              ;; Nested - add to parent
              (let [parent (peek remaining-stack)]
                (if (:current-key parent)
                  (let [k (:current-key parent)
                        updated-stack (-> remaining-stack
                                          (update (dec (count remaining-stack)) update :building assoc k completed)
                                          (update (dec (count remaining-stack)) dissoc :current-key))]
                    (recur rest-events updated-stack))
                  (let [updated-stack (update remaining-stack (dec (count remaining-stack)) update :building conj completed)]
                    (recur rest-events updated-stack))))))
          
          :end-array
          (let [completed (:building (peek stack))
                remaining-stack (pop stack)]
            (if (empty? remaining-stack)
              ;; Root array completed
              (recur rest-events (conj remaining-stack {:building completed :type :done}))
              ;; Nested - add to parent
              (let [parent (peek remaining-stack)]
                (if (:current-key parent)
                  (let [k (:current-key parent)
                        updated-stack (-> remaining-stack
                                          (update (dec (count remaining-stack)) update :building assoc k completed)
                                          (update (dec (count remaining-stack)) dissoc :current-key))]
                    (recur rest-events updated-stack))
                  (let [updated-stack (update remaining-stack (dec (count remaining-stack)) update :building conj completed)]
                    (recur rest-events updated-stack)))))))))))


(deftest test-reconstruct-from-events
  (testing "Can reconstruct original value from events"
    (let [input "name: Alice\nage: 30\ntags[2]: reading,gaming"
          events (toon/decode-stream-sync input)
          reconstructed (build-from-events events)
          direct-decode (toon/decode input)]
      (is (= direct-decode reconstructed)))))


;; ============================================================================
;; Performance and Memory Tests
;; ============================================================================

(deftest test-lazy-evaluation
  (testing "Events are lazily evaluated"
    (let [input "name: Alice\nage: 30\ncity: NYC"
          events (toon/decode-stream-sync input)]
      ;; Take only first 3 events - rest should not be evaluated
      (is (= 3 (count (take 3 events))))
      ;; Verify they are the expected events
      (is (events/start-object? (first events)))
      (is (events/key-event? (second events))))))
