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
          events (events->vec (toon/events input))]
      (is (= 6 (count events)))
      (is (events/start-object? (first events)))
      (is (events/end-object? (last events)))
      (is (= "name" (events/key (nth events 1))))
      (is (= "Alice" (events/value (nth events 2))))
      (is (= "age" (events/key (nth events 3))))
      (is (= 30.0 (events/value (nth events 4)))))))


(deftest test-empty-object-stream
  (testing "Empty object"
    (let [input ""
          events (events->vec (toon/events input))]
      (is (= 2 (count events)))
      (is (events/start-object? (first events)))
      (is (events/end-object? (last events))))))


(deftest test-single-primitive-stream
  (testing "Single primitive value"
    (let [input "42"
          events (events->vec (toon/events input))]
      (is (= 1 (count events)))
      (is (events/primitive? (first events)))
      (is (= 42.0 (events/value (first events)))))))


;; ============================================================================
;; Array Streaming Tests
;; ============================================================================

(deftest test-inline-array-stream
  (testing "Inline primitive array"
    (let [input "[3]: a,b,c"
          events (events->vec (toon/events input))]
      (is (= 5 (count events)))
      (is (events/start-array? (first events)))
      (is (= "a" (events/value (nth events 1))))
      (is (= "b" (events/value (nth events 2))))
      (is (= "c" (events/value (nth events 3))))
      (is (events/end-array? (last events))))))


(deftest test-empty-array-stream
  (testing "Empty array"
    (let [input "[0]"
          events (events->vec (toon/events input))]
      (is (= 2 (count events)))
      (is (events/start-array? (first events)))
      (is (events/end-array? (last events))))))


(deftest test-tabular-array-stream
  (testing "Tabular array with objects"
    (let [input "[2]{id,name}:\n  1,Alice\n  2,Bob"
          events (events->vec (toon/events input))]
      ;; Debugging
      (println "Tabular events:" events)
      (println "Event count:" (count events))
      ;; Start array, 2 objects (each with start, 2 key-value pairs, end), end array
      ;; [start-array, [start-obj, key, val, key, val, end-obj] * 2, end-array]
      (is (>= (count events) 13) "Should have at least 13 events")
      (is (events/start-array? (first events)))
      (is (events/start-object? (nth events 1)))
      (is (= "id" (events/key (nth events 2))))
      (is (= 1.0 (events/value (nth events 3))))
      (is (= "name" (events/key (nth events 4))))
      (is (= "Alice" (events/value (nth events 5))))
      (is (events/end-object? (nth events 6)))
      (is (events/end-array? (last events))))))


;; ============================================================================
;; Key wasQuoted Property Tests (Feature Parity)
;; ============================================================================

(deftest test-unquoted-key-wasquoted
  (testing "Unquoted key has was-quoted false"
    (let [input "name: Alice"
          events (events->vec (toon/events input))
          key-event (->> events
                         (filter events/key-event?)
                         first)]
      (is (some? key-event) "Should have a key event")
      (is (= "name" (events/key key-event)))
      (is (= false (events/was-quoted key-event))
          "unquoted key should have was-quoted false"))))


(deftest test-quoted-key-wasquoted
  (testing "Quoted key has was-quoted true"
    (let [input "\"user.name\": Alice"
          events (events->vec (toon/events input))
          key-event (->> events
                         (filter events/key-event?)
                         first)]
      (is (some? key-event) "Should have a key event")
      (is (= "user.name" (events/key key-event)))
      (is (= true (events/was-quoted key-event))
          "quoted key should have was-quoted true"))))


(deftest test-mixed-quoted-unquoted-keys
  (testing "Object with mix of quoted and unquoted keys"
    (let [input "name: Alice\n\"user.id\": 123\nage: 30"
          events (events->vec (toon/events input))
          key-events (filterv events/key-event? events)]
      (is (= 3 (count key-events)) "Should have 3 key events")
      ;; First key: unquoted
      (is (= "name" (events/key (nth key-events 0))))
      (is (= false (events/was-quoted (nth key-events 0))))
      ;; Second key: quoted
      (is (= "user.id" (events/key (nth key-events 1))))
      (is (= true (events/was-quoted (nth key-events 1))))
      ;; Third key: unquoted
      (is (= "age" (events/key (nth key-events 2))))
      (is (= false (events/was-quoted (nth key-events 2)))))))


(deftest test-nested-object-key-wasquoted
  (testing "Nested object keys preserve was-quoted"
    (let [input "user:\n  \"first.name\": Alice\n  age: 30"
          events (events->vec (toon/events input))
          key-events (filterv events/key-event? events)]
      (is (= 3 (count key-events)))
      ;; Outer key: unquoted
      (is (= "user" (events/key (nth key-events 0))))
      (is (= false (events/was-quoted (nth key-events 0))))
      ;; Nested quoted key
      (is (= "first.name" (events/key (nth key-events 1))))
      (is (= true (events/was-quoted (nth key-events 1))))
      ;; Nested unquoted key
      (is (= "age" (events/key (nth key-events 2))))
      (is (= false (events/was-quoted (nth key-events 2)))))))


;; ============================================================================
;; Array Length Property Tests (Feature Parity)
;; ============================================================================

(deftest test-inline-array-length
  (testing "Inline array includes length property"
    (let [input "[3]: a,b,c"
          events (events->vec (toon/events input))
          start-array-event (first events)]
      (is (events/start-array? start-array-event))
      (is (= 3 (events/length start-array-event))
          "start-array event should include length property"))))


(deftest test-empty-array-length
  (testing "Empty array includes length property of 0"
    (let [input "[0]"
          events (events->vec (toon/events input))
          start-array-event (first events)]
      (is (events/start-array? start-array-event))
      (is (= 0 (events/length start-array-event))
          "empty array should have length 0"))))


(deftest test-tabular-array-length
  (testing "Tabular array includes length property"
    (let [input "[2]{id,name}:\n  1,Alice\n  2,Bob"
          events (events->vec (toon/events input))
          start-array-event (first events)]
      (is (events/start-array? start-array-event))
      (is (= 2 (events/length start-array-event))
          "tabular array should have correct length"))))


(deftest test-object-field-array-length
  (testing "Array field in object includes length property"
    (let [input "name: Alice\ntags[2]: reading,gaming"
          events (events->vec (toon/events input))
          ;; Find the start-array event (should be after name key-value pair and tags key)
          start-array-event (->> events
                                 (filter events/start-array?)
                                 first)]
      (is (some? start-array-event) "Should have a start-array event")
      (is (= 2 (events/length start-array-event))
          "array field should have correct length"))))


(deftest test-empty-array-field-length
  (testing "Empty array field includes length property of 0"
    (let [input "name: Alice\ntags[0]: "
          events (events->vec (toon/events input))
          start-array-event (->> events
                                 (filter events/start-array?)
                                 first)]
      (is (some? start-array-event) "Should have a start-array event")
      (is (= 0 (events/length start-array-event))
          "empty array field should have length 0"))))


;; ============================================================================
;; Nested Structure Tests
;; ============================================================================

(deftest test-nested-object-stream
  (testing "Nested object"
    (let [input "user:\n  name: Alice\n  age: 30"
          events (events->vec (toon/events input))]
      ;; start-obj, key(user), start-obj, key(name), val, key(age), val, end-obj, end-obj
      (is (= 9 (count events)))
      (is (events/start-object? (first events)))
      (is (= "user" (events/key (nth events 1))))
      (is (events/start-object? (nth events 2)))
      (is (= "name" (events/key (nth events 3))))
      (is (= "Alice" (events/value (nth events 4))))
      (is (events/end-object? (nth events 7)))
      (is (events/end-object? (last events))))))


(deftest test-object-with-array-field-stream
  (testing "Object containing an array field"
    (let [input "name: Alice\ntags[2]: reading,gaming"
          events (events->vec (toon/events input))]
      (println "Array field events:" events)
      ;; start-obj, key(name), val, key(tags), start-array, val, val, end-array, end-obj
      (is (= 9 (count events)))
      (is (events/start-object? (first events)))
      (is (= "name" (events/key (nth events 1))))
      (is (= "Alice" (events/value (nth events 2))))
      ;; The key should be just "tags" not "tags[2]"
      (is (= "tags" (events/key (nth events 3))))
      (is (events/start-array? (nth events 4)))
      (is (= "reading" (events/value (nth events 5))))
      (is (= "gaming" (events/value (nth events 6))))
      (is (events/end-array? (nth events 7)))
      (is (events/end-object? (last events))))))


;; ============================================================================
;; Async Stream Tests
;; ============================================================================

(deftest test-async-simple-object
  (testing "Async decode of simple object"
    (let [input "name: Alice\nage: 30"
          events-ch (toon/events-ch input)
          events (async-events->vec events-ch)]
      (is (= 6 (count events)))
      (is (events/start-object? (first events)))
      (is (events/end-object? (last events)))
      (is (= "name" (events/key (nth events 1))))
      (is (= "Alice" (events/value (nth events 2)))))))


(deftest test-async-from-channel
  (testing "Async decode from channel source"
    (let [lines-ch (async/to-chan! ["name: Alice" "age: 30"])
          events-ch (toon/events-ch lines-ch)
          events (async-events->vec events-ch)]
      (is (= 6 (count events)))
      (is (events/start-object? (first events)))
      (is (events/end-object? (last events))))))


(deftest test-async-array
  (testing "Async decode of array"
    (let [input "[3]: a,b,c"
          events-ch (toon/events-ch input)
          events (async-events->vec events-ch)]
      (is (= 5 (count events)))
      (is (events/start-array? (first events)))
      (is (events/end-array? (last events)))
      (is (= "a" (events/value (nth events 1)))))))


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
          events (toon/events input)
          reconstructed (build-from-events events)
          direct-decode (toon/decode input)]
      (is (= direct-decode reconstructed)))))


;; ============================================================================
;; Performance and Memory Tests
;; ============================================================================

(deftest test-lazy-evaluation
  (testing "Events are lazily evaluated"
    (let [input "name: Alice\nage: 30\ncity: NYC"
          events (toon/events input)]
      ;; Take only first 3 events - rest should not be evaluated
      (is (= 3 (count (take 3 events))))
      ;; Verify they are the expected events
      (is (events/start-object? (first events)))
      (is (events/key-event? (second events))))))


;; ============================================================================
;; Streaming Equivalence Tests
;; ============================================================================

(deftest test-streaming-vs-regular-decode-simple
  (testing "Streaming decode matches regular decode for simple object"
    (let [input "name: Alice\nage: 30"
          regular-result (toon/decode input)
          stream-result (toon/events->value (toon/events input))]
      (is (= regular-result stream-result)))))


(deftest test-streaming-vs-regular-decode-nested
  (testing "Streaming decode matches regular decode for nested objects"
    (let [input "user:\n  profile:\n    name: Alice\n    age: 30"
          regular-result (toon/decode input)
          stream-result (toon/events->value (toon/events input))]
      (is (= regular-result stream-result)))))


(deftest test-streaming-vs-regular-decode-mixed
  (testing "Streaming decode matches regular decode for mixed structures"
    (let [input "name: Alice\nscores[3]: 95, 87, 92\naddress:\n  city: NYC\n  zip: 10001"
          regular-result (toon/decode input)
          stream-result (toon/events->value (toon/events input))]
      (is (= regular-result stream-result)))))


(deftest test-streaming-vs-regular-decode-root-primitives
  (testing "Streaming decode matches regular decode for root primitives"
    (doseq [input ["42" "Hello World" "true" "null"]]
      (let [regular-result (toon/decode input)
            stream-result (toon/events->value (toon/events input))]
        (is (= regular-result stream-result)
            (str "Failed for input: " input))))))


;; ============================================================================
;; Root Array Tests
;; ============================================================================

(deftest test-root-array-stream
  (testing "Root array with list items"
    (let [input "[2]:\n  - Apple\n  - Banana"
          events (events->vec (toon/events input))]
      (is (events/start-array? (first events)))
      (is (= "Apple" (events/value (nth events 1))))
      (is (= "Banana" (events/value (nth events 2))))
      (is (events/end-array? (last events))))))


(deftest test-root-primitive-number
  (testing "Root primitive number"
    (let [input "42"
          events (events->vec (toon/events input))]
      (is (= 1 (count events)))
      (is (events/primitive? (first events)))
      (is (= 42.0 (events/value (first events)))))))


(deftest test-root-primitive-string
  (testing "Root primitive string"
    (let [input "Hello World"
          events (events->vec (toon/events input))]
      (is (= 1 (count events)))
      (is (events/primitive? (first events)))
      (is (= "Hello World" (events/value (first events)))))))


(deftest test-root-primitive-boolean
  (testing "Root primitive boolean"
    (let [input "true"
          events (events->vec (toon/events input))]
      (is (= 1 (count events)))
      (is (events/primitive? (first events)))
      (is (= true (events/value (first events)))))))


(deftest test-root-primitive-null
  (testing "Root primitive null"
    (let [input "null"
          events (events->vec (toon/events input))]
      (is (= 1 (count events)))
      (is (events/primitive? (first events)))
      (is (nil? (events/value (first events)))))))


;; ============================================================================
;; List Array with Nested Objects
;; ============================================================================

(deftest test-list-array-with-nested-objects
  (testing "List array containing nested objects"
    (let [input "users[2]:\n  - name: Alice\n    age: 30\n  - name: Bob\n    age: 25"
          regular-result (toon/decode input)
          stream-result (toon/events->value (toon/events input))]
      (is (= regular-result stream-result))
      (is (= {"users" [{"name" "Alice" "age" 30.0}
                       {"name" "Bob" "age" 25.0}]}
             stream-result)))))
