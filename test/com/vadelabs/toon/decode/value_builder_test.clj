(ns com.vadelabs.toon.decode.value-builder-test
  (:require
    [clojure.test :refer [deftest is testing]]
    [com.vadelabs.toon.decode.value-builder :as vb]
    [com.vadelabs.toon.interface :as toon]))


;; ============================================================================
;; Basic Reconstruction Tests
;; ============================================================================

(deftest test-build-simple-object
  (testing "Build simple object from events"
    (let [events [{:type :start-object}
                  {:type :key :key "name"}
                  {:type :primitive :value "Alice"}
                  {:type :key :key "age"}
                  {:type :primitive :value 30}
                  {:type :end-object}]
          result (vb/events->value events)]
      (is (= {"name" "Alice" "age" 30} result)))))


(deftest test-build-nested-object
  (testing "Build nested object from events"
    (let [events [{:type :start-object}
                  {:type :key :key "user"}
                  {:type :start-object}
                  {:type :key :key "name"}
                  {:type :primitive :value "Alice"}
                  {:type :key :key "age"}
                  {:type :primitive :value 30}
                  {:type :end-object}
                  {:type :end-object}]
          result (vb/events->value events)]
      (is (= {"user" {"name" "Alice" "age" 30}} result)))))


(deftest test-build-array
  (testing "Build array from events"
    (let [events [{:type :start-array}
                  {:type :primitive :value 1}
                  {:type :primitive :value 2}
                  {:type :primitive :value 3}
                  {:type :end-array}]
          result (vb/events->value events)]
      (is (= [1 2 3] result)))))


(deftest test-build-root-primitive
  (testing "Build root primitive from events"
    (let [events [{:type :primitive :value "Hello World"}]
          result (vb/events->value events)]
      (is (= "Hello World" result)))))


(deftest test-build-empty-object
  (testing "Build empty object"
    (let [events [{:type :start-object}
                  {:type :end-object}]
          result (vb/events->value events)]
      (is (= {} result)))))


(deftest test-build-empty-array
  (testing "Build empty array"
    (let [events [{:type :start-array}
                  {:type :end-array}]
          result (vb/events->value events)]
      (is (= [] result)))))


;; ============================================================================
;; Complex Structure Tests
;; ============================================================================

(deftest test-build-array-of-objects
  (testing "Build array of objects"
    (let [events [{:type :start-array}
                  {:type :start-object}
                  {:type :key :key "id"}
                  {:type :primitive :value 1}
                  {:type :key :key "name"}
                  {:type :primitive :value "Alice"}
                  {:type :end-object}
                  {:type :start-object}
                  {:type :key :key "id"}
                  {:type :primitive :value 2}
                  {:type :key :key "name"}
                  {:type :primitive :value "Bob"}
                  {:type :end-object}
                  {:type :end-array}]
          result (vb/events->value events)]
      (is (= [{"id" 1 "name" "Alice"}
              {"id" 2 "name" "Bob"}]
             result)))))


(deftest test-build-object-with-array
  (testing "Build object containing an array"
    (let [events [{:type :start-object}
                  {:type :key :key "name"}
                  {:type :primitive :value "Alice"}
                  {:type :key :key "tags"}
                  {:type :start-array}
                  {:type :primitive :value "reading"}
                  {:type :primitive :value "gaming"}
                  {:type :end-array}
                  {:type :end-object}]
          result (vb/events->value events)]
      (is (= {"name" "Alice" "tags" ["reading" "gaming"]} result)))))


;; ============================================================================
;; Integration Tests (Roundtrip)
;; ============================================================================

(deftest test-roundtrip-simple
  (testing "Roundtrip: decode -> events -> value matches decode"
    (let [input "name: Alice\nage: 30"
          direct-decode (toon/decode input)
          via-events (-> input
                         toon/events
                         vb/events->value)]
      (is (= direct-decode via-events)))))


(deftest test-roundtrip-nested
  (testing "Roundtrip with nested structures"
    (let [input "user:\n  name: Alice\n  age: 30\n  tags[2]: reading,gaming"
          direct-decode (toon/decode input)
          via-events (-> input
                         toon/events
                         vb/events->value)]
      (is (= direct-decode via-events)))))


(deftest test-roundtrip-array
  (testing "Roundtrip with root array"
          (let [input "[3]: a,b,c"
          direct-decode (toon/decode input)
          via-events (-> input
                         toon/events
                         vb/events->value)]
      (is (= direct-decode via-events)))))


(deftest test-roundtrip-tabular-array
  (testing "Roundtrip with tabular array"
    (let [input "[2]{id,name}:\n  1,Alice\n  2,Bob"
          direct-decode (toon/decode input)
          via-events (-> input
                         toon/events
                         vb/events->value)]
      (is (= direct-decode via-events)))))


;; ============================================================================
;; Error Handling Tests
;; ============================================================================

(deftest test-error-incomplete-stream
  (testing "Error on incomplete event stream"
    (let [events [{:type :start-object}
                  {:type :key :key "name"}
                  ;; Missing primitive and end-object
                  ]]
      (is (thrown? Exception
                   (vb/events->value events))))))


(deftest test-error-mismatched-brackets
  (testing "Error on mismatched end bracket"
    (let [events [{:type :start-object}
                  {:type :key :key "name"}
                  {:type :primitive :value "Alice"}
                  {:type :end-array}]]  ;; Wrong closing bracket!
      (is (thrown? Exception
                   (vb/events->value events))))))


(deftest test-error-key-without-object
  (testing "Error on key event outside object"
    (let [events [{:type :key :key "name"}
                  {:type :primitive :value "Alice"}]]
      (is (thrown? Exception
                   (vb/events->value events))))))
