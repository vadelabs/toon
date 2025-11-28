(ns com.vadelabs.toon.decode.items-test
  "Tests for list item type detection and decoding."
  (:require
    #?(:clj [clojure.test :refer [deftest is testing]]
       :cljs [cljs.test :refer [deftest is testing]])
    [com.vadelabs.toon.decode.items :as items]
    [com.vadelabs.toon.decode.scanner :as scanner]))


;; ============================================================================
;; List Item Type Detection Tests
;; ============================================================================

(deftest list-item-type-test
  (testing "Detect list item types correctly"
    (testing "array items"
      (is (= :array (#'items/list-item-type "[3]: 1,2,3")))
      (is (= :array (#'items/list-item-type "[2]{id,name}:"))))

    (testing "object items"
      (is (= :object (#'items/list-item-type "key: value")))
      (is (= :object (#'items/list-item-type "name: Alice"))))

    (testing "primitive items"
      (is (= :primitive (#'items/list-item-type "hello")))
      (is (= :primitive (#'items/list-item-type "42")))
      (is (= :primitive (#'items/list-item-type "true"))))))


;; ============================================================================
;; List Item Decoding Tests
;; ============================================================================

(deftest list-item-primitive-test
  (testing "Decode primitive list item"
    (let [input "- hello"
          scan-result (scanner/to-parsed-lines input)
          cursor (scanner/cursor-from-scan-result scan-result)
          [line _] (scanner/next-cursor cursor)
          [result _] (items/list-item line cursor 0 "," true)]
      (is (= "hello" result)))))


(deftest list-item-array-test
  (testing "Decode array list item"
    (let [input "- [3]: 1,2,3"
          scan-result (scanner/to-parsed-lines input)
          cursor (scanner/cursor-from-scan-result scan-result)
          [line _] (scanner/next-cursor cursor)
          [result _] (items/list-item line cursor 0 "," true)]
      (is (= [1.0 2.0 3.0] result)))))


(deftest list-item-object-test
  (testing "Decode object list item"
    (let [input "- id: 1\n  name: Alice"
          scan-result (scanner/to-parsed-lines input)
          cursor (scanner/cursor-from-scan-result scan-result)
          [line _] (scanner/next-cursor cursor)
          [result _] (items/list-item line cursor 0 "," true)]
      (is (= {"id" 1.0 "name" "Alice"} result)))))


(deftest list-item-multiline-object-test
  (testing "Decode multi-line object list item"
    (let [input "- name: Bob\n  age: 25\n  active: true"
          scan-result (scanner/to-parsed-lines input)
          cursor (scanner/cursor-from-scan-result scan-result)
          [line _] (scanner/next-cursor cursor)
          [result _] (items/list-item line cursor 0 "," true)]
      (is (= {"name" "Bob" "age" 25.0 "active" true} result)))))
