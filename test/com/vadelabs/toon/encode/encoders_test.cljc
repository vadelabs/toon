(ns com.vadelabs.toon.encode.encoders-test
  (:require
    [clojure.string :as str]
    #?(:clj [clojure.test :refer [deftest is testing]]
       :cljs [cljs.test :refer [deftest is testing]])
    [com.vadelabs.toon.encode.encoders :as obj]
    [com.vadelabs.toon.encode.writer :as writer]))


;; Test options helper
(defn make-options
  ([] (make-options ","))
  ([delimiter]
   {:delimiter delimiter
    :indent 2}))


;; ============================================================================
;; Simple Key-Value Pair Tests
;; ============================================================================

(deftest encode-primitive-key-value-test
  (testing "Encode key with primitive values"
    (let [opts (make-options)
          w (obj/key-value-pair "name" "Alice" opts 0 (writer/create))]
      (is (= "name: Alice" (writer/to-string w))))

    (let [opts (make-options)
          w (obj/key-value-pair "age" 30 opts 0 (writer/create))]
      (is (= "age: 30" (writer/to-string w))))

    (let [opts (make-options)
          w (obj/key-value-pair "active" true opts 0 (writer/create))]
      (is (= "active: true" (writer/to-string w))))

    (let [opts (make-options)
          w (obj/key-value-pair "value" nil opts 0 (writer/create))]
      (is (= "value: null" (writer/to-string w))))))


(deftest encode-string-key-value-with-quoting-test
  (testing "Encode key with string value needing quotes"
    (let [opts (make-options)
          w (obj/key-value-pair "desc" "hello, world" opts 0 (writer/create))]
      (is (= "desc: \"hello, world\"" (writer/to-string w))))))


(deftest encode-key-value-with-indentation-test
  (testing "Encode key-value pair with indentation"
    (let [opts (make-options)
          w (obj/key-value-pair "name" "Alice" opts 1 (writer/create))]
      (is (= "  name: Alice" (writer/to-string w))))

    (let [opts (make-options)
          w (obj/key-value-pair "name" "Alice" opts 2 (writer/create))]
      (is (= "    name: Alice" (writer/to-string w))))))


;; ============================================================================
;; Array Value Tests
;; ============================================================================

(deftest encode-key-with-empty-array-test
  (testing "Encode key with empty array"
    (let [opts (make-options)
          w (obj/key-value-pair "tags" [] opts 0 (writer/create))]
      (is (= "tags[0]" (writer/to-string w))))))


(deftest encode-key-with-primitive-array-test
  (testing "Encode key with array of primitives"
    (let [opts (make-options)
          w (obj/key-value-pair "tags" ["a" "b" "c"] opts 0 (writer/create))]
      (is (= "tags[3]: a,b,c" (writer/to-string w)))))

  (testing "Encode key with array of numbers"
    (let [opts (make-options)
          w (obj/key-value-pair "scores" [100 95 88] opts 0 (writer/create))]
      (is (= "scores[3]: 100,95,88" (writer/to-string w))))))




(deftest encode-key-with-array-of-objects-test
  (testing "Encode key with array of objects"
    (let [opts (make-options)
          test-arr [{"id" 1 "name" "Alice"}
                    {"id" 2 "name" "Bob"}]
          w (obj/key-value-pair "users" test-arr opts 0 (writer/create))]
      ;; The key is followed by array header on same line, then tabular data
      (is (= "users[2]\n[2]{id,name}:\n  1,Alice\n  2,Bob" (writer/to-string w))))))


(deftest encode-key-with-array-of-arrays-test
  (testing "Encode key with array of arrays in list format"
    (let [opts (make-options)
          test-arr [[1 2] [3 4]]
          w (obj/key-value-pair "matrix" test-arr opts 0 (writer/create))]
      (is (= "matrix[2]:\n  - [2]: 1,2\n  - [2]: 3,4" (writer/to-string w))))))


;; ============================================================================
;; Nested Object Tests
;; ============================================================================

(deftest encode-key-with-nested-object-test
  (testing "Encode key with nested object"
    (let [opts (make-options)
          nested {"city" "NYC" "zip" "10001"}
          w (obj/key-value-pair "address" nested opts 0 (writer/create))]
      (is (= "address:\n  city: NYC\n  zip: \"10001\"" (writer/to-string w))))))  ; Numeric-like string is quoted

(deftest encode-key-with-deeply-nested-object-test
  (testing "Encode key with deeply nested object"
    (let [opts (make-options)
          nested {"user" {"name" "Alice" "age" 30}}
          w (obj/key-value-pair "data" nested opts 0 (writer/create))]
      (is (= "data:\n  user:\n    name: Alice\n    age: 30" (writer/to-string w))))))


;; ============================================================================
;; Full Object Encoding Tests
;; ============================================================================

(deftest encode-simple-object-test
  (testing "Encode simple flat object"
    (let [opts (make-options)
          obj {"name" "Alice" "age" 30}
          w (obj/object obj opts 0 (writer/create))
          result (writer/to-string w)]
      ;; Note: Map order may vary
      (is (or (= "name: Alice\nage: 30" result)
              (= "age: 30\nname: Alice" result))))))


(deftest encode-object-with-various-types-test
  (testing "Encode object with different value types"
    (let [opts (make-options)
          obj {"name" "Alice"
               "age" 30
               "active" true
               "score" nil}
          w (obj/object obj opts 0 (writer/create))
          result (writer/to-string w)]
      ;; Check that all keys are present
      (is (str/includes? result "name: Alice"))
      (is (str/includes? result "age: 30"))
      (is (str/includes? result "active: true"))
      (is (str/includes? result "score: null")))))


(deftest encode-object-with-array-values-test
  (testing "Encode object with array values"
    (let [opts (make-options)
          obj {"name" "Alice"
               "tags" ["admin" "user"]}
          w (obj/object obj opts 0 (writer/create))
          result (writer/to-string w)]
      (is (str/includes? result "name: Alice"))
      (is (str/includes? result "tags[2]: admin,user")))))


(deftest encode-nested-object-test
  (testing "Encode object with nested object"
    (let [opts (make-options)
          obj {"user" {"name" "Alice" "age" 30}}
          w (obj/object obj opts 0 (writer/create))]
      (is (= "user:\n  name: Alice\n  age: 30" (writer/to-string w))))))


(deftest encode-object-with-indentation-test
  (testing "Encode object at non-zero depth"
    (let [opts (make-options)
          obj {"name" "Alice" "age" 30}
          w (obj/object obj opts 1 (writer/create))
          result (writer/to-string w)]
      ;; All lines should be indented
      (is (or (= "  name: Alice\n  age: 30" result)
              (= "  age: 30\n  name: Alice" result))))))


;; ============================================================================
;; encode-value Dispatch Tests
;; ============================================================================

(deftest encode-value-primitive-test
  (testing "encode-value handles primitives"
    (let [opts (make-options)
          w (obj/value 42 opts 0 (writer/create))]
      (is (= "42" (writer/to-string w))))

    (let [opts (make-options)
          w (obj/value "hello" opts 0 (writer/create))]
      (is (= "hello" (writer/to-string w))))))


(deftest encode-value-array-test
  (testing "encode-value handles arrays"
    (let [opts (make-options)
          w (obj/value [1 2 3] opts 0 (writer/create))]
      (is (= "[3]: 1,2,3" (writer/to-string w))))))


(deftest encode-value-object-test
  (testing "encode-value handles objects"
    (let [opts (make-options)
          w (obj/value {"a" 1 "b" 2} opts 0 (writer/create))
          result (writer/to-string w)]
      (is (or (= "a: 1\nb: 2" result)
              (= "b: 2\na: 1" result))))))


;; ============================================================================
;; Complex Nested Structure Tests
;; ============================================================================

(deftest encode-complex-nested-structure-test
  (testing "Encode complex nested structure"
    (let [opts (make-options)
          obj {"user" {"name" "Alice"
                       "tags" ["admin" "user"]
                       "scores" [100 95 88]}
               "settings" {"notifications" true
                           "theme" "dark"}}
          w (obj/object obj opts 0 (writer/create))
          result (writer/to-string w)]
      ;; Verify structure contains expected elements
      (is (str/includes? result "user:"))
      (is (str/includes? result "name: Alice"))
      (is (str/includes? result "tags[2]: admin,user"))
      (is (str/includes? result "scores[3]: 100,95,88"))
      (is (str/includes? result "settings:"))
      (is (str/includes? result "notifications: true"))
      (is (str/includes? result "theme: dark")))))


(deftest encode-object-with-tab-delimiter-test
  (testing "Encode object with tab delimiter"
    (let [opts (make-options "\t")
          obj {"tags" ["a" "b" "c"]}
          w (obj/object obj opts 0 (writer/create))]
      (is (= "tags[3\t]: a\tb\tc" (writer/to-string w))))))
