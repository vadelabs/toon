(ns com.vadelabs.toon.integration-test
  (:require
    [clojure.string :as str]
    #?(:clj [clojure.test :refer [deftest is testing]]
       :cljs [cljs.test :refer [deftest is testing]])
    [com.vadelabs.toon.interface :as toon]))


;; ============================================================================
;; Simple Value Encoding Tests
;; ============================================================================

(deftest encode-primitive-values-test
  (testing "Encode primitive values directly"
    (is (= "42" (toon/encode 42)))
    (is (= "hello" (toon/encode "hello")))
    (is (= "true" (toon/encode true)))
    (is (= "false" (toon/encode false)))
    (is (= "null" (toon/encode nil)))))


(deftest encode-simple-array-test
  (testing "Encode simple arrays with headers at root level"
    (is (= "[3]: 1,2,3" (toon/encode [1 2 3])))
    (is (= "[3]: a,b,c" (toon/encode ["a" "b" "c"])))
    (is (= "[0]" (toon/encode [])))))


;; ============================================================================
;; Simple Object Encoding Tests
;; ============================================================================

(deftest encode-simple-object-test
  (testing "Encode simple flat object"
    (let [result (toon/encode {:name "Alice" :age 30})]
      ;; Should normalize keywords to strings and encode
      (is (or (= "name: Alice\nage: 30" result)
              (= "age: 30\nname: Alice" result))))))


(deftest encode-object-with-array-test
  (testing "Encode object with array value"
    (let [result (toon/encode {:name "Ada" :tags [:reading :gaming]})]
      (is (str/includes? result "name: Ada"))
      (is (str/includes? result "tags[2]: reading,gaming")))))


;; ============================================================================
;; Array of Objects Tests
;; ============================================================================

(deftest encode-array-of-objects-test
  (testing "Encode array of objects in tabular format"
    (let [data [{:id 1 :name "Alice"}
                {:id 2 :name "Bob"}]
          result (toon/encode data)]
      (is (= "[2]{id,name}:\n  1,Alice\n  2,Bob" result)))))


(deftest encode-array-of-objects-with-length-marker-test
  (testing "Encode array of objects with length marker"
    (let [data [{:x 10 :y 20}
                {:x 30 :y 40}]
          result (toon/encode data)]
      (is (= "[2]{x,y}:\n  10,20\n  30,40" result)))))


;; ============================================================================
;; Nested Structure Tests
;; ============================================================================

(deftest encode-nested-object-test
  (testing "Encode nested object structure"
    (let [data {:user {:name "Alice" :age 30}}
          result (toon/encode data)]
      (is (= "user:\n  name: Alice\n  age: 30" result)))))


(deftest encode-complex-nested-structure-test
  (testing "Encode complex nested structure from README example"
    (let [data {:user/profile {:name "Ada"
                               :tags #{:admin :user}
                               :scores [100 95 88]}
                :settings {:notifications true
                           :theme :dark}}
          result (toon/encode data)]
      ;; Verify key elements are present
      (is (str/includes? result "user/profile:"))
      (is (str/includes? result "name: Ada"))
      (is (str/includes? result "tags[2]: admin,user"))
      (is (str/includes? result "scores[3]: 100,95,88"))
      (is (str/includes? result "settings:"))
      (is (str/includes? result "notifications: true"))
      (is (str/includes? result "theme: dark")))))


;; ============================================================================
;; Delimiter Option Tests
;; ============================================================================

(deftest encode-with-tab-delimiter-test
  (testing "Encode with tab delimiter"
    (let [data {:tags [:a :b :c]}
          result (toon/encode data {:delimiter "\t"})]
      (is (= "tags[3\t]: a\tb\tc" result)))))


(deftest encode-with-pipe-delimiter-test
  (testing "Encode with pipe delimiter"
    (let [data {:items [1 2 3]}
          result (toon/encode data {:delimiter "|"})]
      (is (= "items[3|]: 1|2|3" result)))))


;; ============================================================================
;; Indent Option Tests
;; ============================================================================

(deftest encode-with-custom-indent-test
  (testing "Encode with 4-space indent"
    (let [data {:user {:name "Alice"}}
          result (toon/encode data {:indent 4})]
      (is (= "user:\n    name: Alice" result)))))


;; ============================================================================
;; Normalization Integration Tests
;; ============================================================================

(deftest encode-with-keyword-keys-test
  (testing "Keywords are normalized to strings"
    (let [data {:name "Alice" :age 30}
          result (toon/encode data)]
      (is (or (= "name: Alice\nage: 30" result)
              (= "age: 30\nname: Alice" result))))))


(deftest encode-with-namespaced-keywords-test
  (testing "Namespaced keywords preserve namespace"
    (let [data {:user/id 123}
          result (toon/encode data)]
      (is (= "user/id: 123" result)))))


(deftest encode-with-sets-test
  (testing "Sets are normalized to sorted vectors"
    (let [data {:tags #{:c :a :b}}
          result (toon/encode data)]
      (is (= "tags[3]: a,b,c" result)))))


;; ============================================================================
;; Quoting Integration Tests
;; ============================================================================

(deftest encode-with-delimiter-in-values-test
  (testing "Values containing delimiter are quoted"
    (let [data {:desc "hello, world"}
          result (toon/encode data)]
      (is (= "desc: \"hello, world\"" result)))))


(deftest encode-with-quotes-in-values-test
  (testing "Values containing quotes are escaped (JSON-style)"
    (let [data {:msg "say \"hi\""}
          result (toon/encode data)]
      (is (= "msg: \"say \\\"hi\\\"\"" result)))))


;; ============================================================================
;; Array of Arrays Tests
;; ============================================================================

(deftest encode-array-of-arrays-test
  (testing "Encode array of arrays with list format"
    (let [data [[1 2] [3 4]]
          result (toon/encode data)]
      (is (= "[2]:\n  - [2]: 1,2\n  - [2]: 3,4" result)))))


;; ============================================================================
;; Real-World Example Tests
;; ============================================================================

(deftest encode-readme-example-1-test
  (testing "README example 1: object with tags"
    (let [data {:name "Ada" :tags [:reading :gaming]}
          result (toon/encode data)]
      (is (str/includes? result "name: Ada"))
      (is (str/includes? result "tags[2]: reading,gaming")))))


(deftest encode-readme-example-2-test
  (testing "README example 2: array of objects"
    (let [data [{:id 1 :name "Alice"}
                {:id 2 :name "Bob"}]
          result (toon/encode data)]
      (is (= "[2]{id,name}:\n  1,Alice\n  2,Bob" result)))))


;; ============================================================================
;; Edge Cases
;; ============================================================================

(deftest encode-empty-object-test
  (testing "Encode empty object"
    (is (= "" (toon/encode {})))))


(deftest encode-empty-array-test
  (testing "Encode empty array with header at root level"
    (is (= "[0]" (toon/encode [])))))


(deftest encode-object-with-nil-values-test
  (testing "Encode object with nil values"
    (let [result (toon/encode {:name nil :age 30})]
      (is (or (= "name: null\nage: 30" result)
              (= "age: 30\nname: null" result))))))


;; ============================================================================
;; Mixed Array Tests (from TypeScript reference)
;; ============================================================================

(deftest encode-mixed-array-primitives-and-objects-test
  (testing "Mixed array with primitives and objects uses list format"
    (let [data {:items [1 {:a 1} "text"]}
          result (toon/encode data)]
      (is (= "items[3]:\n  - 1\n  - a: 1\n  - text" result)))))


(deftest encode-mixed-array-objects-and-arrays-test
  (testing "Mixed array with objects and arrays uses list format"
    (let [data {:items [{:a 1} [1 2]]}
          result (toon/encode data)]
      (is (= "items[2]:\n  - a: 1\n  - [2]: 1,2" result)))))


;; ============================================================================
;; Key Quoting Tests
;; ============================================================================

(deftest encode-key-with-spaces-test
  (testing "Keys with spaces are quoted"
    (is (= "\"user name\": Alice" (toon/encode {"user name" "Alice"})))))


(deftest encode-key-with-colon-test
  (testing "Keys with colons are quoted"
    (is (= "\"key:value\": test" (toon/encode {"key:value" "test"})))))


(deftest encode-key-starting-with-digit-test
  (testing "Keys starting with digits are quoted"
    (is (= "\"123\": value" (toon/encode {"123" "value"})))))


(deftest encode-key-with-brackets-test
  (testing "Keys with brackets are quoted"
    (is (= "\"[special]\": value" (toon/encode {"[special]" "value"})))))


(deftest encode-valid-unquoted-keys-test
  (testing "Valid keys remain unquoted"
    (let [result (toon/encode {"name" "Alice" "user_id" "123" "user.name" "Bob"})]
      (is (str/includes? result "name: Alice"))
      (is (str/includes? result "user_id: \"123\""))
      (is (str/includes? result "user.name: Bob")))))


(deftest encode-tabular-array-with-special-keys-test
  (testing "Tabular array with special character keys"
    (let [data [{"user name" "Alice" "user id" 1}
                {"user name" "Bob" "user id" 2}]
          result (toon/encode data)]
      (is (= "[2]{\"user name\",\"user id\"}:\n  Alice,1\n  Bob,2" result)))))


(deftest encode-roundtrip-with-special-keys-test
  (testing "Roundtrip with special character keys"
    (let [data {"user name" "Alice" "key:value" "test" "123" "numeric"}
          encoded (toon/encode data)
          decoded (toon/decode encoded)]
      (is (= data decoded)))))


;; ============================================================================
;; Leading Zero Value Tests
;; ============================================================================

(deftest encode-value-leading-zeros-test
  (testing "Values with leading zeros are quoted"
    (is (= "value: \"05\"" (toon/encode {"value" "05"})))
    (is (= "value: \"007\"" (toon/encode {"value" "007"})))))
