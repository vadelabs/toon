(ns com.vadelabs.toon.interface-test
  (:require
    #?(:clj [clojure.test :refer [deftest is testing]]
       :cljs [cljs.test :refer [deftest is testing]])
    [com.vadelabs.toon.constants :as const]
    ;;             [com.vadelabs.spec.interface :as spec]
    [com.vadelabs.toon.interface :as trim]))


;; ============================================================================
;; Constants Tests
;; ============================================================================

(deftest constants-test
  (testing "List markers"
    (is (= "-" const/list-item-marker))
    (is (= "- " const/list-item-prefix)))

  (testing "Delimiters"
    (is (= "," (:comma const/delimiters)))
    (is (= "\t" (:tab const/delimiters)))
    (is (= "|" (:pipe const/delimiters)))
    (is (= "," const/default-delimiter)))

  (testing "Literals"
    (is (= "null" const/null-literal))
    (is (= "true" const/true-literal))
    (is (= "false" const/false-literal)))

  (testing "Default options"
    (is (= 2 const/default-indent))))


;; ============================================================================
;; Encode Function Tests (Basic)
;; ============================================================================

(deftest encode-stub-test
  (testing "encode function exists and returns string"
    (is (string? (trim/encode {})))
    (is (string? (trim/encode [] {:indent 2})))
    (is (string? (trim/encode nil {:delimiter ","})))))


(deftest decode-nested-objects-test
  (testing "Decode nested objects"
    (is (= {"user" {"name" "Ada"}}
           (trim/decode "user:\n  name: Ada")))
    (is (= {"user" {"name" "Ada" "age" 30.0}}
           (trim/decode "user:\n  name: Ada\n  age: 30")))
    (is (= {"outer" {"inner" {"value" 42.0}}}
           (trim/decode "outer:\n  inner:\n    value: 42")))))


(deftest decode-inline-arrays-test
  (testing "Decode inline arrays"
    (is (= [1.0 2.0 3.0] (trim/decode "[3]: 1,2,3")))
    (is (= ["a" "b" "c"] (trim/decode "[3]: a,b,c")))
    (is (= [true false] (trim/decode "[2]: true,false")))
    (is (= [] (trim/decode "[0]:")))))


(deftest decode-tabular-arrays-test
  (testing "Decode tabular arrays"
    (is (= [{"id" 1.0 "name" "Alice"}
            {"id" 2.0 "name" "Bob"}]
           (trim/decode "[2]{id,name}:\n  1,Alice\n  2,Bob")))
    (is (= [{"a" 1.0 "b" 2.0 "c" 3.0}]
           (trim/decode "[1]{a,b,c}:\n  1,2,3")))))


(deftest decode-list-arrays-test
  (testing "Decode list arrays with primitives"
    (is (= ["hello" 42.0 true]
           (trim/decode "[3]:\n  - hello\n  - 42\n  - true"))))

  (testing "Decode list arrays with objects"
    (is (= [{"id" 1.0 "name" "Alice"}
            {"id" 2.0 "name" "Bob"}]
           (trim/decode "[2]:\n  - id: 1\n    name: Alice\n  - id: 2\n    name: Bob")))))


(deftest decode-objects-with-arrays-test
  (testing "Decode objects with inline arrays"
    (is (= {"tags" ["dev" "clojure"]}
           (trim/decode "tags[2]: dev,clojure"))))

  (testing "Decode objects with nested arrays"
    (is (= {"items" ["a" "b"]}
           (trim/decode "items[2]:\n  - a\n  - b")))))


(deftest decode-complex-structures-test
  (testing "Decode complex nested structure"
    (is (= {"user" {"name" "Alice"
                    "tags" ["dev" "clojure"]
                    "profile" {"age" 30.0}}}
           (trim/decode "user:\n  name: Alice\n  tags[2]: dev,clojure\n  profile:\n    age: 30")))))


(deftest decode-empty-input-test
  (testing "Decode empty string returns empty map"
    (is (= {} (trim/decode "")))))


;; ============================================================================
;; Round-trip Tests (Encode + Decode)
;; ============================================================================

(deftest roundtrip-primitives-test
  (testing "Round-trip primitives"
    (is (nil? (trim/decode (trim/encode nil))))
    (is (true? (trim/decode (trim/encode true))))
    (is (false? (trim/decode (trim/encode false))))
    (is (= 42.0 (trim/decode (trim/encode 42))))
    (is (= "hello" (trim/decode (trim/encode "hello"))))))


(deftest roundtrip-flat-objects-test
  (testing "Round-trip flat objects"
    (is (= {"name" "Ada"}
           (trim/decode (trim/encode {"name" "Ada"}))))
    (is (= {"name" "Ada" "age" 30.0}
           (trim/decode (trim/encode {"name" "Ada" "age" 30}))))))


(deftest roundtrip-nested-objects-test
  (testing "Round-trip nested objects"
    (is (= {"user" {"name" "Ada" "age" 30.0}}
           (trim/decode (trim/encode {"user" {"name" "Ada" "age" 30}}))))))


(deftest roundtrip-arrays-test
  (testing "Round-trip arrays in objects (inline)"
    (is (= {"items" [1.0 2.0 3.0]}
           (trim/decode (trim/encode {"items" [1 2 3]})))))

  (testing "Round-trip tabular arrays"
    (is (= [{"id" 1.0 "name" "Alice"}
            {"id" 2.0 "name" "Bob"}]
           (trim/decode (trim/encode [{"id" 1 "name" "Alice"}
                                      {"id" 2 "name" "Bob"}]))))))


(deftest roundtrip-complex-structures-test
  (testing "Round-trip complex nested structure"
    (let [data {"user" {"name" "Alice"
                        "tags" ["dev" "clojure"]
                        "profile" {"age" 30
                                   "active" true}}}
          encoded (trim/encode data)
          decoded (trim/decode encoded)]
      (is (= {"user" {"name" "Alice"
                      "tags" ["dev" "clojure"]
                      "profile" {"age" 30.0
                                 "active" true}}}
             decoded)))))
