(ns com.vadelabs.toon.encode.lines-test
  (:require
    #?(:clj [clojure.test :refer [deftest is testing]]
       :cljs [cljs.test :refer [deftest is testing]])
    [clojure.string :as str]
    [com.vadelabs.toon.interface :as toon]))


;; ============================================================================
;; Basic encode-lines Tests
;; ============================================================================

(deftest encode-lines-no-newlines-test
  (testing "Lines yielded without newline characters"
    (let [data {"name" "Alice" "age" 30}
          lines (toon/encode-lines data)]
      (is (every? string? lines) "All elements should be strings")
      (is (not-any? #(str/includes? % "\n") lines)
          "No line should contain newline characters"))))


(deftest encode-lines-empty-object-test
  (testing "Empty object produces zero lines"
    (let [lines (toon/encode-lines {})]
      (is (= 0 (count lines)) "Empty object should produce zero lines"))))


(deftest encode-lines-iterable-test
  (testing "Result is iterable sequence"
    (let [data {"name" "Alice" "age" 30}
          lines (toon/encode-lines data)]
      (is (seqable? lines) "Result should be seqable")
      (is (= 2 (count lines)) "Should have correct number of lines")
      ;; Should be able to iterate multiple times
      (is (= (vec lines) (vec lines)) "Should be idempotent"))))


(deftest encode-lines-no-trailing-spaces-test
  (testing "No trailing spaces in any line"
    (let [data {"user" {"name" "Alice"
                        "age" 30}
                "tags" ["dev" "clojure"]}
          lines (toon/encode-lines data)]
      (is (not-any? #(str/ends-with? % " ") lines)
          "No line should end with trailing space"))))


(deftest encode-lines-correct-count-test
  (testing "Correct number of lines produced"
    (is (= 2 (count (toon/encode-lines {"name" "Alice" "age" 30}))))
    (is (= 1 (count (toon/encode-lines [1 2 3]))))
    (is (= 3 (count (toon/encode-lines [{"id" 1 "name" "Alice"}
                                        {"id" 2 "name" "Bob"}])))
        "Tabular array: header + 2 data lines")))


;; ============================================================================
;; Equivalence Tests
;; ============================================================================

(deftest encode-lines-equivalence-simple-test
  (testing "encode-lines joined equals encode for simple object"
    (let [data {"name" "Alice" "age" 30}
          from-encode (toon/encode data)
          from-lines (str/join "\n" (toon/encode-lines data))]
      (is (= from-encode from-lines)))))


(deftest encode-lines-equivalence-nested-test
  (testing "encode-lines joined equals encode for nested structures"
    (let [data {"user" {"name" "Alice"
                        "profile" {"age" 30
                                   "active" true}}}
          from-encode (toon/encode data)
          from-lines (str/join "\n" (toon/encode-lines data))]
      (is (= from-encode from-lines)))))


(deftest encode-lines-equivalence-array-test
  (testing "encode-lines joined equals encode for arrays"
    (let [data [{"id" 1 "name" "Alice"}
                {"id" 2 "name" "Bob"}]
          from-encode (toon/encode data)
          from-lines (str/join "\n" (toon/encode-lines data))]
      (is (= from-encode from-lines)))))


(deftest encode-lines-equivalence-primitives-test
  (testing "encode-lines joined equals encode for primitives"
    (is (= (toon/encode 42)
           (str/join "\n" (toon/encode-lines 42))))
    (is (= (toon/encode "hello")
           (str/join "\n" (toon/encode-lines "hello"))))
    (is (= (toon/encode true)
           (str/join "\n" (toon/encode-lines true))))
    (is (= (toon/encode nil)
           (str/join "\n" (toon/encode-lines nil))))))


;; ============================================================================
;; Options Tests
;; ============================================================================

(deftest encode-lines-with-options-test
  (testing "encode-lines respects delimiter option"
    (let [data {"tags" ["a" "b" "c"]}
          lines (toon/encode-lines data {:delimiter "\t"})
          joined (str/join "\n" lines)]
      (is (str/includes? joined "\t") "Should use tab delimiter")
      (is (= (toon/encode data {:delimiter "\t"})
             joined)
          "Should match encode with same options"))))


;; ============================================================================
;; Edge Cases
;; ============================================================================

(deftest encode-lines-single-field-test
  (testing "Single field object produces one line"
    (let [lines (toon/encode-lines {"name" "Alice"})]
      (is (= 1 (count lines)))
      (is (= "name: Alice" (first lines))))))


(deftest encode-lines-inline-array-test
  (testing "Inline array in object"
    (let [lines (toon/encode-lines {"tags" ["dev" "clojure"]})]
      (is (= 1 (count lines)))
      (is (str/starts-with? (first lines) "tags[2]:"))
      (is (str/includes? (first lines) "dev,clojure")))))


(deftest encode-lines-complex-structure-test
  (testing "Complex nested structure"
    (let [data {"user" {"name" "Alice"
                        "tags" ["dev" "clojure"]
                        "profile" {"age" 30}}}
          lines (toon/encode-lines data)
          joined (str/join "\n" lines)]
      (is (= (toon/encode data) joined))
      (is (> (count lines) 3) "Should have multiple lines for nested structure"))))
