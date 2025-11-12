(ns com.vadelabs.toon.decode.arrays-test
  "Tests for array decoding functions."
  (:require
    #?(:clj [clojure.test :refer [deftest is testing]]
       :cljs [cljs.test :refer [deftest is testing]])
    [com.vadelabs.toon.decode.arrays :as arrays]
    [com.vadelabs.toon.decode.items :as items]
    [com.vadelabs.toon.decode.parser :as parser]
    [com.vadelabs.toon.decode.scanner :as scanner]))


;; ============================================================================
;; Inline Primitive Array Tests
;; ============================================================================

(deftest inline-primitive-array-simple-test
  (testing "Decode simple inline array"
    (let [header-info {:length 3 :delimiter "," :inline-values "a,b,c"}
          result (arrays/inline-primitive-array header-info)]
      (is (= ["a" "b" "c"] result)))))


(deftest inline-primitive-array-numbers-test
  (testing "Decode inline array with numbers"
    (let [header-info {:length 3 :delimiter "," :inline-values "1,2,3"}
          result (arrays/inline-primitive-array header-info)]
      (is (= [1.0 2.0 3.0] result)))))


(deftest inline-primitive-array-mixed-test
  (testing "Decode inline array with mixed types"
    (let [header-info {:length 4 :delimiter "," :inline-values "1,hello,true,null"}
          result (arrays/inline-primitive-array header-info)]
      (is (= [1.0 "hello" true nil] result)))))


(deftest inline-primitive-array-pipe-delimiter-test
  (testing "Decode inline array with pipe delimiter"
    (let [header-info {:length 3 :delimiter "|" :inline-values "a|b|c"}
          result (arrays/inline-primitive-array header-info)]
      (is (= ["a" "b" "c"] result)))))


(deftest inline-primitive-array-empty-test
  (testing "Decode empty inline array"
    (let [header-info {:length 0 :delimiter "," :inline-values nil}
          result (arrays/inline-primitive-array header-info)]
      (is (= [] result)))))


(deftest inline-primitive-array-length-mismatch-test
  (testing "Throw on length mismatch in strict mode"
    (let [header-info {:length 2 :delimiter "," :inline-values "a,b,c"}]
      (is (thrown-with-msg?
            #?(:clj Exception :cljs js/Error)
            #"Array length mismatch"
            (arrays/inline-primitive-array header-info true))))))


;; ============================================================================
;; Tabular Array Tests
;; ============================================================================

(deftest tabular-array-simple-test
  (testing "Decode simple tabular array"
    (let [input "items[2]{id,name}:\n  1,Alice\n  2,Bob"
          scan-result (scanner/to-parsed-lines input)
          cursor (scanner/cursor-from-scan-result scan-result)
          [header-line cursor-after-header] (scanner/next-cursor cursor)
          header-info (parser/array-header-line (:content header-line))
          [result _] (arrays/tabular-array header-info cursor-after-header 1)]
      (is (= 2 (count result)))
      (is (= {"id" 1.0 "name" "Alice"} (first result)))
      (is (= {"id" 2.0 "name" "Bob"} (second result))))))


(deftest tabular-array-three-columns-test
  (testing "Decode tabular array with three columns"
    (let [input "[3]{a,b,c}:\n  1,2,3\n  4,5,6\n  7,8,9"
          scan-result (scanner/to-parsed-lines input)
          cursor (scanner/cursor-from-scan-result scan-result)
          [header-line cursor-after-header] (scanner/next-cursor cursor)
          header-info (parser/array-header-line (:content header-line))
          [result _] (arrays/tabular-array header-info cursor-after-header 1)]
      (is (= 3 (count result)))
      (is (= {"a" 1.0 "b" 2.0 "c" 3.0} (first result))))))


(deftest tabular-array-with-strings-test
  (testing "Decode tabular array with quoted strings"
    (let [input "[2]{id,desc}:\n  1,\"hello, world\"\n  2,simple"
          scan-result (scanner/to-parsed-lines input)
          cursor (scanner/cursor-from-scan-result scan-result)
          [header-line cursor-after-header] (scanner/next-cursor cursor)
          header-info (parser/array-header-line (:content header-line))
          [result _] (arrays/tabular-array header-info cursor-after-header 1)]
      (is (= 2 (count result)))
      (is (= {"id" 1.0 "desc" "hello, world"} (first result)))
      (is (= {"id" 2.0 "desc" "simple"} (second result))))))


;; ============================================================================
;; List Array Tests
;; ============================================================================

(deftest list-array-primitives-test
  (testing "Decode list array with primitive items"
    (let [input "[3]:\n  - hello\n  - 42\n  - true"
          scan-result (scanner/to-parsed-lines input)
          cursor (scanner/cursor-from-scan-result scan-result)
          [header-line cursor-after-header] (scanner/next-cursor cursor)
          header-info (parser/array-header-line (:content header-line))
          [result _] (arrays/list-array header-info cursor-after-header 1 true items/list-item)]
      (is (= ["hello" 42.0 true] result)))))


(deftest list-array-inline-arrays-test
  (testing "Decode list array with inline arrays"
    (let [input "[2]:\n  - [2]: 1,2\n  - [3]: 3,4,5"
          scan-result (scanner/to-parsed-lines input)
          cursor (scanner/cursor-from-scan-result scan-result)
          [header-line cursor-after-header] (scanner/next-cursor cursor)
          header-info (parser/array-header-line (:content header-line))
          [result _] (arrays/list-array header-info cursor-after-header 1 true items/list-item)]
      (is (= [[1.0 2.0] [3.0 4.0 5.0]] result)))))


(deftest list-array-objects-test
  (testing "Decode list array with objects"
    (let [input "[2]:\n  - id: 1\n    name: Alice\n  - id: 2\n    name: Bob"
          scan-result (scanner/to-parsed-lines input)
          cursor (scanner/cursor-from-scan-result scan-result)
          [header-line cursor-after-header] (scanner/next-cursor cursor)
          header-info (parser/array-header-line (:content header-line))
          [result _] (arrays/list-array header-info cursor-after-header 1 true items/list-item)]
      (is (= 2 (count result)))
      (is (= {"id" 1.0 "name" "Alice"} (first result)))
      (is (= {"id" 2.0 "name" "Bob"} (second result))))))


;; ============================================================================
;; Row vs Key-Value Heuristic Tests
;; ============================================================================

(deftest row-or-key-value-test
  (testing "Disambiguate data rows from key-value lines"
    (testing "delimiter before colon is a row"
      (is (= :row (#'arrays/row-or-key-value? "1,2:3" ","))))

    (testing "colon before delimiter is key-value"
      (is (= :key-value (#'arrays/row-or-key-value? "key:1,2" ","))))

    (testing "no colon is a row"
      (is (= :row (#'arrays/row-or-key-value? "1,2,3" ","))))

    (testing "no delimiter is key-value"
      (is (= :key-value (#'arrays/row-or-key-value? "key:value" ","))))))


;; ============================================================================
;; Additional Coverage Tests
;; ============================================================================

(deftest tabular-array-length-mismatch-strict-test
  (testing "Throws on length mismatch in strict mode for tabular arrays"
    (let [input "[2]{id,name}:\n  1,Alice\n  2,Bob\n  3,Charlie"
          scan-result (scanner/to-parsed-lines input)
          cursor (scanner/cursor-from-scan-result scan-result)
          [header-line cursor-after-header] (scanner/next-cursor cursor)
          header-info (parser/array-header-line (:content header-line))]
      (is (thrown-with-msg?
            #?(:clj Exception :cljs js/Error)
            #"length mismatch"
            (arrays/tabular-array header-info cursor-after-header 1 true))))))


(deftest list-array-length-mismatch-strict-test
  (testing "Throws on length mismatch in strict mode for list arrays"
    (let [input "[2]:\n  - item1\n  - item2\n  - item3"
          scan-result (scanner/to-parsed-lines input)
          cursor (scanner/cursor-from-scan-result scan-result)
          [header-line cursor-after-header] (scanner/next-cursor cursor)
          header-info (parser/array-header-line (:content header-line))]
      (is (thrown-with-msg?
            #?(:clj Exception :cljs js/Error)
            #"length mismatch"
            (arrays/list-array header-info cursor-after-header 1 true items/list-item))))))


(deftest inline-primitive-array-non-strict-length-test
  (testing "Allows length mismatch in non-strict mode"
    (let [header-info {:length 2 :delimiter "," :inline-values "a,b,c"}
          result (arrays/inline-primitive-array header-info false)]
      (is (= ["a" "b" "c"] result)))))


(deftest tabular-array-non-strict-length-test
  (testing "Allows length mismatch in non-strict mode for tabular arrays"
    (let [input "[2]{id,name}:\n  1,Alice\n  2,Bob\n  3,Charlie"
          scan-result (scanner/to-parsed-lines input)
          cursor (scanner/cursor-from-scan-result scan-result)
          [header-line cursor-after-header] (scanner/next-cursor cursor)
          header-info (parser/array-header-line (:content header-line))
          [result _] (arrays/tabular-array header-info cursor-after-header 1 false)]
      (is (= 3 (count result))))))




(deftest list-array-non-strict-length-test
  (testing "Allows length mismatch in non-strict mode for list arrays"
    (let [input "[2]:\n  - item1\n  - item2\n  - item3"
          scan-result (scanner/to-parsed-lines input)
          cursor (scanner/cursor-from-scan-result scan-result)
          [header-line cursor-after-header] (scanner/next-cursor cursor)
          header-info (parser/array-header-line (:content header-line))
          [result _] (arrays/list-array header-info cursor-after-header 1 false items/list-item)]
      (is (= 3 (count result))))))


(deftest tabular-array-empty-test
  (testing "Decode empty tabular array"
    (let [input "[0]{id,name}:"
          scan-result (scanner/to-parsed-lines input)
          cursor (scanner/cursor-from-scan-result scan-result)
          [header-line cursor-after-header] (scanner/next-cursor cursor)
          header-info (parser/array-header-line (:content header-line))
          [result _] (arrays/tabular-array header-info cursor-after-header 1 true)]
      (is (= [] result)))))


(deftest list-array-empty-test
  (testing "Decode empty list array"
    (let [input "[0]:"
          scan-result (scanner/to-parsed-lines input)
          cursor (scanner/cursor-from-scan-result scan-result)
          [header-line cursor-after-header] (scanner/next-cursor cursor)
          header-info (parser/array-header-line (:content header-line))
          [result _] (arrays/list-array header-info cursor-after-header 1 true items/list-item)]
      (is (= [] result)))))
