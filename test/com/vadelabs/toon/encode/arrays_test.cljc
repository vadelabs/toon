(ns com.vadelabs.toon.encode.arrays-test
  (:require
    #?(:clj [clojure.test :refer [deftest is testing]]
       :cljs [cljs.test :refer [deftest is testing]])
    [com.vadelabs.toon.encode.arrays :as arr]
    [com.vadelabs.toon.encode.writer :as writer]))


;; ============================================================================
;; Array Header Tests
;; ============================================================================

(deftest array-header-without-marker-test
  (testing "Array header"
    (is (= "[3]" (arr/array-header 3 ",")))
    (is (= "[0]" (arr/array-header 0 ",")))
    (is (= "[100]" (arr/array-header 100 ",")))))


(deftest array-header-with-delimiter-markers-test
  (testing "Array header includes delimiter marker for non-comma delimiters"
    (is (= "[3]" (arr/array-header 3 ",")))     ; Comma: no marker
    (is (= "[3|]" (arr/array-header 3 "|")))    ; Pipe: shows marker
    (is (= "[3\t]" (arr/array-header 3 "\t")))  ; Tab: shows marker
    (is (= "[3;]" (arr/array-header 3 ";")))))


;; ============================================================================
;; Common Keys Extraction Tests
;; ============================================================================

(deftest extract-common-keys-all-same-test
  (testing "Extract keys when all objects have same keys"
    (is (= ["a" "b"] (arr/extract-common-keys [{"a" 1 "b" 2}
                                               {"a" 3 "b" 4}])))
    (is (= ["x" "y" "z"] (arr/extract-common-keys [{"x" 1 "y" 2 "z" 3}
                                                   {"x" 4 "y" 5 "z" 6}])))))


(deftest extract-common-keys-subset-test
  (testing "Extract only common keys when objects have different keys"
    (is (= ["a"] (arr/extract-common-keys [{"a" 1 "b" 2}
                                           {"a" 3 "c" 4}])))
    (is (= ["x" "z"] (arr/extract-common-keys [{"x" 1 "y" 2 "z" 3}
                                               {"x" 4 "z" 5 "w" 6}])))))


(deftest extract-common-keys-no-common-test
  (testing "Returns empty when no common keys"
    (is (= [] (arr/extract-common-keys [{"a" 1}
                                        {"b" 2}])))))


(deftest extract-common-keys-empty-array-test
  (testing "Returns nil for empty array"
    (is (nil? (arr/extract-common-keys [])))))


(deftest extract-common-keys-preserves-order-test
  (testing "Preserves key order from first object"
    (is (= ["c" "b" "a"] (arr/extract-common-keys [{"c" 1 "b" 2 "a" 3}
                                                   {"a" 4 "b" 5 "c" 6}])))))


;; ============================================================================
;; Inline Array Encoding Tests
;; ============================================================================

(deftest encode-inline-array-numbers-test
  (testing "Encode array of numbers at root (depth 0)"
    (let [w (arr/inline [1 2 3] "," 0 (writer/create))]
      (is (= "[3]: 1,2,3" (writer/to-string w)))))

  (testing "Encode array of floats at nested level"
    (let [w (arr/inline [1.5 2.5 3.5] "," 1 (writer/create))]
      (is (= "  1.5,2.5,3.5" (writer/to-string w))))))


(deftest encode-inline-array-strings-test
  (testing "Encode array of simple strings at root"
    (let [w (arr/inline ["a" "b" "c"] "," 0 (writer/create))]
      (is (= "[3]: a,b,c" (writer/to-string w)))))

  (testing "Encode array with strings needing quotes at nested level"
    (let [w (arr/inline ["a" "b,c" "d"] "," 1 (writer/create))]
      (is (= "  a,\"b,c\",d" (writer/to-string w))))))


(deftest encode-inline-array-mixed-primitives-test
  (testing "Encode array of mixed primitive types at root"
    (let [w (arr/inline [1 "two" true nil] "," 0 (writer/create))]
      (is (= "[4]: 1,two,true,null" (writer/to-string w))))))


(deftest encode-inline-array-with-tab-delimiter-test
  (testing "Encode array with tab delimiter at root"
    (let [w (arr/inline ["a" "b" "c"] "\t" 0 (writer/create))]
      (is (= "[3\t]: a\tb\tc" (writer/to-string w)))))

  (testing "Tab delimiter doesn't quote commas at nested level"
    (let [w (arr/inline ["a,b" "c,d"] "\t" 1 (writer/create))]
      (is (= "  a,b\tc,d" (writer/to-string w))))))


(deftest encode-inline-array-indented-test
  (testing "Encode array with indentation (at nested level, no header)"
    (let [w (arr/inline [1 2 3] "," 1 (writer/create))]
      (is (= "  1,2,3" (writer/to-string w))))))


;; ============================================================================
;; Tabular Array Header Tests
;; ============================================================================

(deftest encode-tabular-array-header-test
  (testing "Encode tabular header"
    (let [w (arr/tabular-header 2 ["id" "name"] "," 0 (writer/create))]
      (is (= "[2]{id,name}:" (writer/to-string w))))))


(deftest encode-tabular-array-header-with-tab-delimiter-test
  (testing "Encode tabular header with tab delimiter"
    (let [w (arr/tabular-header 2 ["x" "y"] "\t" 0 (writer/create))]
      (is (= "[2\t]{x\ty}:" (writer/to-string w))))))


;; ============================================================================
;; Tabular Array Row Tests
;; ============================================================================

(deftest encode-tabular-array-row-test
  (testing "Encode single row with all keys present"
    (let [obj {"id" 1 "name" "Alice" "age" 30}
          w (arr/tabular-row obj ["id" "name"] "," 1 (writer/create))]
      (is (= "  1,Alice" (writer/to-string w)))))

  (testing "Encode row with nil values"
    (let [obj {"id" 1 "name" nil}
          w (arr/tabular-row obj ["id" "name"] "," 0 (writer/create))]
      (is (= "1,null" (writer/to-string w))))))


(deftest encode-tabular-array-row-with-quoting-test
  (testing "Encode row with values needing quotes"
    (let [obj {"id" 1 "name" "Alice, Bob"}
          w (arr/tabular-row obj ["id" "name"] "," 0 (writer/create))]
      (is (= "1,\"Alice, Bob\"" (writer/to-string w))))))


;; ============================================================================
;; Full Tabular Array Tests
;; ============================================================================

(deftest encode-tabular-array-basic-test
  (testing "Encode simple tabular array"
    (let [objects [{"id" 1 "name" "Alice"}
                   {"id" 2 "name" "Bob"}]
          w (arr/tabular objects "," 0 (writer/create))]
      (is (= "[2]{id,name}:\n  1,Alice\n  2,Bob" (writer/to-string w))))))




(deftest encode-tabular-array-subset-keys-test
  (testing "Encode tabular array with subset of common keys"
    (let [objects [{"id" 1 "name" "Alice" "extra" 99}
                   {"id" 2 "name" "Bob"}]
          w (arr/tabular objects "," 0 (writer/create))]
      ;; Should only include "id" and "name" (common keys)
      (is (= "[2]{id,name}:\n  1,Alice\n  2,Bob" (writer/to-string w))))))


(deftest encode-tabular-array-no-common-keys-test
  (testing "Tabular array with no common keys returns writer unchanged"
    (let [objects [{"a" 1} {"b" 2}]
          w (arr/tabular objects "," 0 (writer/create))]
      (is (= "" (writer/to-string w))))))


;; ============================================================================
;; Array of Arrays Tests
;; ============================================================================

(deftest encode-array-of-arrays-basic-test
  (testing "Encode simple array of arrays with list format"
    (let [arrays [[1 2] [3 4]]
          w (arr/of-arrays arrays "," 0 (writer/create))]
      (is (= "[2]:\n  - [2]: 1,2\n  - [2]: 3,4" (writer/to-string w))))))




(deftest encode-array-of-arrays-variable-lengths-test
  (testing "Encode array of arrays with variable lengths"
    (let [arrays [[1 2 3] [4 5] [6]]
          w (arr/of-arrays arrays "," 0 (writer/create))]
      (is (= "[3]:\n  - [3]: 1,2,3\n  - [2]: 4,5\n  - [1]: 6" (writer/to-string w))))))


(deftest encode-array-of-arrays-with-tab-delimiter-test
  (testing "Encode array of arrays with tab delimiter"
    (let [arrays [[1 2] [3 4]]
          w (arr/of-arrays arrays "\t" 0 (writer/create))]
      (is (= "[2\t]:\n  - [2\t]: 1\t2\n  - [2\t]: 3\t4" (writer/to-string w))))))


;; ============================================================================
;; Main encode-array Dispatch Tests
;; ============================================================================

(deftest encode-array-empty-test
  (testing "Encode empty array at root"
    (let [w (arr/encode [] "," 0 (writer/create))]
      (is (= "[0]" (writer/to-string w))))))


(deftest encode-array-primitives-test
  (testing "Encode array of primitives at root includes header"
    (let [w (arr/encode [1 2 3] "," 0 (writer/create))]
      (is (= "[3]: 1,2,3" (writer/to-string w))))))


(deftest encode-array-objects-test
  (testing "Encode array of objects dispatches to tabular"
    (let [w (arr/encode [{"a" 1} {"a" 2}] "," 0 (writer/create))]
      (is (= "[2]{a}:\n  1\n  2" (writer/to-string w))))))


(deftest encode-array-arrays-test
  (testing "Encode array of arrays dispatches to list format"
    (let [w (arr/encode [[1 2] [3 4]] "," 0 (writer/create))]
      (is (= "[2]:\n  - [2]: 1,2\n  - [2]: 3,4" (writer/to-string w))))))


(deftest encode-array-with-indentation-test
  (testing "Encode array respects depth parameter"
    (let [w (arr/encode [1 2 3] "," 2 (writer/create))]
      (is (= "    1,2,3" (writer/to-string w))))))


;; ============================================================================
;; Mixed Array Encoding Tests (with list markers)
;; ============================================================================

(deftest encode-mixed-array-primitives-test
  (testing "Encode mixed array of primitives with list markers"
    (let [test-arr [1 "text" true nil]
          w (arr/mixed test-arr "," 0 (writer/create))]
      (is (= "[4]:\n  - 1\n  - text\n  - true\n  - null" (writer/to-string w))))))


(deftest encode-mixed-array-with-nested-arrays-test
  (testing "Encode mixed array containing primitive arrays"
    (let [test-arr [[1 2] [3 4 5]]
          w (arr/mixed test-arr "," 0 (writer/create))]
      (is (= "[2]:\n  - [2]: 1,2\n  - [3]: 3,4,5" (writer/to-string w))))))


(deftest encode-mixed-array-primitives-and-arrays-test
  (testing "Encode mixed array with primitives and arrays"
    (let [test-arr [1 [2 3] "text"]
          w (arr/mixed test-arr "," 0 (writer/create))]
      (is (= "[3]:\n  - 1\n  - [2]: 2,3\n  - text" (writer/to-string w))))))




(deftest encode-mixed-array-with-tab-delimiter-test
  (testing "Encode mixed array with tab delimiter"
    (let [test-arr [1 [2 3]]
          w (arr/mixed test-arr "\t" 0 (writer/create))]
      (is (= "[2\t]:\n  - 1\n  - [2\t]: 2\t3" (writer/to-string w))))))


(deftest encode-mixed-array-dispatch-test
  (testing "encode-array dispatches to mixed encoding for mixed arrays"
    (let [test-arr [1 "text" [2 3]]
          w (arr/encode test-arr "," 0 (writer/create))]
      (is (= "[3]:\n  - 1\n  - text\n  - [2]: 2,3" (writer/to-string w))))))


;; ============================================================================
;; Object as List Item Tests
;; ============================================================================

(deftest encode-object-as-list-item-single-key-test
  (testing "Encode object with single key as list item"
    (let [obj {"a" 1}
          w (arr/object-as-list-item obj "," 1 (writer/create))]
      (is (= "  - a: 1" (writer/to-string w))))))


(deftest encode-object-as-list-item-multiple-keys-test
  (testing "Encode object with multiple keys as list item"
    (let [obj {"id" 1 "name" "Alice"}
          w (arr/object-as-list-item obj "," 1 (writer/create))
          result (writer/to-string w)]
      ;; First key on hyphen line, remaining keys indented
      (is (or (= "  - id: 1\n    name: Alice" result)
              (= "  - name: Alice\n    id: 1" result))))))


(deftest encode-object-as-list-item-empty-object-test
  (testing "Encode empty object as list item"
    (let [obj {}
          w (arr/object-as-list-item obj "," 1 (writer/create))]
      (is (= "  -" (writer/to-string w))))))


(deftest encode-mixed-array-with-objects-test
  (testing "Encode mixed array with primitives and objects"
    (let [test-arr [1 {"a" 1} "text"]
          w (arr/encode test-arr "," 0 (writer/create))
          result (writer/to-string w)]
      (is (= "[3]:\n  - 1\n  - a: 1\n  - text" result)))))


(deftest encode-mixed-array-with-multi-key-objects-test
  (testing "Encode mixed array with objects having NO common keys"
    (let [test-arr [{"name" "Alice" "age" 30} {"city" "NYC" "zip" "10001"}]
          w (arr/encode test-arr "," 0 (writer/create))
          result (writer/to-string w)]
      ;; No common keys, so uses mixed array with list markers
      ;; Each object should have first key on hyphen line, rest indented
      (is (or (= "[2]:\n  - name: Alice\n    age: 30\n  - city: NYC\n    zip: \"10001\"" result)
              (= "[2]:\n  - age: 30\n    name: Alice\n  - zip: \"10001\"\n    city: NYC" result)
              (= "[2]:\n  - name: Alice\n    age: 30\n  - zip: \"10001\"\n    city: NYC" result)
              (= "[2]:\n  - age: 30\n    name: Alice\n  - city: NYC\n    zip: \"10001\"" result))))))
