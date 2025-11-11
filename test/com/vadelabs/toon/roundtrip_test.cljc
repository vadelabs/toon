(ns com.vadelabs.toon.roundtrip-test
  "Property-based roundtrip tests for TOON encoding/decoding."
  (:require
    #?(:clj [clojure.test :refer [deftest is testing]]
       :cljs [cljs.test :refer [deftest is testing]])
    [clojure.test.check.clojure-test :refer [defspec]]
    [clojure.test.check.generators :as gen]
    [clojure.test.check.properties :as prop]
    [com.vadelabs.toon.interface :as toon]))


;; ============================================================================
;; Helper Functions
;; ============================================================================

(defn- normalize-for-comparison
  "Normalizes values for roundtrip comparison.

  TOON normalization:
  - All numbers become doubles
  - All map keys become strings (keywords → strings)
  - Sets become sorted vectors"
  [value]
  (cond
    (number? value)
    (double value)

    (map? value)
    (into {} (map (fn [[k v]]
                    [(if (keyword? k) (name k) (str k))
                     (normalize-for-comparison v)])
                  value))

    (vector? value)
    (mapv normalize-for-comparison value)

    :else
    value))


(defn- roundtrip
  "Encodes and decodes a value, returning the result."
  [value]
  (-> value
      (toon/encode)
      (toon/decode)))


;; ============================================================================
;; Custom Generators
;; ============================================================================

(def gen-non-empty-string
  "Generator for non-empty strings"
  (gen/such-that #(not (empty? %)) gen/string-alphanumeric 100))


(def gen-json-primitive
  "Generator for JSON primitive values"
  (gen/one-of [gen/string-alphanumeric
               gen/int
               gen/boolean
               (gen/return nil)]))


(def gen-simple-map
  "Generator for simple string-keyed maps with primitive values"
  (gen/map gen-non-empty-string gen-json-primitive {:min-elements 1 :max-elements 5}))


(def gen-string-vector
  "Generator for non-empty vectors of strings"
  (gen/vector gen-non-empty-string 1 10))


(def gen-int-vector
  "Generator for non-empty vectors of integers"
  (gen/vector gen/int 1 10))


(def gen-bool-vector
  "Generator for non-empty vectors of booleans"
  (gen/vector gen/boolean 1 5))


;; ============================================================================
;; Primitive Roundtrip Tests
;; ============================================================================

(defspec primitive-string-roundtrip 20
  (prop/for-all [s gen/string-alphanumeric]
                (= s (roundtrip s))))


(defspec primitive-boolean-roundtrip 10
  (prop/for-all [b gen/boolean]
                (= b (roundtrip b))))


(deftest primitive-nil-roundtrip
  (testing "nil roundtrips correctly"
    (is (= nil (roundtrip nil)))))


(defspec primitive-number-roundtrip 20
  (prop/for-all [n gen/int]
                (= (double n) (roundtrip n))))


;; ============================================================================
;; Array Roundtrip Tests
;; ============================================================================

(defspec array-string-roundtrip 20
  (prop/for-all [arr (gen/vector gen-non-empty-string 1 10)]
                (= arr (roundtrip arr))))


(defspec array-number-roundtrip 20
  (prop/for-all [arr (gen/vector gen/int 1 10)]
                (let [expected (mapv double arr)
                      actual (roundtrip arr)]
                  (= expected actual))))


(defspec array-boolean-roundtrip 10
  (prop/for-all [arr (gen/vector gen/boolean 1 5)]
                (= arr (roundtrip arr))))


(deftest array-empty-roundtrip
  (testing "Empty arrays roundtrip correctly"
    (is (= [] (roundtrip [])))))


;; ============================================================================
;; Object Roundtrip Tests
;; ============================================================================

(defspec object-simple-roundtrip 20
  (prop/for-all [obj (gen/map gen-non-empty-string gen-non-empty-string
                              {:min-elements 1 :max-elements 5})]
                (= obj (roundtrip obj))))


(defspec object-mixed-values-roundtrip 20
  (prop/for-all [name gen-non-empty-string
                 age gen/int
                 active gen/boolean]
                (let [obj {"name" name "age" age "active" active}
                      expected (normalize-for-comparison obj)
                      actual (roundtrip obj)]
                  (= expected actual))))


;; ============================================================================
;; Nested Structure Roundtrip Tests
;; ============================================================================

(defspec nested-object-roundtrip 20
  (prop/for-all [name gen-non-empty-string
                 age gen/int]
                (let [obj {"user" {"name" name "age" age}}
                      expected (normalize-for-comparison obj)
                      actual (roundtrip obj)]
                  (= expected actual))))


(defspec object-with-array-roundtrip 20
  (prop/for-all [name gen-non-empty-string
                 tags gen-string-vector]
                (let [obj {"name" name "tags" tags}
                      expected (normalize-for-comparison obj)
                      actual (roundtrip obj)]
                  (= expected actual))))


(defspec array-of-objects-roundtrip 20
  (prop/for-all [objects (gen/vector
                           (gen/fmap (fn [[id name]] {"id" id "name" name})
                                     (gen/tuple gen/int gen-non-empty-string))
                           1 5)]
                (let [expected (normalize-for-comparison objects)
                      actual (roundtrip objects)]
                  (= expected actual))))


;; ============================================================================
;; Edge Cases
;; ============================================================================

(deftest edge-cases-roundtrip-test
  (testing "Edge cases roundtrip correctly"
    (testing "empty objects"
      (is (= {} (roundtrip {}))))

    (testing "objects with nil values"
      (is (= {"a" nil "b" 1.0}
             (roundtrip {"a" nil "b" 1}))))

    (testing "strings with special characters"
      (let [special-strings ["hello, world"
                             "say \"hi\""
                             "line1\nline2"
                             "tab\there"
                             "backslash\\here"]]
        (doseq [s special-strings]
          (is (= s (roundtrip s))))))

    (testing "strings requiring quotes"
      (doseq [s ["a,b" "x:y" "foo\nbar" "\"quoted\""]]
        (is (= s (roundtrip s)))))))


;; ============================================================================
;; Complex JSON-Compatible Structure Test
;; ============================================================================

(def gen-complex-value
  "Generator for complex but safe JSON values (avoiding nested arrays)"
  (gen/one-of [gen-non-empty-string
               gen/int
               gen/boolean
               (gen/return nil)
               gen-simple-map]))


(defspec complex-json-roundtrip 30
  (prop/for-all [value gen-complex-value]
                (let [expected (normalize-for-comparison value)
                      actual (roundtrip value)]
                  (= expected actual))))


;; ============================================================================
;; Enhanced Generators for Comprehensive Testing
;; ============================================================================

(def gen-special-string
  "Generator for strings requiring quoting"
  (gen/one-of [(gen/return "true")
               (gen/return "false")
               (gen/return "null")
               (gen/return "42")
               (gen/return "3.14")
               (gen/return "007")
               (gen/return "a,b")
               (gen/return "key:value")
               (gen/return "say \"hi\"")
               (gen/return "line1\nline2")
               (gen/return "tab\there")
               (gen/return "back\\slash")
               (gen/return " leading")
               (gen/return "trailing ")
               (gen/return "[array]")
               (gen/return "{object}")
               (gen/return "- item")]))


(def gen-unicode-string
  "Generator for unicode and emoji strings"
  (gen/one-of [(gen/return "café")
               (gen/return "你好")
               (gen/return "🚀")
               (gen/return "héllo wörld")
               (gen/return "テスト")]))


(def gen-safe-key
  "Generator for safe object keys"
  (gen/one-of [gen-non-empty-string
               (gen/fmap #(str "key" %) gen/nat)]))


(def gen-nested-array
  "Generator for arrays of arrays"
  (gen/vector (gen/vector gen/int 0 5) 1 5))


;; ============================================================================
;; Deep Nesting Tests
;; ============================================================================

(defspec deeply-nested-objects-roundtrip 50
  (prop/for-all [name gen-non-empty-string
                 age gen/int
                 city gen-non-empty-string]
                (let [obj {"level1" {"level2" {"level3" {"name" name "age" age "city" city}}}}
                      expected (normalize-for-comparison obj)
                      actual (roundtrip obj)]
                  (= expected actual))))


(defspec nested-arrays-roundtrip 50
  (prop/for-all [arr gen-nested-array]
                (let [expected (mapv #(mapv double %) arr)
                      actual (roundtrip arr)]
                  (= expected actual))))


;; ============================================================================
;; Special String Cases
;; ============================================================================

(defspec special-strings-roundtrip 50
  (prop/for-all [s gen-special-string]
                (= s (roundtrip s))))


(defspec unicode-strings-roundtrip 50
  (prop/for-all [s gen-unicode-string]
                (= s (roundtrip s))))


(defspec strings-with-whitespace-roundtrip 50
  (prop/for-all [ws1 (gen/elements [" " "  " "\t"])
                 content gen-non-empty-string
                 ws2 (gen/elements [" " "  " "\t"])]
                (let [s (str ws1 content ws2)]
                  (= s (roundtrip s)))))


;; ============================================================================
;; Different Delimiter Options
;; ============================================================================

(defspec roundtrip-with-tab-delimiter 50
  (prop/for-all [obj gen-simple-map]
                (let [encoded (toon/encode obj {:delimiter "\t"})
                      decoded (toon/decode encoded)
                      expected (normalize-for-comparison obj)]
                  (= expected decoded))))


(defspec roundtrip-with-pipe-delimiter 50
  (prop/for-all [obj gen-simple-map]
                (let [encoded (toon/encode obj {:delimiter "|"})
                      decoded (toon/decode encoded)
                      expected (normalize-for-comparison obj)]
                  (= expected decoded))))


;; ============================================================================
;; Length Marker Options
;; ============================================================================



;; ============================================================================
;; Large Structure Tests
;; ============================================================================

(defspec large-object-roundtrip 30
  (prop/for-all [obj (gen/map gen-safe-key gen-json-primitive
                              {:min-elements 10 :max-elements 50})]
                (let [expected (normalize-for-comparison obj)
                      actual (roundtrip obj)]
                  (= expected actual))))


(defspec large-array-roundtrip 30
  (prop/for-all [arr (gen/vector gen/int 50 200)]
                (let [expected (mapv double arr)
                      actual (roundtrip arr)]
                  (= expected actual))))


(defspec large-array-of-objects-roundtrip 30
  (prop/for-all [objects (gen/vector
                           (gen/fmap (fn [[id name]]
                                       {"id" id "name" name})
                                     (gen/tuple gen/int gen-non-empty-string))
                           10 50)]
                (let [expected (normalize-for-comparison objects)
                      actual (roundtrip objects)]
                  (= expected actual))))


;; ============================================================================
;; Edge Cases for Keys
;; ============================================================================

(defspec keys-with-special-chars-roundtrip 50
  (prop/for-all [value gen-non-empty-string]
                (let [obj {"user name" value
                           "key:value" value
                           "123" value
                           "$special" value}
                      expected (normalize-for-comparison obj)
                      actual (roundtrip obj)]
                  (= expected actual))))


(defspec namespaced-keys-roundtrip 50
  (prop/for-all [value gen-non-empty-string]
                (let [obj {"user/name" value
                           "app.config/port" value
                           "ns.core/value" value}
                      expected (normalize-for-comparison obj)
                      actual (roundtrip obj)]
                  (= expected actual))))
