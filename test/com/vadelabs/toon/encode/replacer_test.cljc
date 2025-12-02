(ns com.vadelabs.toon.encode.replacer-test
  (:require
    #?(:clj [clojure.test :refer [deftest is testing]]
       :cljs [cljs.test :refer [deftest is testing]])
    [clojure.string :as str]
    [com.vadelabs.toon.core :as toon]
    [com.vadelabs.toon.encode.replacer :as replacer]))


;; ============================================================================
;; Basic Replacer Tests
;; ============================================================================

(deftest apply-replacer-identity-test
  (testing "Replacer that returns value unchanged"
    (let [data {"name" "Alice" "age" 30}
          result (replacer/apply-replacer data (fn [_ v _] v))]
      (is (= data result)))))


(deftest apply-replacer-omit-property-test
  (testing "Replacer that omits specific properties"
    (let [data {"name" "Alice" "password" "secret" "email" "alice@test.com"}
          result (replacer/apply-replacer data (fn [k v _]
                                                 (when-not (= k "password") v)))]
      (is (= {"name" "Alice" "email" "alice@test.com"} result)))))


(deftest apply-replacer-transform-values-test
  (testing "Replacer that transforms string values to uppercase"
    (let [data {"name" "alice" "status" "active"}
          result (replacer/apply-replacer data (fn [_ v _]
                                                 (if (string? v)
                                                   (str/upper-case v)
                                                   v)))]
      (is (= {"name" "ALICE" "status" "ACTIVE"} result)))))


(deftest apply-replacer-root-unchanged-on-nil-test
  (testing "Root returning nil means no change (not omit)"
    (let [data {"name" "Alice"}
          result (replacer/apply-replacer data (fn [k v _]
                                                 (if (= k "")
                                                   nil  ; nil for root = no change
                                                   v)))]
      (is (= data result)))))


;; ============================================================================
;; Path Tracking Tests
;; ============================================================================

(deftest apply-replacer-path-tracking-test
  (testing "Replacer receives correct path for nested values"
    (let [paths (atom [])
          data {"user" {"name" "Alice" "address" {"city" "NYC"}}}
          _ (replacer/apply-replacer data (fn [k v path]
                                            (swap! paths conj {:key k :path path})
                                            v))]
      ;; Root is called with key="" and path=[]
      (is (some #(= {:key "" :path []} %) @paths))
      ;; First level
      (is (some #(= {:key "user" :path ["user"]} %) @paths))
      ;; Second level
      (is (some #(= {:key "name" :path ["user" "name"]} %) @paths))
      (is (some #(= {:key "address" :path ["user" "address"]} %) @paths))
      ;; Third level
      (is (some #(= {:key "city" :path ["user" "address" "city"]} %) @paths)))))


(deftest apply-replacer-array-index-as-string-test
  (testing "Array elements receive string index as key"
    (let [keys-received (atom [])
          data {"items" ["a" "b" "c"]}
          _ (replacer/apply-replacer data (fn [k v _]
                                            (swap! keys-received conj k)
                                            v))]
      (is (some #(= "0" %) @keys-received))
      (is (some #(= "1" %) @keys-received))
      (is (some #(= "2" %) @keys-received)))))


;; ============================================================================
;; Array Filtering Tests
;; ============================================================================

(deftest apply-replacer-omit-array-elements-test
  (testing "Replacer can omit array elements"
    (let [data {"values" [1 2 3 4 5]}
          result (replacer/apply-replacer data (fn [k v _]
                                                 ;; Omit even numbers
                                                 (if (and (number? v) (even? v))
                                                   nil
                                                   v)))]
      (is (= {"values" [1 3 5]} result)))))


(deftest apply-replacer-transform-array-elements-test
  (testing "Replacer can transform array elements"
    (let [data {"values" [1 2 3]}
          result (replacer/apply-replacer data (fn [_ v _]
                                                 (if (number? v)
                                                   (* v 2)
                                                   v)))]
      (is (= {"values" [2 4 6]} result)))))


;; ============================================================================
;; Nested Structure Tests
;; ============================================================================

(deftest apply-replacer-deeply-nested-test
  (testing "Replacer works on deeply nested structures"
    (let [data {"a" {"b" {"c" {"d" "value"}}}}
          result (replacer/apply-replacer data (fn [k v _]
                                                 (if (= k "d")
                                                   "transformed"
                                                   v)))]
      (is (= {"a" {"b" {"c" {"d" "transformed"}}}} result)))))


(deftest apply-replacer-mixed-structure-test
  (testing "Replacer works on mixed structures (objects and arrays)"
    (let [data {"users" [{"name" "Alice" "active" true}
                         {"name" "Bob" "active" false}]}
          result (replacer/apply-replacer data (fn [k v _]
                                                 ;; Remove inactive users
                                                 (if (and (map? v) (false? (get v "active")))
                                                   nil
                                                   v)))]
      (is (= {"users" [{"name" "Alice" "active" true}]} result)))))


;; ============================================================================
;; Integration with encode Tests
;; ============================================================================

(deftest encode-with-replacer-filter-test
  (testing "encode with replacer to filter sensitive fields"
    (let [data {:name "Alice" :password "secret123" :email "alice@test.com"}
          result (toon/encode data {:replacer (fn [k v _]
                                                (when-not (= k "password") v))})]
      (is (str/includes? result "name: Alice"))
      (is (str/includes? result "email: alice@test.com"))
      (is (not (str/includes? result "password")))
      (is (not (str/includes? result "secret123"))))))


(deftest encode-with-replacer-transform-test
  (testing "encode with replacer to transform values"
    (let [data {:status "active" :count 5}
          result (toon/encode data {:replacer (fn [_ v _]
                                                (if (string? v)
                                                  (str/upper-case v)
                                                  v))})]
      (is (str/includes? result "status: ACTIVE"))
      (is (str/includes? result "count: 5")))))


(deftest encode-lines-with-replacer-test
  (testing "encode-lines also supports replacer"
    (let [data {:name "Alice" :secret "hidden"}
          lines (toon/encode-lines data {:replacer (fn [k v _]
                                                     (when-not (= k "secret") v))})]
      (is (some #(str/includes? % "name: Alice") lines))
      (is (not (some #(str/includes? % "secret") lines))))))


(deftest encode-with-replacer-add-timestamp-test
  (testing "encode with replacer to add field to root object"
    (let [data {:name "Alice"}
          result (toon/encode data {:replacer (fn [k v path]
                                                (if (empty? path)
                                                  (assoc v "added" "yes")
                                                  v))})]
      (is (str/includes? result "name: Alice"))
      (is (str/includes? result "added: yes")))))


(deftest encode-with-replacer-array-filter-test
  (testing "encode with replacer to filter array elements"
    (let [data {:values [1 2 3 4 5]}
          result (toon/encode data {:replacer (fn [_ v _]
                                                (if (and (number? v) (> v 3))
                                                  nil
                                                  v))})]
      ;; Should only have values 1, 2, 3
      (is (str/includes? result "values[3]: 1,2,3")))))
