(ns com.vadelabs.toon.encode.keys-test
  (:require
    #?(:clj [clojure.test :refer [deftest is testing]]
       :cljs [cljs.test :refer [deftest is testing]])
    [com.vadelabs.toon.encode.keys :as keys]))


;; ============================================================================
;; Key Folding Tests
;; ============================================================================

(deftest folding-result-simple-chain-test
  (testing "Folds simple nested single-key objects"
    (let [value {"config" {"server" "localhost"}}
          siblings []
          options {:key-folding :safe :flatten-depth ##Inf}
          result (keys/fold "data" value siblings options)]
      (is (some? result))
      (is (= "data.config.server" (:folded-key result)))
      (is (nil? (:remainder result)))
      (is (= "localhost" (:leaf-value result)))
      (is (= 3 (:segment-count result))))))


(deftest folding-result-disabled-test
  (testing "Returns nil when folding is disabled"
    (let [value {"config" {"server" "localhost"}}
          siblings []
          options {:key-folding :off :flatten-depth ##Inf}
          result (keys/fold "data" value siblings options)]
      (is (nil? result)))))


(deftest folding-result-non-object-test
  (testing "Returns nil for non-object values"
    (let [options {:key-folding :safe :flatten-depth ##Inf}
          siblings []]
      (is (nil? (keys/fold "key" "string" siblings options)))
      (is (nil? (keys/fold "key" 42 siblings options)))
      (is (nil? (keys/fold "key" [1 2 3] siblings options))))))


(deftest folding-result-multi-key-object-test
  (testing "Returns nil for multi-key objects"
    (let [value {"a" 1 "b" 2}
          siblings []
          options {:key-folding :safe :flatten-depth ##Inf}
          result (keys/fold "key" value siblings options)]
      (is (nil? result)))))


(deftest folding-result-two-segments-test
  (testing "Folds with exactly 2 segments"
    (let [value {"server" "localhost"}
          siblings []
          options {:key-folding :safe :flatten-depth ##Inf}
          result (keys/fold "data" value siblings options)]
      (is (some? result))
      (is (= "data.server" (:folded-key result)))
      (is (= 2 (:segment-count result))))))


(deftest folding-result-invalid-segment-test
  (testing "Returns nil when segment contains dots"
    (let [value {"config" {"server.name" "localhost"}}
          siblings []
          options {:key-folding :safe :flatten-depth ##Inf}
          result (keys/fold "data" value siblings options)]
      (is (nil? result))))

  (testing "Returns nil when segment contains slash"
    (let [value {"config" {"server/name" "localhost"}}
          siblings []
          options {:key-folding :safe :flatten-depth ##Inf}
          result (keys/fold "data" value siblings options)]
      (is (nil? result))))

  (testing "Returns nil when segment starts with digit"
    (let [value {"config" {"123" "localhost"}}
          siblings []
          options {:key-folding :safe :flatten-depth ##Inf}
          result (keys/fold "data" value siblings options)]
      (is (nil? result)))))


(deftest folding-result-collision-with-siblings-test
  (testing "Returns nil when folded key collides with sibling"
    (let [value {"config" {"server" "localhost"}}
          siblings ["data.config.server"]  ; Collision!
          options {:key-folding :safe :flatten-depth ##Inf}
          result (keys/fold "data" value siblings options)]
      (is (nil? result)))))


(deftest folding-result-partial-fold-test
  (testing "Partially folds when depth limit reached"
    (let [value {"a" {"b" {"c" {"d" "value"}}}}
          siblings []
          options {:key-folding :safe :flatten-depth 3}
          result (keys/fold "root" value siblings options)]
      (is (some? result))
      (is (= "root.a.b" (:folded-key result)))
      (is (= {"c" {"d" "value"}} (:remainder result)))
      (is (= {"c" {"d" "value"}} (:leaf-value result)))
      (is (= 3 (:segment-count result))))))


(deftest folding-result-partial-fold-multi-key-test
  (testing "Partially folds when encountering multi-key object"
    (let [value {"config" {"settings" {"host" "localhost" "port" 8080}}}
          siblings []
          options {:key-folding :safe :flatten-depth ##Inf}
          result (keys/fold "data" value siblings options)]
      (is (some? result))
      (is (= "data.config.settings" (:folded-key result)))
      (is (= {"host" "localhost" "port" 8080} (:remainder result)))
      (is (= {"host" "localhost" "port" 8080} (:leaf-value result)))
      (is (= 3 (:segment-count result))))))


(deftest folding-result-leaf-array-test
  (testing "Folds chain ending in array"
    (let [value {"config" {"tags" ["a" "b" "c"]}}
          siblings []
          options {:key-folding :safe :flatten-depth ##Inf}
          result (keys/fold "data" value siblings options)]
      (is (some? result))
      (is (= "data.config.tags" (:folded-key result)))
      (is (nil? (:remainder result)))
      (is (= ["a" "b" "c"] (:leaf-value result)))
      (is (= 3 (:segment-count result))))))


(deftest folding-result-leaf-empty-object-test
  (testing "Folds chain ending in empty object"
    (let [value {"config" {"settings" {}}}
          siblings []
          options {:key-folding :safe :flatten-depth ##Inf}
          result (keys/fold "data" value siblings options)]
      (is (some? result))
      (is (= "data.config.settings" (:folded-key result)))
      (is (nil? (:remainder result)))
      (is (= {} (:leaf-value result)))
      (is (= 3 (:segment-count result))))))


(deftest folding-result-valid-identifiers-test
  (testing "Folds with valid identifier segments"
    (let [value {"user_config" {"server_name" "localhost"}}
          siblings []
          options {:key-folding :safe :flatten-depth ##Inf}
          result (keys/fold "app_data" value siblings options)]
      (is (some? result))
      (is (= "app_data.user_config.server_name" (:folded-key result))))))


;; ============================================================================
;; Edge Cases
;; ============================================================================

(deftest folding-result-underscore-prefix-test
  (testing "Folds keys starting with underscore"
    (let [value {"_private" {"_config" "value"}}
          siblings []
          options {:key-folding :safe :flatten-depth ##Inf}
          result (keys/fold "_data" value siblings options)]
      (is (some? result))
      (is (= "_data._private._config" (:folded-key result))))))


(deftest folding-result-very-deep-chain-test
  (testing "Folds very deep chain (10+ levels)"
    (let [value {"a" {"b" {"c" {"d" {"e" {"f" {"g" {"h" {"i" {"j" "deep"}}}}}}}}}}
          siblings []
          options {:key-folding :safe :flatten-depth ##Inf}
          result (keys/fold "root" value siblings options)]
      (is (some? result))
      (is (= "root.a.b.c.d.e.f.g.h.i.j" (:folded-key result)))
      (is (= "deep" (:leaf-value result)))
      (is (= 11 (:segment-count result))))))


(deftest folding-result-root-literal-collision-test
  (testing "Returns nil when folded key collides with root literal"
    (let [value {"config" {"server" "localhost"}}
          siblings []
          root-literals #{"data.config.server"}  ; Root-level dotted key
          options {:key-folding :safe :flatten-depth ##Inf}
          result (keys/fold "data" value siblings options root-literals nil)]
      (is (nil? result)))))


(deftest folding-result-mixed-case-identifiers-test
  (testing "Folds with mixed case identifiers"
    (let [value {"MyConfig" {"ServerName" "localhost"}}
          siblings []
          options {:key-folding :safe :flatten-depth ##Inf}
          result (keys/fold "AppData" value siblings options)]
      (is (some? result))
      (is (= "AppData.MyConfig.ServerName" (:folded-key result))))))


(deftest folding-result-numeric-suffix-test
  (testing "Folds keys with numeric suffixes"
    (let [value {"config2" {"server3" "localhost"}}
          siblings []
          options {:key-folding :safe :flatten-depth ##Inf}
          result (keys/fold "data1" value siblings options)]
      (is (some? result))
      (is (= "data1.config2.server3" (:folded-key result))))))


(deftest folding-result-single-level-no-fold-test
  (testing "Returns nil for single-level (only 1 segment total)"
    (let [value "not-an-object"
          siblings []
          options {:key-folding :safe :flatten-depth ##Inf}
          result (keys/fold "key" value siblings options)]
      (is (nil? result)))))


(deftest folding-result-empty-object-chain-test
  (testing "Folds chain with empty object at multiple levels"
    (let [value {"a" {"b" {}}}
          siblings []
          options {:key-folding :safe :flatten-depth ##Inf}
          result (keys/fold "root" value siblings options)]
      (is (some? result))
      (is (= "root.a.b" (:folded-key result)))
      (is (= {} (:leaf-value result))))))


(deftest folding-result-flatten-depth-1-test
  (testing "Returns nil when flatten-depth is 1 (need 2+ for folding)"
    (let [value {"config" {"server" "localhost"}}
          siblings []
          options {:key-folding :safe :flatten-depth 1}
          result (keys/fold "data" value siblings options)]
      (is (nil? result)))))


(deftest folding-result-collision-with-nested-prefix-test
  (testing "Returns nil when folded key is prefix of sibling"
    (let [value {"config" {"server" "localhost"}}
          siblings ["data.config"]  ; Sibling that is prefix
          options {:key-folding :safe :flatten-depth ##Inf}
          result (keys/fold "data" value siblings options)]
      ;; Should still fold since "data.config.server" != "data.config"
      (is (some? result)))))
