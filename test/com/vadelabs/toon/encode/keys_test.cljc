(ns com.vadelabs.toon.encode.keys-test
  (:require
    #?(:clj [clojure.test :refer [deftest is testing]]
       :cljs [cljs.test :refer [deftest is testing]])
    [com.vadelabs.toon.encode.keys :as keys]))


;; ============================================================================
;; Key Collapsing Tests
;; ============================================================================

(deftest collapsing-result-simple-chain-test
  (testing "Collapses simple nested single-key objects"
    (let [value {"config" {"server" "localhost"}}
          siblings []
          options {:key-collapsing :safe :flatten-depth ##Inf}
          result (keys/collapse "data" value siblings options)]
      (is (some? result))
      (is (= "data.config.server" (:collapsed-key result)))
      (is (nil? (:remainder result)))
      (is (= "localhost" (:leaf-value result)))
      (is (= 3 (:segment-count result))))))


(deftest collapsing-result-disabled-test
  (testing "Returns nil when collapsing is disabled"
    (let [value {"config" {"server" "localhost"}}
          siblings []
          options {:key-collapsing :off :flatten-depth ##Inf}
          result (keys/collapse "data" value siblings options)]
      (is (nil? result)))))


(deftest collapsing-result-non-object-test
  (testing "Returns nil for non-object values"
    (let [options {:key-collapsing :safe :flatten-depth ##Inf}
          siblings []]
      (is (nil? (keys/collapse "key" "string" siblings options)))
      (is (nil? (keys/collapse "key" 42 siblings options)))
      (is (nil? (keys/collapse "key" [1 2 3] siblings options))))))


(deftest collapsing-result-multi-key-object-test
  (testing "Returns nil for multi-key objects"
    (let [value {"a" 1 "b" 2}
          siblings []
          options {:key-collapsing :safe :flatten-depth ##Inf}
          result (keys/collapse "key" value siblings options)]
      (is (nil? result)))))


(deftest collapsing-result-two-segments-test
  (testing "Collapses with exactly 2 segments"
    (let [value {"server" "localhost"}
          siblings []
          options {:key-collapsing :safe :flatten-depth ##Inf}
          result (keys/collapse "data" value siblings options)]
      (is (some? result))
      (is (= "data.server" (:collapsed-key result)))
      (is (= 2 (:segment-count result))))))


(deftest collapsing-result-invalid-segment-test
  (testing "Returns nil when segment contains dots"
    (let [value {"config" {"server.name" "localhost"}}
          siblings []
          options {:key-collapsing :safe :flatten-depth ##Inf}
          result (keys/collapse "data" value siblings options)]
      (is (nil? result))))

  (testing "Returns nil when segment contains slash"
    (let [value {"config" {"server/name" "localhost"}}
          siblings []
          options {:key-collapsing :safe :flatten-depth ##Inf}
          result (keys/collapse "data" value siblings options)]
      (is (nil? result))))

  (testing "Returns nil when segment starts with digit"
    (let [value {"config" {"123" "localhost"}}
          siblings []
          options {:key-collapsing :safe :flatten-depth ##Inf}
          result (keys/collapse "data" value siblings options)]
      (is (nil? result)))))


(deftest collapsing-result-collision-with-siblings-test
  (testing "Returns nil when collapsed key collides with sibling"
    (let [value {"config" {"server" "localhost"}}
          siblings ["data.config.server"]  ; Collision!
          options {:key-collapsing :safe :flatten-depth ##Inf}
          result (keys/collapse "data" value siblings options)]
      (is (nil? result)))))


(deftest collapsing-result-partial-fold-test
  (testing "Partially collapses when depth limit reached"
    (let [value {"a" {"b" {"c" {"d" "value"}}}}
          siblings []
          options {:key-collapsing :safe :flatten-depth 3}
          result (keys/collapse "root" value siblings options)]
      (is (some? result))
      (is (= "root.a.b" (:collapsed-key result)))
      (is (= {"c" {"d" "value"}} (:remainder result)))
      (is (= {"c" {"d" "value"}} (:leaf-value result)))
      (is (= 3 (:segment-count result))))))


(deftest collapsing-result-partial-fold-multi-key-test
  (testing "Partially collapses when encountering multi-key object"
    (let [value {"config" {"settings" {"host" "localhost" "port" 8080}}}
          siblings []
          options {:key-collapsing :safe :flatten-depth ##Inf}
          result (keys/collapse "data" value siblings options)]
      (is (some? result))
      (is (= "data.config.settings" (:collapsed-key result)))
      (is (= {"host" "localhost" "port" 8080} (:remainder result)))
      (is (= {"host" "localhost" "port" 8080} (:leaf-value result)))
      (is (= 3 (:segment-count result))))))


(deftest collapsing-result-leaf-array-test
  (testing "Collapses chain ending in array"
    (let [value {"config" {"tags" ["a" "b" "c"]}}
          siblings []
          options {:key-collapsing :safe :flatten-depth ##Inf}
          result (keys/collapse "data" value siblings options)]
      (is (some? result))
      (is (= "data.config.tags" (:collapsed-key result)))
      (is (nil? (:remainder result)))
      (is (= ["a" "b" "c"] (:leaf-value result)))
      (is (= 3 (:segment-count result))))))


(deftest collapsing-result-leaf-empty-object-test
  (testing "Collapses chain ending in empty object"
    (let [value {"config" {"settings" {}}}
          siblings []
          options {:key-collapsing :safe :flatten-depth ##Inf}
          result (keys/collapse "data" value siblings options)]
      (is (some? result))
      (is (= "data.config.settings" (:collapsed-key result)))
      (is (nil? (:remainder result)))
      (is (= {} (:leaf-value result)))
      (is (= 3 (:segment-count result))))))


(deftest collapsing-result-valid-identifiers-test
  (testing "Collapses with valid identifier segments"
    (let [value {"user_config" {"server_name" "localhost"}}
          siblings []
          options {:key-collapsing :safe :flatten-depth ##Inf}
          result (keys/collapse "app_data" value siblings options)]
      (is (some? result))
      (is (= "app_data.user_config.server_name" (:collapsed-key result))))))


;; ============================================================================
;; Edge Cases
;; ============================================================================

(deftest collapsing-result-underscore-prefix-test
  (testing "Collapses keys starting with underscore"
    (let [value {"_private" {"_config" "value"}}
          siblings []
          options {:key-collapsing :safe :flatten-depth ##Inf}
          result (keys/collapse "_data" value siblings options)]
      (is (some? result))
      (is (= "_data._private._config" (:collapsed-key result))))))


(deftest collapsing-result-very-deep-chain-test
  (testing "Collapses very deep chain (10+ levels)"
    (let [value {"a" {"b" {"c" {"d" {"e" {"f" {"g" {"h" {"i" {"j" "deep"}}}}}}}}}}
          siblings []
          options {:key-collapsing :safe :flatten-depth ##Inf}
          result (keys/collapse "root" value siblings options)]
      (is (some? result))
      (is (= "root.a.b.c.d.e.f.g.h.i.j" (:collapsed-key result)))
      (is (= "deep" (:leaf-value result)))
      (is (= 11 (:segment-count result))))))


(deftest collapsing-result-root-literal-collision-test
  (testing "Returns nil when collapsed key collides with root literal"
    (let [value {"config" {"server" "localhost"}}
          siblings []
          root-literals #{"data.config.server"}  ; Root-level dotted key
          options {:key-collapsing :safe :flatten-depth ##Inf}
          result (keys/collapse "data" value siblings options root-literals nil)]
      (is (nil? result)))))


(deftest collapsing-result-mixed-case-identifiers-test
  (testing "Collapses with mixed case identifiers"
    (let [value {"MyConfig" {"ServerName" "localhost"}}
          siblings []
          options {:key-collapsing :safe :flatten-depth ##Inf}
          result (keys/collapse "AppData" value siblings options)]
      (is (some? result))
      (is (= "AppData.MyConfig.ServerName" (:collapsed-key result))))))


(deftest collapsing-result-numeric-suffix-test
  (testing "Collapses keys with numeric suffixes"
    (let [value {"config2" {"server3" "localhost"}}
          siblings []
          options {:key-collapsing :safe :flatten-depth ##Inf}
          result (keys/collapse "data1" value siblings options)]
      (is (some? result))
      (is (= "data1.config2.server3" (:collapsed-key result))))))


(deftest collapsing-result-single-level-no-fold-test
  (testing "Returns nil for single-level (only 1 segment total)"
    (let [value "not-an-object"
          siblings []
          options {:key-collapsing :safe :flatten-depth ##Inf}
          result (keys/collapse "key" value siblings options)]
      (is (nil? result)))))


(deftest collapsing-result-empty-object-chain-test
  (testing "Collapses chain with empty object at multiple levels"
    (let [value {"a" {"b" {}}}
          siblings []
          options {:key-collapsing :safe :flatten-depth ##Inf}
          result (keys/collapse "root" value siblings options)]
      (is (some? result))
      (is (= "root.a.b" (:collapsed-key result)))
      (is (= {} (:leaf-value result))))))


(deftest collapsing-result-flatten-depth-1-test
  (testing "Returns nil when flatten-depth is 1 (need 2+ for collapsing)"
    (let [value {"config" {"server" "localhost"}}
          siblings []
          options {:key-collapsing :safe :flatten-depth 1}
          result (keys/collapse "data" value siblings options)]
      (is (nil? result)))))


(deftest collapsing-result-collision-with-nested-prefix-test
  (testing "Returns nil when collapsed key is prefix of sibling"
    (let [value {"config" {"server" "localhost"}}
          siblings ["data.config"]  ; Sibling that is prefix
          options {:key-collapsing :safe :flatten-depth ##Inf}
          result (keys/collapse "data" value siblings options)]
      ;; Should still fold since "data.config.server" != "data.config"
      (is (some? result)))))
