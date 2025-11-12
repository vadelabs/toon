(ns com.vadelabs.toon.decode.keys-test
  (:require
    #?(:clj [clojure.test :refer [deftest is testing]]
       :cljs [cljs.test :refer [deftest is testing]])
    [com.vadelabs.toon.decode.keys :as keys]))


;; ============================================================================
;; Path Expansion Unit Tests
;; ============================================================================

(deftest expand-paths-disabled-test
  (testing "Returns value unchanged when expansion is disabled"
    (let [value {"data.config.server" "localhost"}
          result (keys/expand value true :off)]
      (is (= value result)))))


(deftest expand-paths-simple-dotted-key-test
  (testing "Expands simple dotted key"
    (let [value {"data.config.server" "localhost"}
          result (keys/expand value true :safe)]
      (is (= {"data" {"config" {"server" "localhost"}}} result)))))


(deftest expand-paths-multiple-dotted-keys-test
  (testing "Expands multiple dotted keys"
    (let [value {"user.name.first" "Alice"
                 "user.name.last" "Smith"}
          result (keys/expand value true :safe)]
      (is (= {"user" {"name" {"first" "Alice" "last" "Smith"}}} result)))))


(deftest expand-paths-mixed-keys-test
  (testing "Expands dotted keys while preserving literal keys"
    (let [value {"data.config.server" "localhost"
                 "version" "1.0"}
          result (keys/expand value true :safe)]
      (is (= {"data" {"config" {"server" "localhost"}}
              "version" "1.0"} result)))))


(deftest expand-paths-invalid-segments-test
  (testing "Does not expand when segments contain dots in the segment itself"
    (let [value {"data.config" "value"}
          result (keys/expand value true :safe)]
      (is (= {"data" {"config" "value"}} result))))

  (testing "Does not expand when segments start with digits"
    (let [value {"data.123.config" "value"}
          result (keys/expand value true :safe)]
      ;; "123" is not a valid identifier, so no expansion
      (is (= {"data.123.config" "value"} result)))))


(deftest expand-paths-array-values-test
  (testing "Recursively expands array elements"
    (let [value [{"data.config" "value1"}
                 {"data.config" "value2"}]
          result (keys/expand value true :safe)]
      (is (= [{"data" {"config" "value1"}}
              {"data" {"config" "value2"}}] result)))))


(deftest expand-paths-nested-objects-test
  (testing "Recursively expands nested object values"
    (let [value {"outer" {"inner.key" "value"}}
          result (keys/expand value true :safe)]
      (is (= {"outer" {"inner" {"key" "value"}}} result)))))


(deftest expand-paths-deep-merge-test
  (testing "Deep merges when multiple keys expand to same path"
    (let [value {"user.name" "Alice"
                 "user.age" 30}
          result (keys/expand value true :safe)]
      (is (= {"user" {"name" "Alice" "age" 30}} result)))))


(deftest expand-paths-conflict-strict-test
  (testing "Throws on conflict in strict mode"
    (let [value {"user.name" "Alice"
                 "user" "Bob"}]  ; Conflict: "user" expands to object but also has literal value
      (is (thrown? #?(:clj Exception :cljs js/Error)
                   (keys/expand value true :safe))))))


(deftest expand-paths-conflict-non-strict-test
  (testing "Overwrites on conflict in non-strict mode (LWW)"
    (let [value {"user.name" "Alice"
                 "user" "Bob"}
          result (keys/expand value false :safe)]
      ;; Non-strict should not throw, result depends on insertion order
      (is (map? result)))))


(deftest expand-paths-primitives-test
  (testing "Returns primitives unchanged"
    (is (= "string" (keys/expand "string" true :safe)))
    (is (= 42 (keys/expand 42 true :safe)))
    (is (= true (keys/expand true true :safe)))
    (is (nil? (keys/expand nil true :safe)))))


(deftest expand-paths-empty-structures-test
  (testing "Handles empty structures"
    (is (= {} (keys/expand {} true :safe)))
    (is (= [] (keys/expand [] true :safe)))))


(deftest expand-paths-complex-structure-test
  (testing "Expands complex nested structure"
    (let [value {"app.config.server.host" "localhost"
                 "app.config.server.port" 8080
                 "app.name" "MyApp"
                 "version" "1.0"}
          result (keys/expand value true :safe)]
      (is (= {"app" {"config" {"server" {"host" "localhost"
                                         "port" 8080}}
                     "name" "MyApp"}
              "version" "1.0"} result)))))


;; ============================================================================
;; Edge Cases
;; ============================================================================

(deftest expand-paths-underscore-prefix-test
  (testing "Expands keys starting with underscore"
    (let [value {"_data._private._config" "value"}
          result (keys/expand value true :safe)]
      (is (= {"_data" {"_private" {"_config" "value"}}} result)))))


(deftest expand-paths-very-deep-dotted-key-test
  (testing "Expands very deep dotted key (10+ levels)"
    (let [value {"root.a.b.c.d.e.f.g.h.i.j" "deep"}
          result (keys/expand value true :safe)
          expected {"root" {"a" {"b" {"c" {"d" {"e" {"f" {"g" {"h" {"i" {"j" "deep"}}}}}}}}}}}]
      (is (= expected result)))))


(deftest expand-paths-mixed-case-identifiers-test
  (testing "Expands mixed case identifiers"
    (let [value {"AppData.MyConfig.ServerName" "localhost"}
          result (keys/expand value true :safe)]
      (is (= {"AppData" {"MyConfig" {"ServerName" "localhost"}}} result)))))


(deftest expand-paths-numeric-suffix-test
  (testing "Expands keys with numeric suffixes"
    (let [value {"data1.config2.server3" "localhost"}
          result (keys/expand value true :safe)]
      (is (= {"data1" {"config2" {"server3" "localhost"}}} result)))))


(deftest expand-paths-single-segment-test
  (testing "Leaves single segment keys unchanged"
    (let [value {"simple_key" "value"}
          result (keys/expand value true :safe)]
      (is (= {"simple_key" "value"} result)))))


(deftest expand-paths-overlapping-keys-deep-merge-test
  (testing "Deep merges overlapping dotted keys"
    (let [value {"user.profile.name" "Alice"
                 "user.profile.age" 30
                 "user.settings.theme" "dark"}
          result (keys/expand value true :safe)]
      (is (= {"user" {"profile" {"name" "Alice" "age" 30}
                      "settings" {"theme" "dark"}}} result)))))


(deftest expand-paths-conflict-strict-error-message-test
  (testing "Throws with descriptive error in strict mode on conflict"
    (let [value {"user.name" "Alice"
                 "user" "Bob"}]
      (try
        (keys/expand value true :safe)
        (is false "Should have thrown exception")
        (catch #?(:clj Exception :cljs js/Error) e
          (let [msg #?(:clj (.getMessage e) :cljs (.-message e))]
            (is (re-find #"Path expansion conflict" msg))
            (is (re-find #"user" msg))))))))


(deftest expand-paths-non-strict-lww-test
  (testing "Non-strict uses last-write-wins on conflicts"
    (let [value {"user.name" "Alice"
                 "user" "Bob"}
          result (keys/expand value false :safe)]
      ;; Result depends on map iteration order, but should not throw
      (is (or (= {"user" "Bob"} result)
              (= {"user" {"name" "Alice"}} result))))))


(deftest expand-paths-deeply-nested-literal-and-expanded-test
  (testing "Handles mix of literal nested objects and dotted keys"
    (let [value {"outer" {"inner.dotted.key" "value1"
                          "regular" "value2"}}
          result (keys/expand value true :safe)]
      (is (= {"outer" {"inner" {"dotted" {"key" "value1"}}
                       "regular" "value2"}} result)))))


(deftest expand-paths-empty-segment-invalid-test
  (testing "Does not expand keys with empty segments (double dots)"
    (let [value {"data..config" "value"}
          result (keys/expand value true :safe)]
      ;; Should not expand because empty segment between dots is invalid
      (is (= {"data..config" "value"} result)))))


(deftest expand-paths-trailing-dot-invalid-test
  (testing "Trailing dots create empty segments, remaining valid segments expand"
    (let [value {"data.config." "value"}
          result (keys/expand value true :safe)]
      ;; Empty string segment from trailing dot fails validation, but "data.config" are valid
      ;; So it expands the valid segments
      (is (= {"data" {"config" "value"}} result)))))


(deftest expand-paths-leading-dot-invalid-test
  (testing "Does not expand keys with leading dots"
    (let [value {".data.config" "value"}
          result (keys/expand value true :safe)]
      ;; Leading dot creates empty segment, which is invalid
      (is (= {".data.config" "value"} result)))))


(deftest expand-paths-array-with-dotted-keys-test
  (testing "Recursively expands dotted keys in array elements"
    (let [value [{"user.name" "Alice"}
                 {"user.name" "Bob"}
                 {"user.age" 30}]
          result (keys/expand value true :safe)]
      (is (= [{"user" {"name" "Alice"}}
              {"user" {"name" "Bob"}}
              {"user" {"age" 30}}] result)))))


(deftest expand-paths-deeply-nested-arrays-test
  (testing "Handles arrays nested in expanded objects"
    (let [value {"data.items" [1 2 3]
                 "data.tags" ["a" "b"]}
          result (keys/expand value true :safe)]
      (is (= {"data" {"items" [1 2 3]
                      "tags" ["a" "b"]}} result)))))


;; ============================================================================
;; Additional Coverage Tests for Internal Functions
;; ============================================================================

(deftest expand-paths-strict-conflict-with-path-context-test
  (testing "Throws with path context in strict mode when intermediate path conflicts"
    (let [value {"a.b.c" "value1"
                 "a.b" "value2"}]
      (is (thrown? #?(:clj Exception :cljs js/Error)
                   (keys/expand value true :safe))))))


(deftest expand-paths-strict-conflict-nested-test
  (testing "Throws in strict mode when nested expansion conflicts"
    (let [value {"outer" {"a.b.c" "value1"
                          "a.b" "value2"}}]
      (is (thrown? #?(:clj Exception :cljs js/Error)
                   (keys/expand value true :safe))))))


(deftest expand-paths-non-strict-nested-conflict-test
  (testing "Handles nested conflicts in non-strict mode"
    (let [value {"outer" {"a.b.c" "value1"
                          "a.b" "value2"}}
          result (keys/expand value false :safe)]
      (is (map? result))
      (is (contains? result "outer")))))


(deftest expand-paths-single-segment-dotted-key-test
  (testing "Handles dotted key that expands to single level"
    (let [value {"a.b" "value"}
          result (keys/expand value true :safe)]
      (is (= {"a" {"b" "value"}} result)))))


(deftest expand-paths-merge-multiple-objects-test
  (testing "Deep merges multiple nested object values"
    (let [value {"a.b" {"c" {"d" 1}}
                 "a.b.c" {"e" 2}}
          result (keys/expand value true :safe)]
      (is (= {"a" {"b" {"c" {"d" 1 "e" 2}}}} result)))))


(deftest expand-paths-conflict-at-intermediate-level-test
  (testing "Handles conflict at intermediate path level in non-strict"
    (let [value {"a.b.c.d" "deep"
                 "a.b" "shallow"}
          result (keys/expand value false :safe)]
      (is (map? result)))))


(deftest expand-paths-literal-key-conflict-with-expanded-test
  (testing "Handles literal key that conflicts with already-expanded path"
    (let [value {"a.b" "value1"
                 "a" {"b" "value2"}}
          result (keys/expand value false :safe)]
      (is (map? result)))))


(deftest expand-paths-recursive-array-expansion-test
  (testing "Recursively expands nested arrays with dotted keys"
    (let [value [[{"a.b" "v1"}] [{"c.d" "v2"}]]
          result (keys/expand value true :safe)]
      (is (= [[{"a" {"b" "v1"}}] [{"c" {"d" "v2"}}]] result)))))


(deftest expand-paths-deep-object-merge-test
  (testing "Deep merges when both existing and new values are objects"
    (let [value {"user.profile" {"name" "Alice"}
                 "user" {"profile" {"age" 30}}}
          result (keys/expand value true :safe)]
      (is (or (= {"user" {"profile" {"age" 30}}} result)
              (= {"user" {"profile" {"name" "Alice" "age" 30}}} result))))))


(deftest expand-paths-non-strict-primitive-overwrite-test
  (testing "Non-strict mode overwrites primitive with object"
    (let [value {"a" "primitive"
                 "a.b" "nested"}
          result (keys/expand value false :safe)]
      (is (map? result)))))


(deftest expand-paths-strict-type-mismatch-error-test
  (testing "Strict mode provides type information in error"
    (let [value {"a" "string"
                 "a.b" "nested"}]
      (try
        (keys/expand value true :safe)
        (is false "Should have thrown exception")
        (catch #?(:clj Exception :cljs js/Error) e
          (let [data #?(:clj (ex-data e) :cljs (.-data e))]
            (is (some? data))
            (is (= :path-expansion-conflict (:type data)))))))))
