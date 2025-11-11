(ns com.vadelabs.toon.key-folding-integration-test
  (:require
    #?(:clj [clojure.test :refer [deftest is testing]]
       :cljs [cljs.test :refer [deftest is testing]])
    [com.vadelabs.toon.interface :as toon]))


;; ============================================================================
;; Key Folding Integration Tests
;; ============================================================================

(deftest key-folding-disabled-by-default-test
  (testing "Key folding is disabled by default"
    (let [data {"data" {"config" {"server" "localhost"}}}
          result (toon/encode data)]
      (is (= "data:\n  config:\n    server: localhost" result)))))


(deftest key-folding-simple-chain-test
  (testing "Folds simple nested single-key objects"
    (let [data {"data" {"config" {"server" "localhost"}}}
          result (toon/encode data {:key-folding :safe})]
      (is (= "data.config.server: localhost" result)))))


(deftest key-folding-multiple-chains-test
  (testing "Folds multiple independent chains"
    (let [data {"user" {"name" {"first" "Alice"}}
                "app" {"version" {"number" "1.0"}}}
          result (toon/encode data {:key-folding :safe})
          lines (clojure.string/split-lines result)]
      (is (= 2 (count lines)))
      (is (some #(= "app.version.number: \"1.0\"" %) lines))
      (is (some #(= "user.name.first: Alice" %) lines)))))


(deftest key-folding-partial-fold-test
  (testing "Partially folds when encountering multi-key object"
    (let [data {"data" {"config" {"server" "localhost" "port" 8080}}}
          result (toon/encode data {:key-folding :safe})
          lines (clojure.string/split-lines result)]
      ;; Should have data.config: followed by indented keys
      (is (= 3 (count lines)))
      (is (= "data.config:" (first lines)))
      ;; Port and server can be in any order
      (is (or (and (clojure.string/includes? (second lines) "port")
                   (clojure.string/includes? (nth lines 2) "server"))
              (and (clojure.string/includes? (second lines) "server")
                   (clojure.string/includes? (nth lines 2) "port")))))))


(deftest key-folding-with-array-leaf-test
  (testing "Folds chain ending in array"
    (let [data {"data" {"config" {"tags" ["a" "b" "c"]}}}
          result (toon/encode data {:key-folding :safe})]
      (is (= "data.config.tags[3]: a,b,c" result)))))


(deftest key-folding-with-empty-array-test
  (testing "Folds chain ending in empty array"
    (let [data {"data" {"config" {"items" []}}}
          result (toon/encode data {:key-folding :safe})]
      (is (= "data.config.items[0]" result)))))


(deftest key-folding-with-empty-object-leaf-test
  (testing "Folds chain ending in empty object"
    (let [data {"data" {"config" {"settings" {}}}}
          result (toon/encode data {:key-folding :safe})]
      (is (= "data.config.settings:" result)))))


(deftest key-folding-with-object-array-test
  (testing "Folds chain ending in array of objects"
    (let [data {"data" {"users" [{"id" 1 "name" "Alice"}
                                 {"id" 2 "name" "Bob"}]}}
          result (toon/encode data {:key-folding :safe})]
      (is (= "data.users[2]:\n  [2]{id,name}:\n    1,Alice\n    2,Bob" result)))))


(deftest key-folding-flatten-depth-limit-test
  (testing "Respects flatten-depth limit"
    (let [data {"a" {"b" {"c" {"d" "value"}}}}
          result (toon/encode data {:key-folding :safe :flatten-depth 3})]
      (is (= "a.b.c:\n  d: value" result)))))


(deftest key-folding-two-segments-test
  (testing "Folds with two segments"
    (let [data {"data" {"server" "localhost"}}
          result (toon/encode data {:key-folding :safe})]
      (is (= "data.server: localhost" result)))))


(deftest key-folding-invalid-segments-test
  (testing "Does not fold when segments contain dots"
    (let [data {"data" {"config" {"server.name" "localhost"}}}
          result (toon/encode data {:key-folding :safe})]
      (is (= "data:\n  config:\n    server.name: localhost" result))))

  (testing "Does not fold when segments contain slashes"
    (let [data {"data" {"config" {"server/name" "localhost"}}}
          result (toon/encode data {:key-folding :safe})]
      (is (= "data:\n  config:\n    server/name: localhost" result))))

  (testing "Does not fold when segments start with digits"
    (let [data {"data" {"config" {"123" "localhost"}}}
          result (toon/encode data {:key-folding :safe})]
      (is (= "data:\n  config:\n    \"123\": localhost" result)))))


(deftest key-folding-nested-object-with-sibling-test
  (testing "Folds one chain while keeping sibling unfolded"
    (let [data {"nested" {"single" {"key" "value"}}
                "other" "data"}
          result (toon/encode data {:key-folding :safe})
          lines (clojure.string/split-lines result)]
      (is (= 2 (count lines)))
      (is (or (and (= "nested.single.key: value" (first lines))
                   (= "other: data" (second lines)))
              (and (= "other: data" (first lines))
                   (= "nested.single.key: value" (second lines))))))))


(deftest key-folding-with-numbers-test
  (testing "Folds chain with numeric leaf values"
    (let [data {"config" {"settings" {"port" 8080}}}
          result (toon/encode data {:key-folding :safe})]
      (is (= "config.settings.port: 8080" result)))))


(deftest key-folding-with-booleans-test
  (testing "Folds chain with boolean leaf values"
    (let [data {"config" {"settings" {"enabled" true}}}
          result (toon/encode data {:key-folding :safe})]
      (is (= "config.settings.enabled: true" result)))))


(deftest key-folding-with-null-test
  (testing "Folds chain with null leaf values"
    (let [data {"config" {"settings" {"value" nil}}}
          result (toon/encode data {:key-folding :safe})]
      (is (= "config.settings.value: null" result)))))


(deftest key-folding-mixed-structure-test
  (testing "Folds appropriate chains in mixed structure"
    (let [data {"app" {"name" "MyApp"
                       "config" {"server" {"host" "localhost"}}}
                "version" "1.0"}
          result (toon/encode data {:key-folding :safe})
          lines (clojure.string/split-lines result)]
      ;; app has multiple keys so stays nested, but config.server.host folds within it
      (is (= 4 (count lines)))
      (is (some #(= "app:" %) lines))
      (is (some #(clojure.string/includes? % "name: MyApp") lines))
      (is (some #(clojure.string/includes? % "config.server.host: localhost") lines))
      (is (some #(= "version: \"1.0\"" %) lines)))))


;; ============================================================================
;; Edge Cases
;; ============================================================================

(deftest key-folding-underscore-prefix-test
  (testing "Folds keys starting with underscore"
    (let [data {"_private" {"_config" {"_value" "secret"}}}
          result (toon/encode data {:key-folding :safe})]
      (is (= "_private._config._value: secret" result)))))


(deftest key-folding-very-deep-chain-test
  (testing "Folds very deep nested chains"
    (let [data {"a" {"b" {"c" {"d" {"e" {"f" "deep"}}}}}}
          result (toon/encode data {:key-folding :safe})]
      (is (= "a.b.c.d.e.f: deep" result)))))


(deftest key-folding-mixed-case-test
  (testing "Folds mixed case identifiers"
    (let [data {"AppData" {"MyConfig" {"ServerName" "localhost"}}}
          result (toon/encode data {:key-folding :safe})]
      (is (= "AppData.MyConfig.ServerName: localhost" result)))))


(deftest key-folding-numeric-suffix-test
  (testing "Folds keys with numeric suffixes"
    (let [data {"data1" {"config2" {"server3" "localhost"}}}
          result (toon/encode data {:key-folding :safe})]
      (is (= "data1.config2.server3: localhost" result)))))


(deftest key-folding-with-tab-delimiter-test
  (testing "Folds with tab delimiter for arrays"
    (let [data {"data" {"tags" ["a" "b" "c"]}}
          result (toon/encode data {:key-folding :safe :delimiter "\t"})]
      (is (= "data.tags[3\t]: a\tb\tc" result)))))


(deftest key-folding-with-pipe-delimiter-test
  (testing "Folds with pipe delimiter for arrays"
    (let [data {"data" {"tags" ["a" "b" "c"]}}
          result (toon/encode data {:key-folding :safe :delimiter "|"})]
      (is (= "data.tags[3|]: a|b|c" result)))))


(deftest key-folding-flatten-depth-zero-test
  (testing "No folding when flatten-depth is 0"
    (let [data {"data" {"config" {"server" "localhost"}}}
          result (toon/encode data {:key-folding :safe :flatten-depth 0})]
      (is (= "data:\n  config:\n    server: localhost" result)))))


(deftest key-folding-partial-with-array-objects-test
  (testing "Partially folds with array of objects"
    (let [data {"app" {"users" [{"id" 1} {"id" 2}]}}
          result (toon/encode data {:key-folding :safe})]
      (is (clojure.string/includes? result "app.users[2]:")))))


(deftest key-folding-empty-nested-objects-test
  (testing "Folds chains with multiple empty objects"
    (let [data {"a" {"b" {"c" {}}}}
          result (toon/encode data {:key-folding :safe})]
      (is (= "a.b.c:" result)))))


(deftest key-folding-roundtrip-underscore-test
  (testing "Roundtrip with underscore-prefixed keys"
    (let [original {"_data" {"_config" {"_server" "localhost"}}}
          encoded (toon/encode original {:key-folding :safe})
          decoded (toon/decode encoded {:expand-paths :safe})]
      (is (= "_data._config._server: localhost" encoded))
      (is (= original decoded)))))


(deftest key-folding-multiple-chains-same-prefix-test
  (testing "Folds multiple chains with same prefix"
    (let [data {"user" {"profile" {"name" "Alice"}}
                "user2" {"profile" {"name" "Bob"}}}
          result (toon/encode data {:key-folding :safe})
          lines (clojure.string/split-lines result)]
      (is (= 2 (count lines)))
      (is (some #(= "user.profile.name: Alice" %) lines))
      (is (some #(= "user2.profile.name: Bob" %) lines)))))
