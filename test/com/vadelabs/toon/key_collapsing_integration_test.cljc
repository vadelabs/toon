(ns com.vadelabs.toon.key-collapsing-integration-test
  (:require
    #?(:clj [clojure.test :refer [deftest is testing]]
       :cljs [cljs.test :refer [deftest is testing]])
    [clojure.string :as str]
    [com.vadelabs.toon.interface :as toon]))


;; ============================================================================
;; Key Collapsing Integration Tests
;; ============================================================================

(deftest key-collapsing-disabled-by-default-test
  (testing "Key collapsing is disabled by default"
    (let [data {"data" {"config" {"server" "localhost"}}}
          result (toon/encode data)]
      (is (= "data:\n  config:\n    server: localhost" result)))))


(deftest key-collapsing-simple-chain-test
  (testing "Collapses simple nested single-key objects"
    (let [data {"data" {"config" {"server" "localhost"}}}
          result (toon/encode data {:key-collapsing :safe})]
      (is (= "data.config.server: localhost" result)))))


(deftest key-collapsing-multiple-chains-test
  (testing "Collapses multiple independent chains"
    (let [data {"user" {"name" {"first" "Alice"}}
                "app" {"version" {"number" "1.0"}}}
          result (toon/encode data {:key-collapsing :safe})
          lines (str/split-lines result)]
      (is (= 2 (count lines)))
      (is (some #(= "app.version.number: \"1.0\"" %) lines))
      (is (some #(= "user.name.first: Alice" %) lines)))))


(deftest key-collapsing-partial-fold-test
  (testing "Partially collapses when encountering multi-key object"
    (let [data {"data" {"config" {"server" "localhost" "port" 8080}}}
          result (toon/encode data {:key-collapsing :safe})
          lines (str/split-lines result)]
      ;; Should have data.config: followed by indented keys
      (is (= 3 (count lines)))
      (is (= "data.config:" (first lines)))
      ;; Port and server can be in any order
      (is (or (and (str/includes? (second lines) "port")
                   (str/includes? (nth lines 2) "server"))
              (and (str/includes? (second lines) "server")
                   (str/includes? (nth lines 2) "port")))))))


(deftest key-collapsing-with-array-leaf-test
  (testing "Collapses chain ending in array"
    (let [data {"data" {"config" {"tags" ["a" "b" "c"]}}}
          result (toon/encode data {:key-collapsing :safe})]
      (is (= "data.config.tags[3]: a,b,c" result)))))


(deftest key-collapsing-with-empty-array-test
  (testing "Collapses chain ending in empty array"
    (let [data {"data" {"config" {"items" []}}}
          result (toon/encode data {:key-collapsing :safe})]
      (is (= "data.config.items[0]" result)))))


(deftest key-collapsing-with-empty-object-leaf-test
  (testing "Collapses chain ending in empty object"
    (let [data {"data" {"config" {"settings" {}}}}
          result (toon/encode data {:key-collapsing :safe})]
      (is (= "data.config.settings:" result)))))


(deftest key-collapsing-with-object-array-test
  (testing "Collapses chain ending in array of objects"
    (let [data {"data" {"users" [{"id" 1 "name" "Alice"}
                                 {"id" 2 "name" "Bob"}]}}
          result (toon/encode data {:key-collapsing :safe})]
      (is (= "data.users[2]:\n  [2]{id,name}:\n    1,Alice\n    2,Bob" result)))))


(deftest key-collapsing-flatten-depth-limit-test
  (testing "Respects flatten-depth limit"
    (let [data {"a" {"b" {"c" {"d" "value"}}}}
          result (toon/encode data {:key-collapsing :safe :flatten-depth 3})]
      (is (= "a.b.c:\n  d: value" result)))))


(deftest key-collapsing-two-segments-test
  (testing "Collapses with two segments"
    (let [data {"data" {"server" "localhost"}}
          result (toon/encode data {:key-collapsing :safe})]
      (is (= "data.server: localhost" result)))))


(deftest key-collapsing-invalid-segments-test
  (testing "Does not fold when segments contain dots"
    (let [data {"data" {"config" {"server.name" "localhost"}}}
          result (toon/encode data {:key-collapsing :safe})]
      (is (= "data:\n  config:\n    server.name: localhost" result))))

  (testing "Does not fold when segments contain slashes"
    (let [data {"data" {"config" {"server/name" "localhost"}}}
          result (toon/encode data {:key-collapsing :safe})]
      (is (= "data:\n  config:\n    server/name: localhost" result))))

  (testing "Does not fold when segments start with digits"
    (let [data {"data" {"config" {"123" "localhost"}}}
          result (toon/encode data {:key-collapsing :safe})]
      (is (= "data:\n  config:\n    \"123\": localhost" result)))))


(deftest key-collapsing-nested-object-with-sibling-test
  (testing "Collapses one chain while keeping sibling unfolded"
    (let [data {"nested" {"single" {"key" "value"}}
                "other" "data"}
          result (toon/encode data {:key-collapsing :safe})
          lines (str/split-lines result)]
      (is (= 2 (count lines)))
      (is (or (and (= "nested.single.key: value" (first lines))
                   (= "other: data" (second lines)))
              (and (= "other: data" (first lines))
                   (= "nested.single.key: value" (second lines))))))))


(deftest key-collapsing-with-numbers-test
  (testing "Collapses chain with numeric leaf values"
    (let [data {"config" {"settings" {"port" 8080}}}
          result (toon/encode data {:key-collapsing :safe})]
      (is (= "config.settings.port: 8080" result)))))


(deftest key-collapsing-with-booleans-test
  (testing "Collapses chain with boolean leaf values"
    (let [data {"config" {"settings" {"enabled" true}}}
          result (toon/encode data {:key-collapsing :safe})]
      (is (= "config.settings.enabled: true" result)))))


(deftest key-collapsing-with-null-test
  (testing "Collapses chain with null leaf values"
    (let [data {"config" {"settings" {"value" nil}}}
          result (toon/encode data {:key-collapsing :safe})]
      (is (= "config.settings.value: null" result)))))


(deftest key-collapsing-mixed-structure-test
  (testing "Collapses appropriate chains in mixed structure"
    (let [data {"app" {"name" "MyApp"
                       "config" {"server" {"host" "localhost"}}}
                "version" "1.0"}
          result (toon/encode data {:key-collapsing :safe})
          lines (str/split-lines result)]
      ;; app has multiple keys so stays nested, but config.server.host collapses within it
      (is (= 4 (count lines)))
      (is (some #(= "app:" %) lines))
      (is (some #(str/includes? % "name: MyApp") lines))
      (is (some #(str/includes? % "config.server.host: localhost") lines))
      (is (some #(= "version: \"1.0\"" %) lines)))))


;; ============================================================================
;; Edge Cases
;; ============================================================================

(deftest key-collapsing-underscore-prefix-test
  (testing "Collapses keys starting with underscore"
    (let [data {"_private" {"_config" {"_value" "secret"}}}
          result (toon/encode data {:key-collapsing :safe})]
      (is (= "_private._config._value: secret" result)))))


(deftest key-collapsing-very-deep-chain-test
  (testing "Collapses very deep nested chains"
    (let [data {"a" {"b" {"c" {"d" {"e" {"f" "deep"}}}}}}
          result (toon/encode data {:key-collapsing :safe})]
      (is (= "a.b.c.d.e.f: deep" result)))))


(deftest key-collapsing-mixed-case-test
  (testing "Collapses mixed case identifiers"
    (let [data {"AppData" {"MyConfig" {"ServerName" "localhost"}}}
          result (toon/encode data {:key-collapsing :safe})]
      (is (= "AppData.MyConfig.ServerName: localhost" result)))))


(deftest key-collapsing-numeric-suffix-test
  (testing "Collapses keys with numeric suffixes"
    (let [data {"data1" {"config2" {"server3" "localhost"}}}
          result (toon/encode data {:key-collapsing :safe})]
      (is (= "data1.config2.server3: localhost" result)))))


(deftest key-collapsing-with-tab-delimiter-test
  (testing "Collapses with tab delimiter for arrays"
    (let [data {"data" {"tags" ["a" "b" "c"]}}
          result (toon/encode data {:key-collapsing :safe :delimiter "\t"})]
      (is (= "data.tags[3\t]: a\tb\tc" result)))))


(deftest key-collapsing-with-pipe-delimiter-test
  (testing "Collapses with pipe delimiter for arrays"
    (let [data {"data" {"tags" ["a" "b" "c"]}}
          result (toon/encode data {:key-collapsing :safe :delimiter "|"})]
      (is (= "data.tags[3|]: a|b|c" result)))))


(deftest key-collapsing-flatten-depth-zero-test
  (testing "No collapsing when flatten-depth is 0"
    (let [data {"data" {"config" {"server" "localhost"}}}
          result (toon/encode data {:key-collapsing :safe :flatten-depth 0})]
      (is (= "data:\n  config:\n    server: localhost" result)))))


(deftest key-collapsing-partial-with-array-objects-test
  (testing "Partially collapses with array of objects"
    (let [data {"app" {"users" [{"id" 1} {"id" 2}]}}
          result (toon/encode data {:key-collapsing :safe})]
      (is (str/includes? result "app.users[2]:")))))


(deftest key-collapsing-empty-nested-objects-test
  (testing "Collapses chains with multiple empty objects"
    (let [data {"a" {"b" {"c" {}}}}
          result (toon/encode data {:key-collapsing :safe})]
      (is (= "a.b.c:" result)))))


(deftest key-collapsing-roundtrip-underscore-test
  (testing "Roundtrip with underscore-prefixed keys"
    (let [original {"_data" {"_config" {"_server" "localhost"}}}
          encoded (toon/encode original {:key-collapsing :safe})
          decoded (toon/decode encoded {:expand-paths :safe})]
      (is (= "_data._config._server: localhost" encoded))
      (is (= original decoded)))))


(deftest key-collapsing-multiple-chains-same-prefix-test
  (testing "Collapses multiple chains with same prefix"
    (let [data {"user" {"profile" {"name" "Alice"}}
                "user2" {"profile" {"name" "Bob"}}}
          result (toon/encode data {:key-collapsing :safe})
          lines (str/split-lines result)]
      (is (= 2 (count lines)))
      (is (some #(= "user.profile.name: Alice" %) lines))
      (is (some #(= "user2.profile.name: Bob" %) lines)))))
