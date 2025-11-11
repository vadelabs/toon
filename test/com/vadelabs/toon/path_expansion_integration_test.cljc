(ns com.vadelabs.toon.path-expansion-integration-test
  (:require
    #?(:clj [clojure.test :refer [deftest is testing]]
       :cljs [cljs.test :refer [deftest is testing]])
    [com.vadelabs.toon.interface :as toon]))


;; ============================================================================
;; Path Expansion Integration Tests
;; ============================================================================

(deftest path-expansion-disabled-by-default-test
  (testing "Path expansion is disabled by default"
    (let [toon-str "data.config.server: localhost"
          result (toon/decode toon-str)]
      (is (= {"data.config.server" "localhost"} result)))))


(deftest path-expansion-simple-dotted-key-test
  (testing "Expands simple dotted key"
    (let [toon-str "data.config.server: localhost"
          result (toon/decode toon-str {:expand-paths :safe})]
      (is (= {"data" {"config" {"server" "localhost"}}} result)))))


(deftest path-expansion-multiple-keys-test
  (testing "Expands multiple dotted keys"
    (let [toon-str "user.name.first: Alice\nuser.name.last: Smith"
          result (toon/decode toon-str {:expand-paths :safe})]
      (is (= {"user" {"name" {"first" "Alice" "last" "Smith"}}} result)))))


(deftest path-expansion-mixed-keys-test
  (testing "Expands dotted keys while preserving literal keys"
    (let [toon-str "data.config.server: localhost\nversion: 1.0"
          result (toon/decode toon-str {:expand-paths :safe})]
      (is (= {"data" {"config" {"server" "localhost"}}
              "version" 1.0} result)))))


(deftest path-expansion-with-arrays-test
  (testing "Expands paths in array values"
    (let [toon-str "[2]:\n  - data.config: value1\n  - data.config: value2"
          result (toon/decode toon-str {:expand-paths :safe})]
      (is (= [{"data" {"config" "value1"}}
              {"data" {"config" "value2"}}] result)))))


(deftest path-expansion-roundtrip-test
  (testing "Roundtrip with folding and expansion"
    (let [original {"data" {"config" {"server" "localhost"}}}
          encoded (toon/encode original {:key-folding :safe})
          decoded (toon/decode encoded {:expand-paths :safe})]
      (is (= "data.config.server: localhost" encoded))
      (is (= original decoded)))))


(deftest path-expansion-complex-roundtrip-test
  (testing "Complex roundtrip with folding and expansion"
    (let [original {"app" {"name" "MyApp"}
                    "config" {"server" {"host" "localhost" "port" 8080}}}
          encoded (toon/encode original {:key-folding :safe})
          decoded (toon/decode encoded {:expand-paths :safe})]
      ;; After roundtrip, structure should be preserved (numbers become floats)
      (is (= "app.name: MyApp\nconfig.server:\n  host: localhost\n  port: 8080"
             encoded))
      (is (= {"app" {"name" "MyApp"}
              "config" {"server" {"host" "localhost" "port" 8080.0}}} decoded)))))


(deftest path-expansion-with-numeric-values-test
  (testing "Expands paths with numeric values"
    (let [toon-str "config.port: 8080\nconfig.timeout: 30"
          result (toon/decode toon-str {:expand-paths :safe})]
      (is (= {"config" {"port" 8080.0 "timeout" 30.0}} result)))))


(deftest path-expansion-with-boolean-values-test
  (testing "Expands paths with boolean values"
    (let [toon-str "config.enabled: true\nconfig.debug: false"
          result (toon/decode toon-str {:expand-paths :safe})]
      (is (= {"config" {"enabled" true "debug" false}} result)))))


(deftest path-expansion-with-null-values-test
  (testing "Expands paths with null values"
    (let [toon-str "config.value: null"
          result (toon/decode toon-str {:expand-paths :safe})]
      (is (= {"config" {"value" nil}} result)))))


(deftest path-expansion-with-array-leaf-test
  (testing "Expands paths with array leaf values"
    (let [toon-str "config.tags[3]: a,b,c"
          result (toon/decode toon-str {:expand-paths :safe})]
      (is (= {"config" {"tags" ["a" "b" "c"]}} result)))))


(deftest path-expansion-nested-object-roundtrip-test
  (testing "Roundtrip with deeply nested objects"
    (let [original {"level1" {"level2" {"level3" {"value" "deep"}}}}
          encoded (toon/encode original {:key-folding :safe})
          decoded (toon/decode encoded {:expand-paths :safe})]
      (is (= "level1.level2.level3.value: deep" encoded))
      (is (= original decoded)))))


(deftest path-expansion-partial-fold-roundtrip-test
  (testing "Roundtrip with partially folded structure"
    (let [original {"data" {"config" {"host" "localhost" "port" 8080}}}
          encoded (toon/encode original {:key-folding :safe})
          decoded (toon/decode encoded {:expand-paths :safe})]
      (is (= {"data" {"config" {"host" "localhost" "port" 8080.0}}} decoded)))))


(deftest path-expansion-invalid-segments-test
  (testing "Does not expand keys with invalid segments"
    (let [toon-str "data.123.config: value"
          result (toon/decode toon-str {:expand-paths :safe})]
      ;; "123" is not a valid identifier, so no expansion
      (is (= {"data.123.config" "value"} result)))))


(deftest path-expansion-multiple-independent-paths-test
  (testing "Expands multiple independent dotted paths"
    (let [toon-str "user.profile.name: Alice\napp.settings.theme: dark"
          result (toon/decode toon-str {:expand-paths :safe})]
      (is (= {"user" {"profile" {"name" "Alice"}}
              "app" {"settings" {"theme" "dark"}}} result)))))
