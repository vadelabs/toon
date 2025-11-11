(ns com.vadelabs.toon.decode.parser-test
  (:require
    #?(:clj [clojure.test :refer [deftest is testing]]
       :cljs [cljs.test :refer [deftest is testing]])
    [com.vadelabs.toon.decode.parser :as parser]
    [com.vadelabs.toon.utils :as str-utils]))


;; ============================================================================
;; String Unescaping Tests
;; ============================================================================

(deftest unescape-simple-string-test
  (testing "Unescape string with no escapes"
    (is (= "hello" (str-utils/unescaped "hello")))))


(deftest unescape-backslash-test
  (testing "Unescape backslash"
    (is (= "\\" (str-utils/unescaped "\\\\")))))


(deftest unescape-quote-test
  (testing "Unescape double quote"
    (is (= "\"" (str-utils/unescaped "\\\"")))))


(deftest unescape-newline-test
  (testing "Unescape newline"
    (is (= "\n" (str-utils/unescaped "\\n")))))


(deftest unescape-return-test
  (testing "Unescape carriage return"
    (is (= "\r" (str-utils/unescaped "\\r")))))


(deftest unescape-tab-test
  (testing "Unescape tab"
    (is (= "\t" (str-utils/unescaped "\\t")))))


(deftest unescape-multiple-escapes-test
  (testing "Unescape string with multiple escape sequences"
    (is (= "line1\nline2\ttab" (str-utils/unescaped "line1\\nline2\\ttab")))))


(deftest unescape-mixed-content-test
  (testing "Unescape string with mixed content"
    (is (= "He said \"hello\"" (str-utils/unescaped "He said \\\"hello\\\"")))))


(deftest unescape-invalid-escape-strict-test
  (testing "Strict mode rejects invalid escape sequence"
    (is (thrown-with-msg?
          #?(:clj Exception :cljs js/Error)
          #"Invalid escape sequence"
          (str-utils/unescaped "\\x" true)))))


(deftest unescape-invalid-escape-non-strict-test
  (testing "Non-strict mode allows invalid escape sequence"
    (is (= "\\x" (str-utils/unescaped "\\x" false)))))


;; ============================================================================
;; String Literal Parsing Tests
;; ============================================================================

(deftest parse-simple-string-literal-test
  (testing "Parse simple quoted string"
    (is (= "hello" (parser/string-literal "\"hello\"")))))


(deftest parse-empty-string-literal-test
  (testing "Parse empty quoted string"
    (is (= "" (parser/string-literal "\"\"")))))


(deftest parse-string-with-escapes-test
  (testing "Parse string with escape sequences"
    (is (= "hello\nworld" (parser/string-literal "\"hello\\nworld\"")))))


(deftest parse-string-with-quotes-test
  (testing "Parse string containing escaped quotes"
    (is (= "say \"hi\"" (parser/string-literal "\"say \\\"hi\\\"\"")))))


(deftest parse-unterminated-string-test
  (testing "Throw on unterminated string"
    (is (thrown-with-msg?
          #?(:clj Exception :cljs js/Error)
          #"Unterminated string"
          (parser/string-literal "\"hello")))))


(deftest parse-non-string-literal-test
  (testing "Throw on non-quoted input"
    (is (thrown-with-msg?
          #?(:clj Exception :cljs js/Error)
          #"must start with double quote"
          (parser/string-literal "hello")))))


;; ============================================================================
;; Primitive Token Parsing Tests
;; ============================================================================

(deftest parse-null-token-test
  (testing "Parse null literal"
    (is (nil? (parser/primitive-token "null")))))


(deftest parse-true-token-test
  (testing "Parse true literal"
    (is (true? (parser/primitive-token "true")))))


(deftest parse-false-token-test
  (testing "Parse false literal"
    (is (false? (parser/primitive-token "false")))))


(deftest parse-integer-token-test
  (testing "Parse integer"
    (is (= 42.0 (parser/primitive-token "42")))))


(deftest parse-negative-integer-token-test
  (testing "Parse negative integer"
    (is (= -10.0 (parser/primitive-token "-10")))))


(deftest parse-decimal-token-test
  (testing "Parse decimal number"
    (is (= 3.14 (parser/primitive-token "3.14")))))


(deftest parse-quoted-string-token-test
  (testing "Parse quoted string token"
    (is (= "hello world" (parser/primitive-token "\"hello world\"")))))


(deftest parse-unquoted-string-token-test
  (testing "Parse unquoted string token"
    (is (= "hello" (parser/primitive-token "hello")))))


(deftest parse-token-with-whitespace-test
  (testing "Parse token with surrounding whitespace"
    (is (= 42.0 (parser/primitive-token "  42  ")))))


;; ============================================================================
;; Delimited Value Parsing Tests
;; ============================================================================

(deftest parse-simple-comma-delimited-test
  (testing "Parse simple comma-delimited values"
    (is (= ["a" "b" "c"] (parser/delimited-values "a,b,c")))))


(deftest parse-comma-delimited-with-spaces-test
  (testing "Parse comma-delimited with spaces (trimmed)"
    (is (= ["a" "b" "c"] (parser/delimited-values "a , b , c")))))


(deftest parse-delimited-with-quoted-value-test
  (testing "Parse with quoted value containing delimiter"
    (is (= ["a" "\"b,c\"" "d"] (parser/delimited-values "a,\"b,c\",d")))))


(deftest parse-delimited-with-escaped-quote-test
  (testing "Parse with escaped quote in quoted value"
    (is (= ["a" "\"b\\\"c\"" "d"] (parser/delimited-values "a,\"b\\\"c\",d")))))


(deftest parse-tab-delimited-test
  (testing "Parse tab-delimited values"
    (is (= ["a" "b" "c"] (parser/delimited-values "a\tb\tc" "\t")))))


(deftest parse-pipe-delimited-test
  (testing "Parse pipe-delimited values"
    (is (= ["a" "b" "c"] (parser/delimited-values "a|b|c" "|")))))


(deftest parse-empty-delimited-values-test
  (testing "Parse string with empty values"
    (is (= ["a" "" "c"] (parser/delimited-values "a,,c")))))


(deftest parse-single-value-test
  (testing "Parse single value (no delimiter)"
    (is (= ["hello"] (parser/delimited-values "hello")))))


(deftest parse-delimited-with-complex-quote-test
  (testing "Parse complex quoted value"
    (is (= ["1" "\"Alice, Bob\"" "true"]
           (parser/delimited-values "1,\"Alice, Bob\",true")))))


;; ============================================================================
;; Bracket Segment Parsing Tests
;; ============================================================================

(deftest parse-simple-bracket-segment-test
  (testing "Parse simple bracket with length"
    (let [result (parser/bracket-segment "3")]
      (is (= 3 (:length result)))
      (is (= "," (:delimiter result))))))




(deftest parse-bracket-with-pipe-delimiter-test
  (testing "Parse bracket with pipe delimiter"
    (let [result (parser/bracket-segment "3|")]
      (is (= 3 (:length result)))
      (is (= "|" (:delimiter result))))))


(deftest parse-bracket-with-tab-delimiter-test
  (testing "Parse bracket with tab delimiter"
    (let [result (parser/bracket-segment (str "3" \tab))]
      (is (= 3 (:length result)))
      (is (= "\t" (:delimiter result))))))




(deftest parse-invalid-bracket-segment-test
  (testing "Throw on invalid bracket segment"
    (is (thrown-with-msg?
          #?(:clj Exception :cljs js/Error)
          #"Invalid array length"
          (parser/bracket-segment "abc")))))


;; ============================================================================
;; Array Header Parsing Tests
;; ============================================================================

(deftest parse-simple-array-header-test
  (testing "Parse simple array header without key"
    (let [result (parser/array-header-line "[3]:")]
      (is (= 3 (:length result)))
      (is (= "," (:delimiter result)))
      (is (nil? (:key result)))
      (is (nil? (:fields result)))
      (is (nil? (:inline-values result))))))


(deftest parse-array-header-with-key-test
  (testing "Parse array header with key"
    (let [result (parser/array-header-line "items[2]:")]
      (is (= "items" (:key result)))
      (is (= 2 (:length result))))))


(deftest parse-array-header-with-fields-test
  (testing "Parse array header with field list"
    (let [result (parser/array-header-line "[2]{id,name}:")]
      (is (= 2 (:length result)))
      (is (= ["id" "name"] (:fields result))))))


(deftest parse-array-header-with-key-and-fields-test
  (testing "Parse array header with key and fields"
    (let [result (parser/array-header-line "users[2]{id,name}:")]
      (is (= "users" (:key result)))
      (is (= 2 (:length result)))
      (is (= ["id" "name"] (:fields result))))))


(deftest parse-array-header-with-inline-values-test
  (testing "Parse array header with inline values"
    (let [result (parser/array-header-line "[3]: a,b,c")]
      (is (= 3 (:length result)))
      (is (= "a,b,c" (:inline-values result))))))




(deftest parse-array-header-with-delimiter-test
  (testing "Parse array header with pipe delimiter"
    (let [result (parser/array-header-line "[3|]:")]
      (is (= 3 (:length result)))
      (is (= "|" (:delimiter result))))))


(deftest parse-array-header-complex-test
  (testing "Parse complex array header with all features"
    (let [result (parser/array-header-line "data[5|]{a,b,c}: 1|2|3")]
      (is (= "data" (:key result)))
      (is (= 5 (:length result)))
      (is (= "|" (:delimiter result)))
      (is (= ["a" "b" "c"] (:fields result)))
      (is (= "1|2|3" (:inline-values result))))))


(deftest parse-invalid-array-header-test
  (testing "Throw on invalid array header (no brackets)"
    (is (thrown-with-msg?
          #?(:clj Exception :cljs js/Error)
          #"must contain bracket segment"
          (parser/array-header-line "items:")))))


;; ============================================================================
;; Key Token Parsing Tests
;; ============================================================================

(deftest parse-simple-key-test
  (testing "Parse simple unquoted key"
    (is (= "name" (parser/key-token "name")))))


(deftest parse-key-with-trailing-colon-test
  (testing "Parse key with trailing colon"
    (is (= "name" (parser/key-token "name:")))))


(deftest parse-key-with-spaces-test
  (testing "Parse key with surrounding spaces"
    (is (= "name" (parser/key-token "  name  ")))))


(deftest parse-quoted-key-test
  (testing "Parse quoted key"
    (is (= "user name" (parser/key-token "\"user name\"")))))


(deftest parse-quoted-key-with-colon-test
  (testing "Parse quoted key with trailing colon"
    (is (= "user name" (parser/key-token "\"user name\":")))))


(deftest parse-quoted-key-with-escapes-test
  (testing "Parse quoted key with escape sequences"
    (is (= "key\nvalue" (parser/key-token "\"key\\nvalue\"")))))


;; ============================================================================
;; Edge Cases
;; ============================================================================

(deftest parse-zero-length-array-header-test
  (testing "Parse array header with zero length"
    (let [result (parser/array-header-line "[0]:")]
      (is (= 0 (:length result))))))


(deftest parse-large-array-length-test
  (testing "Parse array header with large length"
    (let [result (parser/array-header-line "[1000]:")]
      (is (= 1000 (:length result))))))


(deftest find-closing-quote-test
  (testing "Find closing quote in string"
    (is (= 5 (str-utils/closing-quote "hello\"" 0)))
    (is (= 6 (str-utils/closing-quote "ab\\\"cd\"" 0)))))


(deftest find-unquoted-char-test
  (testing "Find character outside quoted sections"
    (is (= 1 (str-utils/unquoted-char "a,b" \,)))
    (is (= 5 (str-utils/unquoted-char "\"a,b\",c" \,)))))
