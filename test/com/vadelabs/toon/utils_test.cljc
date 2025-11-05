(ns com.vadelabs.toon.utils-test
  (:require #?(:clj [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :refer [deftest is testing]])
            [com.vadelabs.toon.utils :as utils]))

;; ============================================================================
;; Escape String Tests (JSON-style escaping)
;; ============================================================================

(deftest escape-backslash-test
  (testing "Backslash is escaped to double backslash"
    (is (= "C:\\\\path" (utils/escaped "C:\\path")))
    (is (= "a\\\\b\\\\c" (utils/escaped "a\\b\\c")))))

(deftest escape-double-quote-test
  (testing "Double quotes are escaped with backslash"
    (is (= "say \\\"hi\\\"" (utils/escaped "say \"hi\"")))
    (is (= "\\\"quoted\\\"" (utils/escaped "\"quoted\"")))))

(deftest escape-newline-test
  (testing "Newlines are escaped to \\n"
    (is (= "line1\\nline2" (utils/escaped "line1\nline2")))
    (is (= "a\\nb\\nc" (utils/escaped "a\nb\nc")))))

(deftest escape-carriage-return-test
  (testing "Carriage returns are escaped to \\r"
    (is (= "line1\\rline2" (utils/escaped "line1\rline2")))))

(deftest escape-tab-test
  (testing "Tabs are escaped to \\t"
    (is (= "a\\tb" (utils/escaped "a\tb")))
    (is (= "col1\\tcol2\\tcol3" (utils/escaped "col1\tcol2\tcol3")))))

(deftest escape-combined-test
  (testing "Multiple escape sequences work correctly"
    (is (= "say \\\"hi\\\"\\nbye" (utils/escaped "say \"hi\"\nbye")))
    (is (= "C:\\\\path\\\\file.txt" (utils/escaped "C:\\path\\file.txt")))
    (is (= "a\\\\b\\\"c\\nd" (utils/escaped "a\\b\"c\nd")))))

(deftest escape-order-test
  (testing "Backslash is escaped first to avoid double-escaping"
    ;; Input: \n (literal backslash + n, not newline)
    ;; Should become: \\n (escaped backslash + n)
    ;; NOT: \\\\n (which would be double-escaping)
    (is (= "\\\\n" (utils/escaped "\\n")))))

;; ============================================================================
;; Pattern Detection Tests
;; ============================================================================

;; Removed boolean-literal?, null-literal? tests - these are now inlined in needs-quoting?
;; The functionality is still tested through needs-quoting? comprehensive tests below

(deftest numeric-like-detection-test
  (testing "Integer-like strings are detected"
    (is (utils/numeric-like? "0"))
    (is (utils/numeric-like? "42"))
    (is (utils/numeric-like? "-7"))
    (is (utils/numeric-like? "05"))  ; Leading zero
    (is (utils/numeric-like? "1000")))

  (testing "Decimal-like strings are detected"
    (is (utils/numeric-like? "3.14"))
    (is (utils/numeric-like? "-0.5"))
    (is (utils/numeric-like? "0.0"))
    (is (utils/numeric-like? "123.456")))

  (testing "Scientific notation is detected"
    (is (utils/numeric-like? "1e6"))
    (is (utils/numeric-like? "1E6"))
    (is (utils/numeric-like? "1e-6"))
    (is (utils/numeric-like? "1E+10"))
    (is (utils/numeric-like? "-3.14e-5")))

  (testing "Non-numeric strings are not detected"
    (is (not (utils/numeric-like? "42a")))
    (is (not (utils/numeric-like? "a42")))
    (is (not (utils/numeric-like? "12.34.56")))
    (is (not (utils/numeric-like? "1e")))
    (is (not (utils/numeric-like? "e6")))
    (is (not (utils/numeric-like? " 42")))
    (is (not (utils/numeric-like? "42 ")))))

(deftest structural-chars-detection-test
  (testing "Structural characters are detected"
    (is (utils/has-structural-chars? "[test]"))
    (is (utils/has-structural-chars? "{key}"))
    (is (utils/has-structural-chars? "- item"))
    (is (utils/has-structural-chars? "a[0]"))
    (is (utils/has-structural-chars? "obj{x}"))
    (is (utils/has-structural-chars? "value-123")))

  (testing "Non-structural strings are not detected"
    (is (not (utils/has-structural-chars? "simple")))
    (is (not (utils/has-structural-chars? "hello world")))
    (is (not (utils/has-structural-chars? "123")))))

;; Removed has-control-chars?, has-backslash?, has-whitespace-padding? tests
;; These are now inlined in needs-quoting?, functionality tested through comprehensive tests below

;; ============================================================================
;; needs-quoting? Comprehensive Tests
;; ============================================================================

(deftest simple-strings-no-quoting-test
  (testing "Simple strings without special characters don't need quoting"
    (is (not (utils/needs-quoting? "simple")))
    (is (not (utils/needs-quoting? "hello")))
    (is (not (utils/needs-quoting? "world123")))
    (is (not (utils/needs-quoting? "CamelCase")))
    (is (not (utils/needs-quoting? "with_underscores")))
    (is (not (utils/needs-quoting? "hello world")))))  ; Internal spaces OK

(deftest empty-string-needs-quoting-test
  (testing "Empty and blank strings need quoting"
    (is (utils/needs-quoting? ""))
    (is (utils/needs-quoting? " "))
    (is (utils/needs-quoting? "  "))
    (is (utils/needs-quoting? "\t"))))

(deftest whitespace-padding-needs-quoting-test
  (testing "Strings with leading/trailing whitespace need quoting"
    (is (utils/needs-quoting? " leading"))
    (is (utils/needs-quoting? "trailing "))
    (is (utils/needs-quoting? " both "))
    (is (utils/needs-quoting? "\tleading-tab"))))

(deftest boolean-literal-needs-quoting-test
  (testing "Boolean literals need quoting"
    (is (utils/needs-quoting? "true"))
    (is (utils/needs-quoting? "false")))

  (testing "Case-sensitive: only exact matches need quoting"
    (is (not (utils/needs-quoting? "True")))
    (is (not (utils/needs-quoting? "FALSE")))))

(deftest null-literal-needs-quoting-test
  (testing "Null literal needs quoting"
    (is (utils/needs-quoting? "null")))

  (testing "Case-sensitive: only exact match needs quoting"
    (is (not (utils/needs-quoting? "Null")))
    (is (not (utils/needs-quoting? "NULL")))))

(deftest numeric-like-needs-quoting-test
  (testing "Integer-like strings need quoting"
    (is (utils/needs-quoting? "0"))
    (is (utils/needs-quoting? "42"))
    (is (utils/needs-quoting? "-7"))
    (is (utils/needs-quoting? "05")))  ; Leading zero

  (testing "Decimal-like strings need quoting"
    (is (utils/needs-quoting? "3.14"))
    (is (utils/needs-quoting? "-0.5")))

  (testing "Scientific notation needs quoting"
    (is (utils/needs-quoting? "1e6"))
    (is (utils/needs-quoting? "1E+10"))
    (is (utils/needs-quoting? "-3.14e-5")))

  (testing "Non-numeric strings don't need quoting for this reason"
    (is (not (utils/numeric-like? "42a")))
    (is (not (utils/numeric-like? "a42")))))

(deftest structural-chars-need-quoting-test
  (testing "Structural characters need quoting"
    (is (utils/needs-quoting? "[test]"))
    (is (utils/needs-quoting? "{key}"))
    (is (utils/needs-quoting? "- item"))
    (is (utils/needs-quoting? "a[0]"))
    (is (utils/needs-quoting? "value-123"))))

(deftest delimiter-needs-quoting-test
  (testing "Strings containing delimiter need quoting"
    (is (utils/needs-quoting? "a,b" ","))
    (is (utils/needs-quoting? "a,b,c" ","))
    (is (utils/needs-quoting? "a\tb" "\t"))
    (is (utils/needs-quoting? "a|b" "|")))

  (testing "Delimiter-aware: comma doesn't need quoting with tab/pipe"
    (is (not (utils/needs-quoting? "a,b" "\t")))
    (is (not (utils/needs-quoting? "a,b" "|")))))

(deftest colon-needs-quoting-test
  (testing "Strings containing colon need quoting"
    (is (utils/needs-quoting? "key:value"))
    (is (utils/needs-quoting? "http://example.com"))))

(deftest quote-needs-quoting-test
  (testing "Strings containing quotes need quoting"
    (is (utils/needs-quoting? "say \"hi\""))
    (is (utils/needs-quoting? "\"quoted\""))))

(deftest backslash-needs-quoting-test
  (testing "Strings containing backslash need quoting"
    (is (utils/needs-quoting? "C:\\path"))
    (is (utils/needs-quoting? "a\\b"))))

(deftest control-chars-need-quoting-test
  (testing "Strings with control characters need quoting"
    (is (utils/needs-quoting? "line1\nline2"))
    (is (utils/needs-quoting? "a\tb"))
    (is (utils/needs-quoting? "line1\rline2"))))

;; ============================================================================
;; quote-string Tests
;; ============================================================================

(deftest basic-quote-string-test
  (testing "Basic quoting wraps value in double quotes"
    (is (= "\"hello\"" (utils/wrap "hello")))
    (is (= "\"world\"" (utils/wrap "world")))
    (is (= "\"123\"" (utils/wrap "123")))))

(deftest quote-string-escapes-quotes-test
  (testing "Internal quotes are escaped with backslash"
    (is (= "\"say \\\"hi\\\"\"" (utils/wrap "say \"hi\"")))
    (is (= "\"\\\"quoted\\\"\"" (utils/wrap "\"quoted\"")))
    (is (= "\"a\\\"b\\\"c\"" (utils/wrap "a\"b\"c")))))

(deftest quote-string-escapes-backslash-test
  (testing "Backslashes are escaped"
    (is (= "\"C:\\\\path\"" (utils/wrap "C:\\path")))
    (is (= "\"a\\\\b\"" (utils/wrap "a\\b")))))

(deftest quote-string-escapes-control-chars-test
  (testing "Control characters are escaped"
    (is (= "\"line1\\nline2\"" (utils/wrap "line1\nline2")))
    (is (= "\"a\\tb\"" (utils/wrap "a\tb")))
    (is (= "\"line1\\rline2\"" (utils/wrap "line1\rline2")))))

(deftest quote-string-combined-escaping-test
  (testing "Multiple escape sequences work together"
    (is (= "\"say \\\"hi\\\"\\nbye\"" (utils/wrap "say \"hi\"\nbye")))
    (is (= "\"C:\\\\path\\\\file.txt\"" (utils/wrap "C:\\path\\file.txt")))
    (is (= "\"a\\\\b\\\"c\\nd\"" (utils/wrap "a\\b\"c\nd")))))

(deftest quote-empty-string-test
  (testing "Empty strings are quoted"
    (is (= "\"\"" (utils/wrap "")))))

;; ============================================================================
;; maybe-quote Tests
;; ============================================================================

(deftest maybe-quote-simple-unchanged-test
  (testing "Simple strings are not quoted"
    (is (= "simple" (utils/maybe-quote "simple")))
    (is (= "hello" (utils/maybe-quote "hello")))
    (is (= "world123" (utils/maybe-quote "world123")))))

(deftest maybe-quote-reserved-literals-quoted-test
  (testing "Reserved literals are quoted"
    (is (= "\"true\"" (utils/maybe-quote "true")))
    (is (= "\"false\"" (utils/maybe-quote "false")))
    (is (= "\"null\"" (utils/maybe-quote "null")))))

(deftest maybe-quote-numeric-quoted-test
  (testing "Numeric-like strings are quoted"
    (is (= "\"42\"" (utils/maybe-quote "42")))
    (is (= "\"-3.14\"" (utils/maybe-quote "-3.14")))
    (is (= "\"1e-6\"" (utils/maybe-quote "1e-6")))))

(deftest maybe-quote-structural-quoted-test
  (testing "Strings with structural characters are quoted"
    (is (= "\"[test]\"" (utils/maybe-quote "[test]")))
    (is (= "\"{key}\"" (utils/maybe-quote "{key}")))
    (is (= "\"- item\"" (utils/maybe-quote "- item")))))

(deftest maybe-quote-delimiter-aware-test
  (testing "maybe-quote is delimiter-aware"
    (is (= "\"a,b\"" (utils/maybe-quote "a,b" ",")))
    (is (= "a,b" (utils/maybe-quote "a,b" "|")))
    (is (= "a,b" (utils/maybe-quote "a,b" "\t")))
    (is (= "\"a|b\"" (utils/maybe-quote "a|b" "|")))
    (is (= "\"a\\tb\"" (utils/maybe-quote "a\tb" "\t")))))  ; Tab is escaped to \t

(deftest maybe-quote-escapes-when-needed-test
  (testing "Quoted strings have escape sequences"
    (is (= "\"say \\\"hi\\\"\"" (utils/maybe-quote "say \"hi\"")))
    (is (= "\"C:\\\\path\"" (utils/maybe-quote "C:\\path")))
    (is (= "\"line1\\nline2\"" (utils/maybe-quote "line1\nline2")))))

;; ============================================================================
;; Edge Cases and Real-World Examples
;; ============================================================================

(deftest unicode-and-emoji-test
  (testing "Unicode and emoji don't need quoting"
    (is (= "café" (utils/maybe-quote "café")))
    (is (= "你好" (utils/maybe-quote "你好")))
    (is (= "🚀" (utils/maybe-quote "🚀")))))

(deftest complex-real-world-strings-test
  (testing "Real-world examples"
    ;; URLs with colons need quoting
    (is (= "\"http://example.com\"" (utils/maybe-quote "http://example.com")))

    ;; File paths with backslashes need quoting and escaping
    (is (= "\"C:\\\\Users\\\\file.txt\"" (utils/maybe-quote "C:\\Users\\file.txt")))

    ;; JSON-like strings need quoting
    (is (= "\"{\\\"key\\\":\\\"value\\\"}\"" (utils/maybe-quote "{\"key\":\"value\"}")))

    ;; Leading zeros (looks numeric)
    (is (= "\"007\"" (utils/maybe-quote "007")))
    (is (= "0xFF" (utils/maybe-quote "0xFF")))))  ; Not matched by numeric pattern, no quoting needed

;; ============================================================================
;; Key Validation Tests
;; ============================================================================

(deftest valid-unquoted-key-test
  (testing "Valid unquoted keys match pattern /^[A-Z_][\\w./]*$/i"
    ;; Valid keys
    (is (utils/valid-unquoted-key? "name"))
    (is (utils/valid-unquoted-key? "Name"))
    (is (utils/valid-unquoted-key? "_private"))
    (is (utils/valid-unquoted-key? "user_id"))
    (is (utils/valid-unquoted-key? "user123"))
    (is (utils/valid-unquoted-key? "user.name"))
    (is (utils/valid-unquoted-key? "user_name.first"))
    (is (utils/valid-unquoted-key? "user/profile"))       ; namespaced (Clojure)
    (is (utils/valid-unquoted-key? "ns.name/key"))        ; namespaced with dots

    ;; Invalid keys
    (is (not (utils/valid-unquoted-key? "123")))           ; starts with digit
    (is (not (utils/valid-unquoted-key? "user name")))    ; contains space
    (is (not (utils/valid-unquoted-key? "key:value")))    ; contains colon
    (is (not (utils/valid-unquoted-key? "[special]")))    ; contains brackets
    (is (not (utils/valid-unquoted-key? "user-name")))    ; contains hyphen
    (is (not (utils/valid-unquoted-key? "")))             ; empty string
    ))

(deftest maybe-quote-key-test
  (testing "Keys are quoted when they don't match valid pattern"
    ;; Valid keys remain unquoted
    (is (= "name" (utils/maybe-quote-key "name")))
    (is (= "user_id" (utils/maybe-quote-key "user_id")))
    (is (= "user.name" (utils/maybe-quote-key "user.name")))

    ;; Invalid keys are quoted
    (is (= "\"user name\"" (utils/maybe-quote-key "user name")))
    (is (= "\"123\"" (utils/maybe-quote-key "123")))
    (is (= "\"key:value\"" (utils/maybe-quote-key "key:value")))
    (is (= "\"[special]\"" (utils/maybe-quote-key "[special]")))
    (is (= "\"user-name\"" (utils/maybe-quote-key "user-name")))

    ;; Keys with escape sequences are quoted and escaped
    (is (= "\"key\\nvalue\"" (utils/maybe-quote-key "key\nvalue")))
    (is (= "\"say \\\"hi\\\"\"" (utils/maybe-quote-key "say \"hi\"")))))

;; ============================================================================
;; String Search Utility Tests
;; ============================================================================

(deftest find-closing-quote-test
  (testing "Find closing quote in string"
    (is (= 5 (utils/closing-quote "hello\"" 0)))
    (is (= 6 (utils/closing-quote "ab\\\"cd\"" 0)))))

(deftest find-unquoted-char-test
  (testing "Find character outside quoted sections"
    (is (= 1 (utils/unquoted-char "a,b" \,)))
    (is (= 5 (utils/unquoted-char "\"a,b\",c" \,)))))
