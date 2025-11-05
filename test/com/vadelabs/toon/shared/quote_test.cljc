(ns com.vadelabs.toon.shared.quote-test
  (:require #?(:clj [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :refer [deftest is testing]])
            [com.vadelabs.toon.shared.quote :as quote]))

;; ============================================================================
;; Escape String Tests (JSON-style escaping)
;; ============================================================================

(deftest escape-backslash-test
  (testing "Backslash is escaped to double backslash"
    (is (= "C:\\\\path" (quote/escaped "C:\\path")))
    (is (= "a\\\\b\\\\c" (quote/escaped "a\\b\\c")))))

(deftest escape-double-quote-test
  (testing "Double quotes are escaped with backslash"
    (is (= "say \\\"hi\\\"" (quote/escaped "say \"hi\"")))
    (is (= "\\\"quoted\\\"" (quote/escaped "\"quoted\"")))))

(deftest escape-newline-test
  (testing "Newlines are escaped to \\n"
    (is (= "line1\\nline2" (quote/escaped "line1\nline2")))
    (is (= "a\\nb\\nc" (quote/escaped "a\nb\nc")))))

(deftest escape-carriage-return-test
  (testing "Carriage returns are escaped to \\r"
    (is (= "line1\\rline2" (quote/escaped "line1\rline2")))))

(deftest escape-tab-test
  (testing "Tabs are escaped to \\t"
    (is (= "a\\tb" (quote/escaped "a\tb")))
    (is (= "col1\\tcol2\\tcol3" (quote/escaped "col1\tcol2\tcol3")))))

(deftest escape-combined-test
  (testing "Multiple escape sequences work correctly"
    (is (= "say \\\"hi\\\"\\nbye" (quote/escaped "say \"hi\"\nbye")))
    (is (= "C:\\\\path\\\\file.txt" (quote/escaped "C:\\path\\file.txt")))
    (is (= "a\\\\b\\\"c\\nd" (quote/escaped "a\\b\"c\nd")))))

(deftest escape-order-test
  (testing "Backslash is escaped first to avoid double-escaping"
    ;; Input: \n (literal backslash + n, not newline)
    ;; Should become: \\n (escaped backslash + n)
    ;; NOT: \\\\n (which would be double-escaping)
    (is (= "\\\\n" (quote/escaped "\\n")))))

;; ============================================================================
;; Pattern Detection Tests
;; ============================================================================

;; Removed boolean-literal?, null-literal? tests - these are now inlined in needs-quoting?
;; The functionality is still tested through needs-quoting? comprehensive tests below

(deftest numeric-like-detection-test
  (testing "Integer-like strings are detected"
    (is (quote/numeric-like? "0"))
    (is (quote/numeric-like? "42"))
    (is (quote/numeric-like? "-7"))
    (is (quote/numeric-like? "05"))  ; Leading zero
    (is (quote/numeric-like? "1000")))

  (testing "Decimal-like strings are detected"
    (is (quote/numeric-like? "3.14"))
    (is (quote/numeric-like? "-0.5"))
    (is (quote/numeric-like? "0.0"))
    (is (quote/numeric-like? "123.456")))

  (testing "Scientific notation is detected"
    (is (quote/numeric-like? "1e6"))
    (is (quote/numeric-like? "1E6"))
    (is (quote/numeric-like? "1e-6"))
    (is (quote/numeric-like? "1E+10"))
    (is (quote/numeric-like? "-3.14e-5")))

  (testing "Non-numeric strings are not detected"
    (is (not (quote/numeric-like? "42a")))
    (is (not (quote/numeric-like? "a42")))
    (is (not (quote/numeric-like? "12.34.56")))
    (is (not (quote/numeric-like? "1e")))
    (is (not (quote/numeric-like? "e6")))
    (is (not (quote/numeric-like? " 42")))
    (is (not (quote/numeric-like? "42 ")))))

(deftest structural-chars-detection-test
  (testing "Structural characters are detected"
    (is (quote/has-structural-chars? "[test]"))
    (is (quote/has-structural-chars? "{key}"))
    (is (quote/has-structural-chars? "- item"))
    (is (quote/has-structural-chars? "a[0]"))
    (is (quote/has-structural-chars? "obj{x}"))
    (is (quote/has-structural-chars? "value-123")))

  (testing "Non-structural strings are not detected"
    (is (not (quote/has-structural-chars? "simple")))
    (is (not (quote/has-structural-chars? "hello world")))
    (is (not (quote/has-structural-chars? "123")))))

;; Removed has-control-chars?, has-backslash?, has-whitespace-padding? tests
;; These are now inlined in needs-quoting?, functionality tested through comprehensive tests below

;; ============================================================================
;; needs-quoting? Comprehensive Tests
;; ============================================================================

(deftest simple-strings-no-quoting-test
  (testing "Simple strings without special characters don't need quoting"
    (is (not (quote/needs-quoting? "simple")))
    (is (not (quote/needs-quoting? "hello")))
    (is (not (quote/needs-quoting? "world123")))
    (is (not (quote/needs-quoting? "CamelCase")))
    (is (not (quote/needs-quoting? "with_underscores")))
    (is (not (quote/needs-quoting? "hello world")))))  ; Internal spaces OK

(deftest empty-string-needs-quoting-test
  (testing "Empty and blank strings need quoting"
    (is (quote/needs-quoting? ""))
    (is (quote/needs-quoting? " "))
    (is (quote/needs-quoting? "  "))
    (is (quote/needs-quoting? "\t"))))

(deftest whitespace-padding-needs-quoting-test
  (testing "Strings with leading/trailing whitespace need quoting"
    (is (quote/needs-quoting? " leading"))
    (is (quote/needs-quoting? "trailing "))
    (is (quote/needs-quoting? " both "))
    (is (quote/needs-quoting? "\tleading-tab"))))

(deftest boolean-literal-needs-quoting-test
  (testing "Boolean literals need quoting"
    (is (quote/needs-quoting? "true"))
    (is (quote/needs-quoting? "false")))

  (testing "Case-sensitive: only exact matches need quoting"
    (is (not (quote/needs-quoting? "True")))
    (is (not (quote/needs-quoting? "FALSE")))))

(deftest null-literal-needs-quoting-test
  (testing "Null literal needs quoting"
    (is (quote/needs-quoting? "null")))

  (testing "Case-sensitive: only exact match needs quoting"
    (is (not (quote/needs-quoting? "Null")))
    (is (not (quote/needs-quoting? "NULL")))))

(deftest numeric-like-needs-quoting-test
  (testing "Integer-like strings need quoting"
    (is (quote/needs-quoting? "0"))
    (is (quote/needs-quoting? "42"))
    (is (quote/needs-quoting? "-7"))
    (is (quote/needs-quoting? "05")))  ; Leading zero

  (testing "Decimal-like strings need quoting"
    (is (quote/needs-quoting? "3.14"))
    (is (quote/needs-quoting? "-0.5")))

  (testing "Scientific notation needs quoting"
    (is (quote/needs-quoting? "1e6"))
    (is (quote/needs-quoting? "1E+10"))
    (is (quote/needs-quoting? "-3.14e-5")))

  (testing "Non-numeric strings don't need quoting for this reason"
    (is (not (quote/numeric-like? "42a")))
    (is (not (quote/numeric-like? "a42")))))

(deftest structural-chars-need-quoting-test
  (testing "Structural characters need quoting"
    (is (quote/needs-quoting? "[test]"))
    (is (quote/needs-quoting? "{key}"))
    (is (quote/needs-quoting? "- item"))
    (is (quote/needs-quoting? "a[0]"))
    (is (quote/needs-quoting? "value-123"))))

(deftest delimiter-needs-quoting-test
  (testing "Strings containing delimiter need quoting"
    (is (quote/needs-quoting? "a,b" ","))
    (is (quote/needs-quoting? "a,b,c" ","))
    (is (quote/needs-quoting? "a\tb" "\t"))
    (is (quote/needs-quoting? "a|b" "|")))

  (testing "Delimiter-aware: comma doesn't need quoting with tab/pipe"
    (is (not (quote/needs-quoting? "a,b" "\t")))
    (is (not (quote/needs-quoting? "a,b" "|")))))

(deftest colon-needs-quoting-test
  (testing "Strings containing colon need quoting"
    (is (quote/needs-quoting? "key:value"))
    (is (quote/needs-quoting? "http://example.com"))))

(deftest quote-needs-quoting-test
  (testing "Strings containing quotes need quoting"
    (is (quote/needs-quoting? "say \"hi\""))
    (is (quote/needs-quoting? "\"quoted\""))))

(deftest backslash-needs-quoting-test
  (testing "Strings containing backslash need quoting"
    (is (quote/needs-quoting? "C:\\path"))
    (is (quote/needs-quoting? "a\\b"))))

(deftest control-chars-need-quoting-test
  (testing "Strings with control characters need quoting"
    (is (quote/needs-quoting? "line1\nline2"))
    (is (quote/needs-quoting? "a\tb"))
    (is (quote/needs-quoting? "line1\rline2"))))

;; ============================================================================
;; quote-string Tests
;; ============================================================================

(deftest basic-quote-string-test
  (testing "Basic quoting wraps value in double quotes"
    (is (= "\"hello\"" (quote/wrap "hello")))
    (is (= "\"world\"" (quote/wrap "world")))
    (is (= "\"123\"" (quote/wrap "123")))))

(deftest quote-string-escapes-quotes-test
  (testing "Internal quotes are escaped with backslash"
    (is (= "\"say \\\"hi\\\"\"" (quote/wrap "say \"hi\"")))
    (is (= "\"\\\"quoted\\\"\"" (quote/wrap "\"quoted\"")))
    (is (= "\"a\\\"b\\\"c\"" (quote/wrap "a\"b\"c")))))

(deftest quote-string-escapes-backslash-test
  (testing "Backslashes are escaped"
    (is (= "\"C:\\\\path\"" (quote/wrap "C:\\path")))
    (is (= "\"a\\\\b\"" (quote/wrap "a\\b")))))

(deftest quote-string-escapes-control-chars-test
  (testing "Control characters are escaped"
    (is (= "\"line1\\nline2\"" (quote/wrap "line1\nline2")))
    (is (= "\"a\\tb\"" (quote/wrap "a\tb")))
    (is (= "\"line1\\rline2\"" (quote/wrap "line1\rline2")))))

(deftest quote-string-combined-escaping-test
  (testing "Multiple escape sequences work together"
    (is (= "\"say \\\"hi\\\"\\nbye\"" (quote/wrap "say \"hi\"\nbye")))
    (is (= "\"C:\\\\path\\\\file.txt\"" (quote/wrap "C:\\path\\file.txt")))
    (is (= "\"a\\\\b\\\"c\\nd\"" (quote/wrap "a\\b\"c\nd")))))

(deftest quote-empty-string-test
  (testing "Empty strings are quoted"
    (is (= "\"\"" (quote/wrap "")))))

;; ============================================================================
;; maybe-quote Tests
;; ============================================================================

(deftest maybe-quote-simple-unchanged-test
  (testing "Simple strings are not quoted"
    (is (= "simple" (quote/maybe-quote "simple")))
    (is (= "hello" (quote/maybe-quote "hello")))
    (is (= "world123" (quote/maybe-quote "world123")))))

(deftest maybe-quote-reserved-literals-quoted-test
  (testing "Reserved literals are quoted"
    (is (= "\"true\"" (quote/maybe-quote "true")))
    (is (= "\"false\"" (quote/maybe-quote "false")))
    (is (= "\"null\"" (quote/maybe-quote "null")))))

(deftest maybe-quote-numeric-quoted-test
  (testing "Numeric-like strings are quoted"
    (is (= "\"42\"" (quote/maybe-quote "42")))
    (is (= "\"-3.14\"" (quote/maybe-quote "-3.14")))
    (is (= "\"1e-6\"" (quote/maybe-quote "1e-6")))))

(deftest maybe-quote-structural-quoted-test
  (testing "Strings with structural characters are quoted"
    (is (= "\"[test]\"" (quote/maybe-quote "[test]")))
    (is (= "\"{key}\"" (quote/maybe-quote "{key}")))
    (is (= "\"- item\"" (quote/maybe-quote "- item")))))

(deftest maybe-quote-delimiter-aware-test
  (testing "maybe-quote is delimiter-aware"
    (is (= "\"a,b\"" (quote/maybe-quote "a,b" ",")))
    (is (= "a,b" (quote/maybe-quote "a,b" "|")))
    (is (= "a,b" (quote/maybe-quote "a,b" "\t")))
    (is (= "\"a|b\"" (quote/maybe-quote "a|b" "|")))
    (is (= "\"a\\tb\"" (quote/maybe-quote "a\tb" "\t")))))  ; Tab is escaped to \t

(deftest maybe-quote-escapes-when-needed-test
  (testing "Quoted strings have escape sequences"
    (is (= "\"say \\\"hi\\\"\"" (quote/maybe-quote "say \"hi\"")))
    (is (= "\"C:\\\\path\"" (quote/maybe-quote "C:\\path")))
    (is (= "\"line1\\nline2\"" (quote/maybe-quote "line1\nline2")))))

;; ============================================================================
;; Edge Cases and Real-World Examples
;; ============================================================================

(deftest unicode-and-emoji-test
  (testing "Unicode and emoji don't need quoting"
    (is (= "café" (quote/maybe-quote "café")))
    (is (= "你好" (quote/maybe-quote "你好")))
    (is (= "🚀" (quote/maybe-quote "🚀")))))

(deftest complex-real-world-strings-test
  (testing "Real-world examples"
    ;; URLs with colons need quoting
    (is (= "\"http://example.com\"" (quote/maybe-quote "http://example.com")))

    ;; File paths with backslashes need quoting and escaping
    (is (= "\"C:\\\\Users\\\\file.txt\"" (quote/maybe-quote "C:\\Users\\file.txt")))

    ;; JSON-like strings need quoting
    (is (= "\"{\\\"key\\\":\\\"value\\\"}\"" (quote/maybe-quote "{\"key\":\"value\"}")))

    ;; Leading zeros (looks numeric)
    (is (= "\"007\"" (quote/maybe-quote "007")))
    (is (= "0xFF" (quote/maybe-quote "0xFF")))))  ; Not matched by numeric pattern, no quoting needed

;; ============================================================================
;; Key Validation Tests
;; ============================================================================

(deftest valid-unquoted-key-test
  (testing "Valid unquoted keys match pattern /^[A-Z_][\\w./]*$/i"
    ;; Valid keys
    (is (quote/valid-unquoted-key? "name"))
    (is (quote/valid-unquoted-key? "Name"))
    (is (quote/valid-unquoted-key? "_private"))
    (is (quote/valid-unquoted-key? "user_id"))
    (is (quote/valid-unquoted-key? "user123"))
    (is (quote/valid-unquoted-key? "user.name"))
    (is (quote/valid-unquoted-key? "user_name.first"))
    (is (quote/valid-unquoted-key? "user/profile"))       ; namespaced (Clojure)
    (is (quote/valid-unquoted-key? "ns.name/key"))        ; namespaced with dots

    ;; Invalid keys
    (is (not (quote/valid-unquoted-key? "123")))           ; starts with digit
    (is (not (quote/valid-unquoted-key? "user name")))    ; contains space
    (is (not (quote/valid-unquoted-key? "key:value")))    ; contains colon
    (is (not (quote/valid-unquoted-key? "[special]")))    ; contains brackets
    (is (not (quote/valid-unquoted-key? "user-name")))    ; contains hyphen
    (is (not (quote/valid-unquoted-key? "")))             ; empty string
    ))

(deftest maybe-quote-key-test
  (testing "Keys are quoted when they don't match valid pattern"
    ;; Valid keys remain unquoted
    (is (= "name" (quote/maybe-quote-key "name")))
    (is (= "user_id" (quote/maybe-quote-key "user_id")))
    (is (= "user.name" (quote/maybe-quote-key "user.name")))

    ;; Invalid keys are quoted
    (is (= "\"user name\"" (quote/maybe-quote-key "user name")))
    (is (= "\"123\"" (quote/maybe-quote-key "123")))
    (is (= "\"key:value\"" (quote/maybe-quote-key "key:value")))
    (is (= "\"[special]\"" (quote/maybe-quote-key "[special]")))
    (is (= "\"user-name\"" (quote/maybe-quote-key "user-name")))

    ;; Keys with escape sequences are quoted and escaped
    (is (= "\"key\\nvalue\"" (quote/maybe-quote-key "key\nvalue")))
    (is (= "\"say \\\"hi\\\"\"" (quote/maybe-quote-key "say \"hi\"")))))
