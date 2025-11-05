(ns com.vadelabs.toon.utils
  "Utility functions for TOON encoding and decoding.

  Provides:
  - String search and parsing utilities
  - String escaping/unescaping
  - Value and key quoting logic"
  (:require
    [clojure.string :as str]
    [com.vadelabs.toon.constants :as const])
  #?(:cljs (:require [goog.string])))


;; ============================================================================
;; String Search Utilities
;; ============================================================================

(defn closing-quote
  "Finds the position of the closing quote, respecting escapes.

  Parameters:
    - s: String to search (should start after opening quote)
    - start-pos: Position to start searching (default 0)

  Returns:
    Position of closing quote, or nil if not found"
  ([s]
   (closing-quote s 0))
  ([s start-pos]
   (loop [pos start-pos]
     (if (>= pos (count s))
       nil
       (let [ch (nth s pos)]
         (cond
           (= ch \")
           pos

           (= ch \\)
           (recur (+ pos 2)) ; Skip escaped character

           :else
           (recur (inc pos))))))))


(defn unquoted-char
  "Finds the first occurrence of a character outside quoted sections.

  Parameters:
    - s: String to search
    - target-char: Character to find
    - start-pos: Position to start searching (default 0)

  Returns:
    Position of character, or nil if not found"
  ([s target-char]
   (unquoted-char s target-char 0))
  ([s target-char start-pos]
   (loop [pos start-pos
          in-quotes false]
     (if (>= pos (count s))
       nil
       (let [ch (nth s pos)]
         (cond
           ;; Found target outside quotes
           (and (= ch target-char) (not in-quotes))
           pos

           ;; Handle quotes
           (= ch \")
           (recur (inc pos) (not in-quotes))

           ;; Skip escaped character in quotes
           (and (= ch \\) in-quotes)
           (recur (+ pos 2) in-quotes)

           :else
           (recur (inc pos) in-quotes)))))))


;; ============================================================================
;; String Escaping/Unescaping
;; ============================================================================

(defn escaped
  "Returns an escaped version of the string using JSON-style backslash escaping.

  Single-pass implementation for performance.

  Escape rules:
  - Backslash → \\\\
  - Double quote → \\\"
  - Newline → \\n
  - Carriage return → \\r
  - Tab → \\t

  Parameters:
    - value: String to escape

  Returns:
    Escaped string.

  Examples:
    (escaped \"say \\\"hi\\\"\") ;=> \"say \\\\\\\"hi\\\\\\\"\"
    (escaped \"line1\\nline2\") ;=> \"line1\\\\nline2\"
    (escaped \"C:\\\\path\")    ;=> \"C:\\\\\\\\path\""
  [value]
  (let [sb #?(:clj (StringBuilder. (count value))
              :cljs (goog.string/StringBuffer.))]
    (doseq [c value]
      (case c
        \\ (.append sb "\\\\")
        \" (.append sb "\\\"")
        \newline (.append sb "\\n")
        \return (.append sb "\\r")
        \tab (.append sb "\\t")
        (.append sb c)))
    (.toString sb)))


(defn unescaped
  "Unescapes a string literal.

  Supported escape sequences:
    - \\\\ → \\
    - \\\" → \"
    - \\n → newline
    - \\r → carriage return
    - \\t → tab

  In strict mode, throws on invalid escape sequences.

  Parameters:
    - s: String to unescape
    - strict: Validate escape sequences (default true)

  Returns:
    Unescaped string

  Throws:
    ex-info if invalid escape sequence in strict mode"
  ([s]
   (unescaped s true))
  ([s strict]
   #?(:clj
      (let [sb (StringBuilder.)]
        (loop [pos 0]
          (if (>= pos (count s))
            (.toString sb)
            (let [ch (nth s pos)]
              (if (= ch \\)
                (if (>= (inc pos) (count s))
                  (if strict
                    (throw (ex-info "Invalid escape sequence: trailing backslash"
                                    {:type :invalid-escape
                                     :position pos}))
                    (do
                      (.append sb ch)
                      (recur (inc pos))))
                  (let [next-ch (nth s (inc pos))]
                    (case next-ch
                      \\ (do (.append sb "\\") (recur (+ pos 2)))
                      \" (do (.append sb "\"") (recur (+ pos 2)))
                      \n (do (.append sb "\n") (recur (+ pos 2)))
                      \r (do (.append sb "\r") (recur (+ pos 2)))
                      \t (do (.append sb "\t") (recur (+ pos 2)))
                      ;; Invalid escape
                      (if strict
                        (throw (ex-info (str "Invalid escape sequence: \\" next-ch)
                                        {:type :invalid-escape
                                         :sequence (str "\\" next-ch)
                                         :position pos}))
                        (do
                          (.append sb ch)
                          (.append sb next-ch)
                          (recur (+ pos 2)))))))
                (do
                  (.append sb ch)
                  (recur (inc pos))))))))
      :cljs
      (let [parts (array)]
        (loop [pos 0]
          (if (>= pos (count s))
            (.join parts "")
            (let [ch (nth s pos)]
              (if (= ch \\)
                (if (>= (inc pos) (count s))
                  (if strict
                    (throw (ex-info "Invalid escape sequence: trailing backslash"
                                    {:type :invalid-escape
                                     :position pos}))
                    (do
                      (.push parts ch)
                      (recur (inc pos))))
                  (let [next-ch (nth s (inc pos))]
                    (case next-ch
                      \\ (do (.push parts "\\") (recur (+ pos 2)))
                      \" (do (.push parts "\"") (recur (+ pos 2)))
                      \n (do (.push parts "\n") (recur (+ pos 2)))
                      \r (do (.push parts "\r") (recur (+ pos 2)))
                      \t (do (.push parts "\t") (recur (+ pos 2)))
                      ;; Invalid escape
                      (if strict
                        (throw (ex-info (str "Invalid escape sequence: \\" next-ch)
                                        {:type :invalid-escape
                                         :sequence (str "\\" next-ch)
                                         :position pos}))
                        (do
                          (.push parts ch)
                          (.push parts next-ch)
                          (recur (+ pos 2)))))))
                (do
                  (.push parts ch)
                  (recur (inc pos)))))))))))


;; ============================================================================
;; Pattern Detection Helpers
;; ============================================================================

(defn numeric-like?
  "Returns true if value looks like a number.

  Matches standard numeric patterns (42, -3.14, 1e-6) and
  leading zero patterns (05, 007)."
  [value]
  (or
    ;; Standard numeric pattern: 42, -3.14, 1e-6
    (boolean (re-matches #"^-?\d+(?:\.\d+)?(?:[eE][+-]?\d+)?$" value))
    ;; Leading zeros pattern: 05, 007
    (boolean (re-matches #"^0\d+$" value))))

(defn has-structural-chars?
  "Returns true if value contains structural characters: [ ] { } -"
  [value]
  (boolean (re-find #"[\[\]{}\-]" value)))


;; ============================================================================
;; Value Quoting Logic
;; ============================================================================

(defn needs-quoting?
  "Returns true if a string value needs quoting in TOON format.

  A string needs quoting if it:
  - Is empty or blank
  - Has leading/trailing whitespace
  - Exactly matches reserved literals: 'true', 'false', 'null'
  - Looks like a number (e.g., '42', '-3.14', '1e-6', '05')
  - Contains the active delimiter
  - Contains structural characters: [ ] { } -
  - Contains colon (key-value separator)
  - Contains double quotes or backslashes
  - Contains control characters (newline, tab, carriage return)

  Parameters:
    - value: String value to check
    - delimiter: Delimiter character being used (default: comma)

  Returns:
    Boolean indicating if quoting is needed."
  ([value]
   (needs-quoting? value const/default-delimiter))
  ([value delimiter]
   (or
     ;; Empty or blank strings need quoting
     (str/blank? value)

     ;; Leading/trailing whitespace needs quoting
     (not= value (str/trim value))

     ;; Reserved literals need quoting to avoid ambiguity
     (#{"true" "false" "null"} value)

     ;; Numeric-like strings need quoting
     (numeric-like? value)

     ;; Structural characters need quoting
     (has-structural-chars? value)

     ;; Contains active delimiter
     (str/includes? value delimiter)

     ;; Contains colon (key-value separator)
     (str/includes? value const/colon)

     ;; Contains characters that need escaping
     (str/includes? value const/double-quote)
     (str/includes? value "\\")

     ;; Control characters (newlines, tabs, carriage returns)
     (re-find #"[\n\r\t]" value))))


(defn wrap
  "Wraps a string value in double quotes for safe encoding in TOON format.

  Quoting rules:
  1. Escape special characters using JSON-style backslash escaping
  2. Wrap result in double quotes

  Parameters:
    - value: String value to quote

  Returns:
    Quoted string safe for TOON encoding.

  Examples:
    (quoted \"hello\")         ;=> \"\\\"hello\\\"\"
    (quoted \"say \\\"hi\\\"\") ;=> \"\\\"say \\\\\\\"hi\\\\\\\"\\\"\"
    (quoted \"a,b\")           ;=> \"\\\"a,b\\\"\"
    (quoted \"line1\\nline2\") ;=> \"\\\"line1\\\\nline2\\\"\""
  [value]
  (let [esc (escaped value)]
    (str const/double-quote esc const/double-quote)))


(defn maybe-quote
  "Quotes a string if it needs quoting, otherwise returns it unchanged.

  Parameters:
    - value: String value to potentially quote
    - delimiter: Delimiter character being used (default: comma)

  Returns:
    Original or quoted string.

  Examples:
    (maybe-quote \"simple\")      ;=> \"simple\"
    (maybe-quote \"has, comma\")  ;=> \"\\\"has, comma\\\"\"
    (maybe-quote \"true\")        ;=> \"\\\"true\\\"\" (reserved literal)
    (maybe-quote \"42\")          ;=> \"\\\"42\\\"\" (numeric-like)"
  ([value]
   (maybe-quote value const/default-delimiter))
  ([value delimiter]
   (if (needs-quoting? value delimiter)
     (wrap value)
     value)))


;; ============================================================================
;; Key Quoting Logic
;; ============================================================================

(defn valid-unquoted-key?
  "Returns true if a key can be used without quotes.

  Valid unquoted keys must match the pattern: /^[A-Z_][\\w./]*$/i
  - Start with a letter (A-Z, a-z) or underscore (_)
  - Followed by letters, digits, underscores, dots, or forward slashes

  Note: Forward slashes are allowed to support Clojure namespaced keywords.

  Parameters:
    - key: String key to check

  Returns:
    Boolean indicating if key can be unquoted.

  Examples:
    (valid-unquoted-key? \"name\")        ;=> true
    (valid-unquoted-key? \"user_id\")     ;=> true
    (valid-unquoted-key? \"user.name\")   ;=> true
    (valid-unquoted-key? \"user/id\")     ;=> true (namespaced)
    (valid-unquoted-key? \"user name\")   ;=> false (space)
    (valid-unquoted-key? \"123\")         ;=> false (starts with digit)
    (valid-unquoted-key? \"key:value\")   ;=> false (colon)"
  [key]
  (boolean (re-matches #"^[A-Za-z_][\w./]*$" key)))


(defn maybe-quote-key
  "Quotes a key if it cannot be used unquoted, otherwise returns it unchanged.

  Keys need quoting if they don't match the valid unquoted key pattern.

  Parameters:
    - key: String key to potentially quote

  Returns:
    Original or quoted key.

  Examples:
    (maybe-quote-key \"name\")        ;=> \"name\"
    (maybe-quote-key \"user name\")   ;=> \"\\\"user name\\\"\"
    (maybe-quote-key \"123\")         ;=> \"\\\"123\\\"\"
    (maybe-quote-key \"key:value\")   ;=> \"\\\"key:value\\\"\""
  [key]
  (if (valid-unquoted-key? key)
    key
    (wrap key)))
