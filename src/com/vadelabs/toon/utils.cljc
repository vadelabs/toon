(ns com.vadelabs.toon.utils
  "Utility functions for TOON encoding and decoding.

  Provides:
  - String search and parsing utilities
  - String escaping/unescaping
  - Value and key quoting logic"
  (:require
    [clojure.string :as str]
    [com.vadelabs.toon.constants :as const])
  #?(:cljs
     (:require
       [goog.string])))


;; ============================================================================
;; Regex Pattern Constants
;; ============================================================================

(def ^:private numeric-pattern
  "Pattern for standard numeric literals: 42, -3.14, 1e-6, 1.23e+10
  Matches optional sign, digits, optional decimal, optional exponent"
  #"^-?\d+(?:\.\d+)?(?:[eE][+-]?\d+)?$")

(def ^:private leading-zero-pattern
  "Pattern for numbers with leading zeros: 05, 007, 0123
  These need quoting to preserve the leading zeros"
  #"^0\d+$")

(def ^:private structural-chars-pattern
  "Pattern for TOON structural characters: [ ] { } -
  Values containing these need quoting for clarity"
  #"[\[\]{}\-]")

(def ^:private control-chars-pattern
  "Pattern for control characters: newline, carriage return, tab
  Values containing these need quoting and escaping"
  #"[\n\r\t]")

(def ^:private identifier-segment-pattern
  "Pattern for safe identifier segments (no dots or slashes).
  Must start with letter or underscore, followed by word characters.
  Used for key collapsing and path expansion safety checks."
  #"^[A-Za-z_]\w*$")

(def ^:private valid-unquoted-key-pattern
  "Pattern for valid unquoted keys in TOON format.
  Must start with letter or underscore, can contain word chars, dots, slashes.
  Slashes support Clojure namespaced keywords (e.g., 'user/id')"
  #"^[A-Za-z_][\w./]*$")


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
   (let [builder #?(:clj (StringBuilder.)
                    :cljs (array))
         append! #?(:clj (fn [sb text] (.append sb text))
                    :cljs (fn [arr text] (.push arr text)))]
     (loop [pos 0]
       (if (>= pos (count s))
         #?(:clj (.toString builder)
            :cljs (.join builder ""))
         (let [ch (nth s pos)]
           (if (= ch \\)
             ;; Handle escape sequence
             (if (>= (inc pos) (count s))
               ;; Trailing backslash
               (if strict
                 (throw (ex-info "Invalid escape sequence: string ends with unescaped backslash"
                                 {:type :invalid-escape
                                  :position pos
                                  :suggestion "Escape the backslash with \\\\ or remove the trailing backslash"
                                  :context (subs s (max 0 (- pos 10)) (count s))}))
                 (do
                   (append! builder ch)
                   (recur (inc pos))))
               ;; Process escape character
               (let [next-ch (nth s (inc pos))]
                 (case next-ch
                   \\ (do (append! builder "\\") (recur (+ pos 2)))
                   \" (do (append! builder "\"") (recur (+ pos 2)))
                   \n (do (append! builder "\n") (recur (+ pos 2)))
                   \r (do (append! builder "\r") (recur (+ pos 2)))
                   \t (do (append! builder "\t") (recur (+ pos 2)))
                   ;; Invalid escape
                   (if strict
                     (throw (ex-info (str "Invalid escape sequence: \\" next-ch)
                                     {:type :invalid-escape
                                      :sequence (str "\\" next-ch)
                                      :position pos
                                      :suggestion (str "Use one of: \\\\, \\\", \\n, \\r, \\t (found: \\" next-ch ")")
                                      :valid-escapes ["\\\\", "\\\"", "\\n", "\\r", "\\t"]
                                      :context (subs s (max 0 (- pos 5)) (min (count s) (+ pos 10)))}))
                     (do
                       (append! builder ch)
                       (append! builder next-ch)
                       (recur (+ pos 2)))))))
             ;; Regular character
             (do
               (append! builder ch)
               (recur (inc pos))))))))))


;; ============================================================================
;; Pattern Detection Helpers
;; ============================================================================

(defn numeric-like?
  "Returns true if value looks like a number.

  Matches standard numeric patterns (42, -3.14, 1e-6) and
  leading zero patterns (05, 007)."
  [value]
  (boolean (or (re-matches numeric-pattern value)
               (re-matches leading-zero-pattern value))))


(defn has-structural-chars?
  "Returns true if value contains structural characters: [ ] { } -"
  [value]
  (boolean (re-find structural-chars-pattern value)))


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
     (re-find control-chars-pattern value))))


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

(defn identifier-segment?
  "Returns true if a key segment is a valid identifier for safe collapsing/expansion.

  Identifier segments are more restrictive than unquoted keys:
  - Must start with a letter (A-Z, a-z) or underscore (_)
  - Followed only by letters, digits, or underscores (no dots or slashes)
  - Used for safe key collapsing and path expansion

  Parameters:
    - segment: String segment to check

  Returns:
    Boolean indicating if segment is a valid identifier.

  Examples:
    (identifier-segment? \"name\")        ;=> true
    (identifier-segment? \"user_id\")     ;=> true
    (identifier-segment? \"user123\")     ;=> true
    (identifier-segment? \"user.name\")   ;=> false (contains dot)
    (identifier-segment? \"user/id\")     ;=> false (contains slash)
    (identifier-segment? \"user name\")   ;=> false (contains space)
    (identifier-segment? \"123\")         ;=> false (starts with digit)"
  [segment]
  (some? (re-matches identifier-segment-pattern segment)))


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
  (some? (re-matches valid-unquoted-key-pattern key)))


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
