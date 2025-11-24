(ns com.vadelabs.toon.decode.parser
  "Token parsing functions for TOON format.

  Parses array headers, delimited values, primitive tokens, and keys."
  (:require
    [clojure.string :as str]
    [com.vadelabs.toon.utils :as str-utils]))


;; ============================================================================
;; Constants
;; ============================================================================

(def ^:private numeric-pattern
  "Regex pattern for numeric literals (integers and decimals)."
  #"^-?\d+(\.\d+)?$")

(def ^:private comma-pattern
  "Regex pattern for splitting on commas."
  #",")


;; ============================================================================
;; String Literal Parsing
;; ============================================================================

(defn string-literal
  "Parses a quoted string literal.

  Expects string to start with double quote.
  Finds closing quote and unescapes content.

  Parameters:
    - s: String starting with opening quote
    - strict: Validate escape sequences (default true)

  Returns:
    Unescaped string content

  Throws:
    ex-info if no closing quote found or invalid escapes"
  ([s]
   (string-literal s true))
  ([s strict]
   (when-not (str/starts-with? s "\"")
     (throw (ex-info "String literal must start with double quote"
                     {:type :invalid-string-literal
                      :input s
                      :suggestion "Wrap the string in double quotes: \"your string here\""
                      :example "\"hello world\""})))
   (let [content-start 1
         close-pos (str-utils/closing-quote s content-start)]
     (when-not close-pos
       (throw (ex-info "Unterminated string literal: missing closing double quote"
                       {:type :unterminated-string
                        :input s
                        :suggestion "Add a closing double quote at the end of the string"
                        :example "\"hello world\""})))
     (let [content (subs s content-start close-pos)]
       (str-utils/unescaped content strict)))))
;; ============================================================================

(defn number
  "Parses a numeric string to a number.

  Parameters:
    - s: String to parse

  Returns:
    Number (double) or nil if not a valid number

  Note:
    Normalizes negative zero to positive zero per TOON v1.4 spec"
  [s]
  (when (re-matches numeric-pattern s)
    (let [parsed #?(:clj (Double/parseDouble s)
                    :cljs (js/parseFloat s))]
      ;; Normalize negative zero to positive zero (v1.4 requirement)
      (if (zero? parsed) 0.0 parsed))))


(defn primitive-token
  "Parses a primitive token to its value.

  Handles:
    - null → nil
    - true → true
    - false → false
    - Numbers → numeric value
    - Quoted strings → unescaped string
    - Unquoted strings → string as-is

  Parameters:
    - token: String token to parse
    - strict: Validate escape sequences in strings (default true)

  Returns:
    Parsed value (nil, boolean, number, or string)"
  ([token]
   (primitive-token token true))
  ([token strict]
   (let [trimmed (str/trim token)]
     (cond
       ;; null literal
       (= trimmed "null")
       nil

       ;; Boolean literals
       (= trimmed "true")
       true

       (= trimmed "false")
       false

       ;; Quoted string
       (str/starts-with? trimmed "\"")
       (string-literal trimmed strict)

       ;; Try parsing as number, otherwise treat as unquoted string
       :else
       (if-let [parsed-num (number trimmed)]
         parsed-num
         trimmed)))))


;; ============================================================================
;; Delimited Value Parsing
;; ============================================================================

(defn- escaped-char-pair
  "Processes an escaped character pair in quoted context.

  Returns updated state map with position advanced by 2."
  [pos current ch next-ch in-quotes values]
  {:pos (+ pos 2)
   :current (doto current (.append ch) (.append next-ch))
   :in-quotes in-quotes
   :values values})


(defn- quote-char
  "Processes a quote character, toggling quote state.

  Returns updated state map."
  [pos current ch in-quotes values]
  {:pos (inc pos)
   :current (doto current (.append ch))
   :in-quotes (not in-quotes)
   :values values})


(defn- delimiter-char
  "Processes a delimiter character outside quotes.

  Splits current buffer and starts new token.
  Returns updated state map."
  [pos current in-quotes values]
  {:pos (inc pos)
   :current #?(:clj (StringBuilder.)
               :cljs (goog.string/StringBuffer.))
   :in-quotes in-quotes
   :values (conj values (str/trim (.toString current)))})


(defn- regular-char
  "Processes a regular character.

  Returns updated state map."
  [pos current ch in-quotes values]
  {:pos (inc pos)
   :current (doto current (.append ch))
   :in-quotes in-quotes
   :values values})


(defn- final-token
  "Extracts final token from buffer if non-empty.

  Returns complete values vector."
  [current values]
  (if (zero? #?(:clj (.length current)
                :cljs (.-length current)))
    values
    (conj values (str/trim (.toString current)))))


(defn delimited-values
  "Splits a string by delimiter, respecting quoted sections.

  Parameters:
    - input: String to split
    - delimiter: Delimiter character (default \",\")

  Returns:
    Vector of trimmed token strings

  Example:
    (parse-delimited-values \"a,b,\\\"c,d\\\",e\" \",\")
    => [\"a\" \"b\" \"\\\"c,d\\\"\" \"e\"]"
  ([input]
   (delimited-values input ","))
  ([input delimiter]
   (loop [pos 0
          current #?(:clj (StringBuilder.)
                     :cljs (goog.string/StringBuffer.))
          in-quotes false
          values []]
     (if (>= pos (count input))
       ;; End of string - extract final token
       (final-token current values)
       (let [ch (nth input pos)
             next-state (cond
                          ;; Escaped character in quotes
                          (and (= ch \\) in-quotes (< (inc pos) (count input)))
                          (escaped-char-pair pos current ch (nth input (inc pos)) in-quotes values)

                          ;; Quote character - toggle state
                          (= ch \")
                          (quote-char pos current ch in-quotes values)

                          ;; Delimiter outside quotes - split here
                          (and (= (str ch) delimiter) (not in-quotes))
                          (delimiter-char pos current in-quotes values)

                          ;; Regular character - append
                          :else
                          (regular-char pos current ch in-quotes values))]
         (recur (:pos next-state)
                (:current next-state)
                (:in-quotes next-state)
                (:values next-state)))))))


;; ============================================================================
;; Bracket Segment Parsing
;; ============================================================================

(defn bracket-segment
  "Parses bracket segment to extract length and delimiter.

  Format: [N<delim?>]
    - Required N (numeric length)
    - Optional delimiter (tab or pipe char in brackets)

  Parameters:
    - bracket-content: Content inside brackets (without [ ])

  Returns:
    Map with {:length, :delimiter}

  Example:
    (parse-bracket-segment \"3\") => {:length 3 :delimiter \",\"}
    (parse-bracket-segment \"3|\") => {:length 3 :delimiter \"|\"}
    (parse-bracket-segment \"3\\t\") => {:length 3 :delimiter \"\\t\"}"
  [bracket-content]
  (let [;; Check for explicit delimiter at end
        last-char (when (seq bracket-content) (last bracket-content))
        has-delimiter (or (= last-char \|) (= last-char \tab))
        delimiter (cond
                    (= last-char \|) "|"
                    (= last-char \tab) "\t"
                    :else ",")
        numeric-part (if has-delimiter
                       (subs bracket-content 0 (dec (count bracket-content)))
                       bracket-content)
        length (number numeric-part)]
    ;; Validate empty bracket content
    (when (str/blank? numeric-part)
      (throw (ex-info "Array length cannot be empty in bracket segment"
                      {:type :empty-bracket-segment
                       :input bracket-content
                       :suggestion "Specify array length: [3] or [0] for empty arrays"
                       :examples ["[3]" "[0]" "[10|]"]})))
    ;; Validate that length is a valid number
    (when-not length
      (throw (ex-info "Invalid array length in bracket segment: must be a number"
                      {:type :invalid-bracket-segment
                       :input bracket-content
                       :parsed-value numeric-part
                       :suggestion "Use a numeric length: [3] or [10|] or [2\\t]"
                       :examples ["[3]" "[10|]" "[2\\t]"]})))
    ;; Validate non-negative length
    (when (neg? length)
      (throw (ex-info "Array length must be non-negative"
                      {:type :negative-array-length
                       :input bracket-content
                       :parsed-value length
                       :suggestion "Use a non-negative integer: [0], [3], [100]"
                       :examples ["[0]" "[3]" "[100]"]})))
    {:length (int length)
     :delimiter delimiter}))


;; ============================================================================
;; Array Header Parsing
;; ============================================================================

(defn- bracket-positions
  "Finds bracket positions in array header line.

  Returns map with {:open-bracket, :close-bracket}.
  Throws if brackets not found."
  [line]
  (let [open-bracket (str/index-of line "[")
        close-bracket (str/index-of line "]")]
    (when (or (nil? open-bracket) (nil? close-bracket))
      (throw (ex-info "Array header must contain bracket segment with length"
                      {:type :invalid-array-header
                       :line line
                       :suggestion "Add array length in brackets: key[N]: values"
                       :examples ["tags[3]: a,b,c" "[2]{id,name}:" "items[5]:"]})))
    {:open-bracket open-bracket
     :close-bracket close-bracket}))


(defn- key-prefix
  "Extracts optional key prefix before opening bracket.

  Returns trimmed key string or nil if no key present."
  [line open-bracket]
  (when (> open-bracket 0)
    (str/trim (subs line 0 open-bracket))))


(defn- field-list
  "Extracts optional field list from brace segment.

  Returns vector of field names or nil if no braces found."
  [after-bracket]
  (let [open-brace (str/index-of after-bracket "{")
        close-brace (str/index-of after-bracket "}")]
    (when (and open-brace close-brace)
      (->> (subs after-bracket (inc open-brace) close-brace)
           (#(str/split % comma-pattern))
           (mapv str/trim)))))


(defn- inline-values
  "Extracts optional inline values after colon.

  Returns trimmed inline value string or nil if none present."
  [after-fields]
  (when-let [colon-pos (str/index-of after-fields ":")]
    (some-> (subs after-fields (inc colon-pos))
            str/trim
            not-empty)))


(defn array-header-line
  "Parses an array header line.

  Format: key?[#?N<delim?>]{fields}?: inline-values?

  Parameters:
    - line: Header line string

  Returns:
    Map with {:key, :length, :delimiter, :fields, :inline-values}

  Example:
    (parse-array-header-line \"[3]:\")
    => {:length 3 :delimiter \",\"}

    (parse-array-header-line \"items[2]{id,name}:\")
    => {:key \"items\" :length 2 :delimiter \",\" :fields [\"id\" \"name\"] ...}

    (parse-array-header-line \"[3]: a,b,c\")
    => {:length 3 :delimiter \",\" :inline-values \"a,b,c\" ...}"
  [line]
  (let [{:keys [open-bracket close-bracket]} (bracket-positions line)
        key-part (key-prefix line open-bracket)
        bracket-content (subs line (inc open-bracket) close-bracket)
        bracket-info (bracket-segment bracket-content)
        after-bracket (subs line (inc close-bracket))
        fields (field-list after-bracket)
        after-fields (if fields
                       (subs after-bracket (inc (str/index-of after-bracket "}")))
                       after-bracket)
        inline-vals (inline-values after-fields)]
    (cond-> bracket-info
      key-part (assoc :key key-part)
      fields (assoc :fields fields)
      inline-vals (assoc :inline-values inline-vals))))


;; ============================================================================
;; Key Token Parsing
;; ============================================================================

(defn key-token
  "Parses a key token (before colon).

  Handles quoted and unquoted keys.

  Parameters:
    - key-str: Key string (may include trailing colon)

  Returns:
    Map with {:key \"parsed-key\" :was-quoted boolean}

  Example:
    (parse-key-token \"name\") => {:key \"name\" :was-quoted false}
    (parse-key-token \"\\\"user name\\\"\") => {:key \"user name\" :was-quoted true}"
  [key-str]
  (let [trimmed (str/trim key-str)
        ;; Remove trailing colon if present
        without-colon (if (str/ends-with? trimmed ":")
                        (subs trimmed 0 (dec (count trimmed)))
                        trimmed)
        key-only (str/trim without-colon)
        is-quoted? (str/starts-with? key-only "\"")]
    (if is-quoted?
      ;; Quoted key: parse as string literal
      {:key (string-literal key-only)
       :was-quoted true}
      ;; Unquoted key: return as-is
      {:key key-only
       :was-quoted false})))
