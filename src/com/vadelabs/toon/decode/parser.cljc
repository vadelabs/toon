(ns com.vadelabs.toon.decode.parser
  "Token parsing functions for TOON format.

  Parses array headers, delimited values, primitive tokens, and keys."
  (:require
    [clojure.string :as str]
    [com.vadelabs.toon.shared.string-utils :as str-utils]))


;; ============================================================================
;; Constants
;; ============================================================================

(def ^:private numeric-pattern
  "Regex pattern for numeric literals (integers and decimals)."
  #"^-?\d+(\.\d+)?$")


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
                      :input s})))
   (let [content-start 1
         close-pos (str-utils/closing-quote s content-start)]
     (when-not close-pos
       (throw (ex-info "Unterminated string literal"
                       {:type :unterminated-string
                        :input s})))
     (let [content (subs s content-start close-pos)]
       (str-utils/unescaped content strict)))))
;; ============================================================================

(defn number
  "Parses a numeric string to a number.

  Parameters:
    - s: String to parse

  Returns:
    Number (double) or nil if not a valid number"
  [s]
  (when (re-matches numeric-pattern s)
    #?(:clj (Double/parseDouble s)
       :cljs (js/parseFloat s))))


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
          current ""
          in-quotes false
          values []]
     (if (>= pos (count input))
       ;; End of string
       (if (empty? current)
         values
         (conj values (str/trim current)))
       (let [ch (nth input pos)]
         (cond
           ;; Handle backslash escapes in quotes
           (and (= ch \\) in-quotes (< (inc pos) (count input)))
           (let [next-ch (nth input (inc pos))]
             (recur (+ pos 2)
                    (str current ch next-ch)
                    in-quotes
                    values))

           ;; Toggle quote state
           (= ch \")
           (recur (inc pos)
                  (str current ch)
                  (not in-quotes)
                  values)

           ;; Delimiter outside quotes: split here
           (and (= (str ch) delimiter) (not in-quotes))
           (recur (inc pos)
                  ""
                  in-quotes
                  (conj values (str/trim current)))

           ;; Regular character
           :else
           (recur (inc pos)
                  (str current ch)
                  in-quotes
                  values)))))))


;; ============================================================================
;; Bracket Segment Parsing
;; ============================================================================

(defn bracket-segment
  "Parses bracket segment to extract length and delimiter.

  Format: [#?N<delim?>]
    - Optional # length marker
    - Required N (numeric length)
    - Optional delimiter (tab or pipe char in brackets)

  Parameters:
    - bracket-content: Content inside brackets (without [ ])

  Returns:
    Map with {:length, :delimiter, :has-length-marker}

  Example:
    (parse-bracket-segment \"3\") => {:length 3 :delimiter \",\" :has-length-marker false}
    (parse-bracket-segment \"#3\") => {:length 3 :delimiter \",\" :has-length-marker true}
    (parse-bracket-segment \"3|\") => {:length 3 :delimiter \"|\" :has-length-marker false}
    (parse-bracket-segment \"3\\t\") => {:length 3 :delimiter \"\\t\" :has-length-marker false}"
  [bracket-content]
  (let [has-marker (str/starts-with? bracket-content "#")
        after-marker (if has-marker (subs bracket-content 1) bracket-content)
        ;; Check for explicit delimiter at end
        last-char (when (seq after-marker) (last after-marker))
        has-delimiter (or (= last-char \|) (= last-char \tab))
        delimiter (cond
                    (= last-char \|) "|"
                    (= last-char \tab) "\t"
                    :else ",")
        numeric-part (if has-delimiter
                       (subs after-marker 0 (dec (count after-marker)))
                       after-marker)
        length (number numeric-part)]
    (when-not length
      (throw (ex-info "Invalid array length in bracket segment"
                      {:type :invalid-bracket-segment
                       :input bracket-content})))
    {:length (int length)
     :delimiter delimiter
     :has-length-marker has-marker}))


;; ============================================================================
;; Array Header Parsing
;; ============================================================================

(defn array-header-line
  "Parses an array header line.

  Format: key?[#?N<delim?>]{fields}?: inline-values?

  Parameters:
    - line: Header line string

  Returns:
    Map with {:key, :length, :delimiter, :fields, :has-length-marker, :inline-values}

  Example:
    (parse-array-header-line \"[3]:\")
    => {:length 3 :delimiter \",\" :has-length-marker false}

    (parse-array-header-line \"items[2]{id,name}:\")
    => {:key \"items\" :length 2 :delimiter \",\" :fields [\"id\" \"name\"] ...}

    (parse-array-header-line \"[3]: a,b,c\")
    => {:length 3 :delimiter \",\" :inline-values \"a,b,c\" ...}"
  [line]
  (let [;; Find bracket segment
        open-bracket (str/index-of line "[")
        close-bracket (str/index-of line "]")]
    (when (or (nil? open-bracket) (nil? close-bracket))
      (throw (ex-info "Array header must contain bracket segment"
                      {:type :invalid-array-header
                       :line line})))
    (let [;; Extract key prefix (before [)
            key-part (when (> open-bracket 0)
                       (str/trim (subs line 0 open-bracket)))
            ;; Parse bracket segment
            bracket-content (subs line (inc open-bracket) close-bracket)
            bracket-info (bracket-segment bracket-content)
            ;; Extract content after ]
            after-bracket (subs line (inc close-bracket))
            ;; Check for field list {field1,field2}
            open-brace (str/index-of after-bracket "{")
            close-brace (str/index-of after-bracket "}")
            fields (when (and open-brace close-brace)
                     (let [fields-str (subs after-bracket (inc open-brace) close-brace)]
                       (vec (map str/trim (str/split fields-str #",")))))
            ;; Extract content after fields (or after ])
            after-fields (if fields
                           (subs after-bracket (inc close-brace))
                           after-bracket)
            ;; Check for colon and inline values
            colon-pos (str/index-of after-fields ":")
            inline-values (when colon-pos
                            (let [after-colon (str/trim (subs after-fields (inc colon-pos)))]
                              (when-not (empty? after-colon)
                                after-colon)))]
        (cond-> bracket-info
          key-part (assoc :key key-part)
          fields (assoc :fields fields)
          inline-values (assoc :inline-values inline-values)))))


;; ============================================================================
;; Key Token Parsing
;; ============================================================================

(defn key-token
  "Parses a key token (before colon).

  Handles quoted and unquoted keys.

  Parameters:
    - key-str: Key string (may include trailing colon)

  Returns:
    Key string (unquoted and unescaped if quoted)

  Example:
    (parse-key-token \"name\") => \"name\"
    (parse-key-token \"\\\"user name\\\"\") => \"user name\""
  [key-str]
  (let [trimmed (str/trim key-str)
        ;; Remove trailing colon if present
        without-colon (if (str/ends-with? trimmed ":")
                        (subs trimmed 0 (dec (count trimmed)))
                        trimmed)
        key-only (str/trim without-colon)]
    (if (str/starts-with? key-only "\"")
      ;; Quoted key: parse as string literal
      (string-literal key-only)
      ;; Unquoted key: return as-is
      key-only)))
