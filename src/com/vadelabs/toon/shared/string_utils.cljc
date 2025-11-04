(ns com.vadelabs.toon.shared.string-utils
  "String utility functions for TOON parsing and encoding.

  Provides low-level string operations for:
  - Finding closing quotes with escape handling
  - Finding unquoted characters
  - Unescaping string literals"
  (:require))


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
;; String Unescaping
;; ============================================================================

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
