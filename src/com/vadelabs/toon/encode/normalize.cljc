(ns com.vadelabs.toon.encode.normalize
  "Data normalization for TOON encoding.

  Converts Clojure data structures to JSON-compatible values:
  - Keywords → strings
  - Symbols → strings
  - Sets → vectors
  - Maps with keyword keys → maps with string keys
  - UUIDs → strings
  - Dates/Instants → ISO-8601 strings
  - NaN/Infinity → nil
  - Functions/vars → nil")


;; ============================================================================
;; Normalization
;; ============================================================================

(defn normalize-value
  "Normalizes a Clojure value to a JSON-compatible representation.

  Normalization rules:
  - nil → nil
  - Booleans → unchanged
  - Strings → unchanged
  - Numbers:
    - Finite → unchanged (with -0 → 0)
    - NaN, Infinity, -Infinity → nil
  - Keywords → strings (e.g., :foo → \"foo\", :user/id → \"user/id\")
  - Symbols → strings (e.g., 'foo → \"foo\")
  - UUIDs → strings
  - Dates/Instants → ISO-8601 strings
  - BigInt/BigDecimal → number or string (if out of safe range)
  - Sets → vectors (sorted for determinism)
  - Maps → maps with string keys (recursively normalized values)
  - Vectors/Lists → vectors (recursively normalized)
  - Functions, vars, undefined → nil

  Parameters:
    - value: Any Clojure value

  Returns:
    JSON-compatible value (nil, boolean, number, string, vector, or map)"
  [value]
  (cond
    ;; nil
    (nil? value)
    nil

    ;; Booleans
    (boolean? value)
    value

    ;; Strings
    (string? value)
    value

    ;; Numbers
    (number? value)
    (cond
      ;; Handle -0 → 0
      (and (zero? value) #?(:clj (Double/isFinite value) :cljs (js/isFinite value)))
      0

      ;; Check for NaN and Infinity
      #?(:clj (not (Double/isFinite value))
         :cljs (not (js/isFinite value)))
      nil

      ;; Normal finite number
      :else
      value)

    ;; Keywords → strings
    (keyword? value)
    (if (namespace value)
      (str (namespace value) "/" (name value))
      (name value))

    ;; Symbols → strings
    (symbol? value)
    (if (namespace value)
      (str (namespace value) "/" (name value))
      (name value))

    ;; UUIDs → strings
    #?@(:clj [(instance? java.util.UUID value)
              (str value)]
        :cljs [(uuid? value)
               (str value)])

    ;; Dates/Instants → ISO-8601 strings
    #?@(:clj [(instance? java.util.Date value)
              (.toInstant value)]
        :cljs [(instance? js/Date value)
               (.toISOString value)])

    #?@(:clj [(instance? java.time.Instant value)
              (.toString value)])

    ;; BigInt/BigDecimal → number or string
    #?@(:clj [(instance? clojure.lang.BigInt value)
              (let [n (bigint value)]
                (if (<= Long/MIN_VALUE n Long/MAX_VALUE)
                  (long n)
                  (str n)))]
        :cljs [(= (type value) js/BigInt)
               (let [n (.toString value)]
                 ;; Try to convert to number if in safe range
                 (if (and (>= (js/Number n) js/Number.MIN_SAFE_INTEGER)
                          (<= (js/Number n) js/Number.MAX_SAFE_INTEGER))
                   (js/Number n)
                   n))])

    #?@(:clj [(instance? java.math.BigDecimal value)
              (let [n (bigdec value)]
                (if (and (<= (Math/abs (.doubleValue n)) Double/MAX_VALUE))
                  (.doubleValue n)
                  (str n)))])

    ;; Sets → sorted vectors
    (set? value)
    (->> value
         (map normalize-value)
         sort
         vec)

    ;; Maps → maps with string keys
    (map? value)
    (into {}
          (map (fn [[k v]]
                 [(normalize-value k) (normalize-value v)]))
          value)

    ;; Vectors → vectors (recursively normalized)
    (vector? value)
    (mapv normalize-value value)

    ;; Lists/Sequences → vectors
    (seq? value)
    (mapv normalize-value value)

    ;; Functions, vars, undefined → nil
    :else
    nil))


;; ============================================================================
;; Type Guards
;; ============================================================================

(defn primitive?
  [value]
  (or (nil? value) (boolean? value) (number? value) (string? value)))


(defn json-array?
  [value]
  (vector? value))


(defn json-object?
  [value]
  (map? value))


(defn array-of-primitives?
  [value]
  (and (vector? value) (every? primitive? value)))


(defn array-of-objects?
  [value]
  (and (vector? value) (seq value) (every? json-object? value)))


(defn array-of-arrays?
  [value]
  (and (vector? value) (seq value) (every? json-array? value)))
