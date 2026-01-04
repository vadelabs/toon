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
  - Functions/vars → nil
  - Objects implementing ToJson protocol → custom serialization")

;; ============================================================================
;; ToJson Protocol
;; ============================================================================

(defprotocol ToJson
  "Protocol for custom TOON/JSON serialization.

  Objects implementing this protocol can control how they are serialized
  during TOON encoding. The -to-json method is called before any other
  type-specific normalization.

  Example:
    (defrecord Person [first-name last-name]
      ToJson
      (-to-json [_]
        {:name (str first-name \" \" last-name)}))

    (encode (->Person \"Alice\" \"Smith\"))
    ;; => \"name: Alice Smith\""
  (-to-json [this] "Returns the JSON-compatible representation of this value."))

;; ============================================================================
;; Normalization
;; ============================================================================

(defn- to-json-result
  "Returns the -to-json result if value implements ToJson and doesn't return itself.
   Returns nil if value doesn't implement ToJson or returns itself (fall through case)."
  [value]
  (when (satisfies? ToJson value)
    (let [result (-to-json value)]
      (when-not (identical? result value)
        ;; Return a wrapper to distinguish nil result from "no ToJson"
        {::result result}))))

(defn normalize-value
  "Normalizes a Clojure value to a JSON-compatible representation.

  Normalization rules (in order of precedence):
  - nil → nil
  - ToJson protocol → calls -to-json and normalizes result
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
    - depth: (optional) Current nesting depth (default: 0)
    - max-depth: (optional) Maximum nesting depth (default: 1000)

  Returns:
    JSON-compatible value (nil, boolean, number, string, vector, or map)

  Throws:
    ex-info if max-depth is exceeded to prevent stack overflow"
  ([value]
   (normalize-value value 0 1000))
  ([value depth max-depth]
   (when (> depth max-depth)
     (throw (ex-info "Maximum nesting depth exceeded during normalization"
                     {:type :max-depth-exceeded
                      :depth depth
                      :max-depth max-depth
                      :suggestion "Reduce nesting depth or increase max-depth parameter"})))
   ;; ToJson protocol - custom serialization first
   ;; When -to-json returns self, fall through to process as map (like TypeScript)
   (if-let [to-json-res (to-json-result value)]
     (normalize-value (::result to-json-res) (inc depth) max-depth)
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
                 (.toString (.toInstant value))]
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
                   (if (<= (Math/abs (.doubleValue n)) Double/MAX_VALUE)
                     (.doubleValue n)
                     (str n)))])

       ;; Sets → sorted vectors
       (set? value)
       (->> value
            (map #(normalize-value % (inc depth) max-depth))
            sort
            vec)

       ;; Maps → maps with string keys
       (map? value)
       (into {}
             (map (fn [[k v]]
                    [(normalize-value k (inc depth) max-depth)
                     (normalize-value v (inc depth) max-depth)]))
             value)

       ;; Vectors → vectors (recursively normalized)
       (vector? value)
       (mapv #(normalize-value % (inc depth) max-depth) value)

       ;; Lists/Sequences → vectors
       (seq? value)
       (mapv #(normalize-value % (inc depth) max-depth) value)

       ;; Functions, vars, undefined → nil
       :else
       nil))))

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
