(ns com.vadelabs.toon.decode.scanner
  "Line scanning and parsing for TOON format.

  Converts TOON text into structured line objects with depth tracking.
  Provides cursor-based navigation for parsing."
  (:require
    [clojure.string :as str]))


;; ============================================================================
;; Data Structures
;; ============================================================================

(defn parsed-line
  [raw depth indent content line-number]
  {:raw raw :depth depth :indent indent
   :content content :line-number line-number})


(defn blank-line-info
  [line-number indent depth]
  {:line-number line-number :indent indent :depth depth})


(defn scan-result
  [lines blank-lines]
  {:lines lines :blank-lines blank-lines})


;; ============================================================================
;; Line Scanning
;; ============================================================================

(defn- count-leading-spaces
  "Counts leading spaces in a string.

  Returns number of spaces before first non-space character."
  [s]
  (-> (re-find #"^( *)" s)
      second
      count))


(defn- blank-line?
  "Checks if a line is blank (only whitespace)."
  [s]
  (or (str/blank? s)
      (every? #{\space \tab} s)))


(defn- validate-indentation-strict
  "Validates indentation in strict mode.

  Throws ex-info if:
  - Line contains tabs
  - Indentation is not a multiple of indent-size

  Parameters:
    - line: Line string
    - line-number: 1-indexed line number
    - indent: Number of leading spaces
    - indent-size: Expected indentation unit

  Returns:
    nil on success, throws on error"
  [line line-number indent indent-size]
  (when (str/includes? line "\t")
    (throw (ex-info (str "Tabs not allowed for indentation in strict mode (line " line-number ")")
                    {:type :invalid-indentation
                     :line-number line-number
                     :line line
                     :suggestion "Replace tabs with spaces for indentation"
                     :note "Use :strict false option to allow tabs"})))

  (when-not (zero? (mod indent indent-size))
    (throw (ex-info (str "Indentation must be multiple of " indent-size " spaces (line " line-number ")")
                    {:type :invalid-indentation
                     :line-number line-number
                     :indent indent
                     :indent-size indent-size
                     :suggestion (str "Use " (* indent-size (quot (+ indent indent-size -1) indent-size)) " spaces instead of " indent)
                     :valid-indents (vec (map #(* % indent-size) (range 0 5)))}))))


(defn to-parsed-lines
  "Parses TOON text into structured lines.

  Splits input on newlines, computes depth from indentation,
  and tracks blank lines separately.

  Parameters:
    - input: TOON text string
    - indent-size: Number of spaces per indentation level (default 2)
    - strict: Enable strict mode validation (default true)

  Returns:
    ScanResult with {:lines, :blank-lines}

  Strict mode validates:
    - No tabs in indentation
    - Indentation is multiple of indent-size"
  ([input]
   (to-parsed-lines input 2 true))
  ([input indent-size]
   (to-parsed-lines input indent-size true))
  ([input indent-size strict]
   (let [;; Split on newlines, preserving empty strings (use -1 limit)
         raw-lines (if (empty? input)
                     []
                     (str/split input #"\n" -1))]
     (loop [remaining raw-lines
            line-num 1
            lines []
            blank-lines []]
       (if (empty? remaining)
         (scan-result lines blank-lines)
         (let [line (first remaining)
               indent (count-leading-spaces line)
               depth (quot indent indent-size)
               content (str/triml line)]
           (if (blank-line? line)
             ;; Blank line: track separately
             (recur (rest remaining)
                    (inc line-num)
                    lines
                    (conj blank-lines (blank-line-info line-num indent depth)))
             ;; Content line: validate and parse
             (do
               (when strict
                 (validate-indentation-strict line line-num indent indent-size))
               (recur (rest remaining)
                      (inc line-num)
                      (conj lines (parsed-line line depth indent content line-num))
                      blank-lines)))))))))


;; ============================================================================
;; LineCursor - Iterator for line navigation
;; ============================================================================

(defrecord LineCursor
  [lines blank-lines position]

  Object

  (toString
    [_]
    (str "LineCursor{position: " position ", total: " (count lines) "}")))


(defn create-cursor
  [lines blank-lines]
  (->LineCursor lines blank-lines 0))


(defn cursor-from-scan-result
  [{:keys [lines blank-lines]}]
  (create-cursor lines blank-lines))


(defn peek-cursor
  [^LineCursor cursor]
  (let [pos (:position cursor)
        lines (:lines cursor)]
    (when (< pos (count lines))
      (nth lines pos))))


(defn next-cursor
  [^LineCursor cursor]
  (let [line (peek-cursor cursor)]
    (if line
      [line (->LineCursor (:lines cursor)
                          (:blank-lines cursor)
                          (inc (:position cursor)))]
      [nil cursor])))


(defn advance-cursor
  ([cursor] (advance-cursor cursor 1))
  ([^LineCursor cursor n]
   (->LineCursor (:lines cursor)
                 (:blank-lines cursor)
                 (+ (:position cursor) n))))


(defn at-end?
  [^LineCursor cursor]
  (>= (:position cursor) (count (:lines cursor))))


(defn peek-at-depth
  [^LineCursor cursor target-depth]
  (let [line (peek-cursor cursor)]
    (when (and line (= (:depth line) target-depth))
      line)))


(defn has-more-at-depth?
  [^LineCursor cursor target-depth]
  (some? (peek-at-depth cursor target-depth)))


(defn get-blank-lines-in-range
  [^LineCursor cursor start-line end-line]
  (let [blank-lines (:blank-lines cursor)]
    (filterv #(and (>= (:line-number %) start-line)
                   (<= (:line-number %) end-line))
             blank-lines)))
