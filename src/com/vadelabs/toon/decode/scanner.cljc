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
  "Create a parsed line record."
  [raw depth indent content line-number]
  {:raw raw :depth depth :indent indent
   :content content :line-number line-number})


(defn blank-line-info
  "Create blank line info record."
  [line-number indent depth]
  {:line-number line-number :indent indent :depth depth})


(defn scan-result
  "Create scan result containing lines and blank lines."
  [lines blank-lines]
  {:lines lines :blank-lines blank-lines})


;; ============================================================================
;; Line Parsing Helpers
;; ============================================================================

(defn- count-leading-spaces
  "Counts leading spaces in a string."
  [s]
  (- (count s) (count (str/triml s))))


(defn- blank-line?
  "Checks if a line is blank (only whitespace)."
  [s]
  (or (nil? s)
      (str/blank? s)
      (every? #{\space \tab} s)))


(defn- validate-indentation!
  "Validates indentation in strict mode. Throws on error."
  [line line-number indent indent-size]
  (when (str/includes? line "\t")
    (throw (ex-info (str "Tabs not allowed for indentation in strict mode (line " line-number ")")
                    {:type :invalid-indentation
                     :line-number line-number
                     :line line})))
  (when-not (zero? (mod indent indent-size))
    (throw (ex-info (str "Indentation must be multiple of " indent-size " spaces (line " line-number ")")
                    {:type :invalid-indentation
                     :line-number line-number
                     :indent indent
                     :indent-size indent-size}))))


;; ============================================================================
;; Public API
;; ============================================================================

(defn to-parsed-lines
  "Parse TOON text into structured lines.

  Parameters:
    - input: TOON text string
    - indent-size: Spaces per indent level (default: 2)
    - strict: Enable strict validation (default: true)

  Returns:
    ScanResult with {:lines, :blank-lines}"
  ([input]
   (to-parsed-lines input 2 true))
  ([input indent-size]
   (to-parsed-lines input indent-size true))
  ([input indent-size strict]
   (let [raw-lines (if (empty? input)
                     []
                     (str/split input #"\n" -1))]
     (loop [remaining raw-lines
            line-num 0
            lines []
            blank-lines []]
       (if (empty? remaining)
         (scan-result lines blank-lines)
         (let [line (first remaining)
               line-num' (inc line-num)
               indent (count-leading-spaces (or line ""))
               depth (quot indent indent-size)]
           (if (blank-line? line)
             (recur (rest remaining)
                    line-num'
                    lines
                    (conj blank-lines (blank-line-info line-num' indent depth)))
             (do
               (when strict
                 (validate-indentation! line line-num' indent indent-size))
               (recur (rest remaining)
                      line-num'
                      (conj lines (parsed-line line depth indent (str/trim line) line-num'))
                      blank-lines)))))))))


;; ============================================================================
;; LineCursor - Iterator for line navigation
;; ============================================================================

(defrecord LineCursor
  [lines blank-lines position])


(defn create-cursor
  "Create a cursor from lines and blank-lines vectors."
  [lines blank-lines]
  (->LineCursor lines blank-lines 0))


(defn cursor-from-scan-result
  "Create a cursor from a scan result."
  [{:keys [lines blank-lines]}]
  (create-cursor lines blank-lines))


(defn peek-cursor
  "Get current line without advancing."
  [cursor]
  (let [pos (:position cursor)
        lines (:lines cursor)]
    (when (< pos (count lines))
      (nth lines pos))))


(defn advance-cursor
  "Advance cursor by n positions (default: 1)."
  ([cursor] (advance-cursor cursor 1))
  ([cursor n]
   (->LineCursor (:lines cursor)
                 (:blank-lines cursor)
                 (+ (:position cursor) n))))


(defn next-cursor
  "Get current line and advance cursor. Returns [line new-cursor]."
  [cursor]
  [(peek-cursor cursor) (advance-cursor cursor)])


(defn at-end?
  "Check if cursor is at end of lines."
  [cursor]
  (>= (:position cursor) (count (:lines cursor))))


(defn peek-at-depth
  "Get current line if it matches target depth."
  [cursor target-depth]
  (let [line (peek-cursor cursor)]
    (when (and line (= (:depth line) target-depth))
      line)))


(defn has-more-at-depth?
  "Check if there are more lines at target depth."
  [cursor target-depth]
  (some? (peek-at-depth cursor target-depth)))


(defn get-blank-lines-in-range
  "Get blank lines within line number range [start, end]."
  [cursor start end]
  (filterv #(<= start (:line-number %) end) (:blank-lines cursor)))
