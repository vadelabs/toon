(ns com.vadelabs.toon.decode.event-builder
  "Event-based streaming decoder for TOON format.

  Processes TOON lines and emits parse events instead of building
  complete value trees. Enables memory-efficient processing of large documents."
  (:require
   [clojure.string :as str]
   [com.vadelabs.toon.decode.parser :as parser]
   [com.vadelabs.toon.decode.scanner :as scanner]
   [com.vadelabs.toon.utils :as str-utils]))

;; ============================================================================
;; Forward Declarations
;; ============================================================================

(declare value-events)
(declare object-events)
(declare object-field-events)
(declare array-events)

;; ============================================================================
;; Constants
;; ============================================================================

(def ^:private empty-array-pattern #"^\[0[,\t|]?\]$")
(def ^:private colon-pattern #":")
(def ^:private list-item-prefix "- ")
(def ^:private bullet-marker "•")

;; ============================================================================
;; Helpers
;; ============================================================================

(defn- key-evt
  "Create key event map. Includes :was-quoted only when true."
  [k was-quoted]
  (cond-> {:type :key :key k}
    was-quoted (assoc :was-quoted true)))

(defn- assert-expected-count!
  "Validate actual count matches expected. Throws in strict mode."
  [actual expected context strict]
  (when (and strict (not= actual expected))
    (throw (ex-info (str "Expected " expected " " context ", but found " actual)
                    {:type :count-mismatch
                     :expected expected
                     :actual actual
                     :context context}))))

(defn- list-item?
  "True if content starts with list marker."
  [content]
  (or (str/starts-with? content list-item-prefix)
      (str/starts-with? content bullet-marker)
      (= content "-")))

;; ============================================================================
;; Array Events
;; ============================================================================

(defn- inline-array-events
  "Events for inline array: [3]: a,b,c"
  [{:keys [inline-values delimiter length]} strict]
  (let [raw-values (parser/delimited-values inline-values delimiter)
        parsed-values (mapv #(parser/primitive-token % strict) raw-values)]
    (assert-expected-count! (count parsed-values) length "inline array items" strict)
    (lazy-cat
     [{:type :start-array :length length}]
     (map (fn [v] {:type :primitive :value v}) parsed-values)
     [{:type :end-array}])))

(defn- row-object-events
  "Events for a single tabular row as object."
  [fields values]
  (lazy-cat
   [{:type :start-object}]
   (mapcat (fn [field value]
             [{:type :key :key field}
              {:type :primitive :value value}])
           fields values)
   [{:type :end-object}]))

(defn- collect-tabular-rows
  "Collect tabular array rows.
  Returns {:events [...] :count n}"
  [fields delimiter cursor depth strict expected-count]
  (loop [c cursor
         acc []
         n 0]
    (if (>= n expected-count)
      {:events acc :count n}
      (if-let [line (scanner/peek-at-depth c depth)]
        (let [content (:content line)
              raw-values (parser/delimited-values content delimiter)
              parsed-values (mapv #(parser/primitive-token % strict) raw-values)
              row-evts (row-object-events fields parsed-values)]
          (recur (scanner/advance-cursor c)
                 (into acc row-evts)
                 (inc n)))
        {:events acc :count n}))))

(defn- tabular-array-events
  "Events for tabular array: [2]{id,name}: 1,Alice / 2,Bob"
  [{:keys [fields delimiter length]} cursor depth strict]
  (let [{:keys [events count]} (collect-tabular-rows fields delimiter cursor depth strict length)]
    (assert-expected-count! count length "tabular rows" strict)
    (lazy-cat
     [{:type :start-array :length length}]
     events
     [{:type :end-array}])))

;; ============================================================================
;; List Item Events (broken into smaller functions)
;; ============================================================================

(defn- empty-item-events
  "Events for empty list item (- with nothing after)."
  [cursor depth strict has-children?]
  (if has-children?
    (object-events cursor (inc depth) strict)
    [{:type :start-object} {:type :end-object}]))

(defn- collect-child-field-events
  "Collect events for child fields at given depth."
  [cursor depth strict]
  (loop [c cursor
         acc []]
    (if-let [child-line (scanner/peek-at-depth c depth)]
      (let [field-evts (object-field-events child-line (scanner/advance-cursor c) depth strict)]
        (recur (scanner/advance-cursor c) (into acc field-evts)))
      acc)))

(defn- inline-object-with-children-events
  "Events for list item with inline field + nested children."
  [item-content cursor depth strict]
  (let [[key-part value-part] (str/split item-content colon-pattern 2)
        {:keys [key was-quoted]} (parser/key-token key-part)
        field-value (when value-part (parser/primitive-token (str/trim value-part) strict))
        child-evts (collect-child-field-events cursor (inc depth) strict)]
    (lazy-cat
     [{:type :start-object}
      (key-evt key was-quoted)
      {:type :primitive :value field-value}]
     child-evts
     [{:type :end-object}])))

(defn- collect-sibling-field-events
  "Collect sibling field events, stopping at next list item."
  [cursor depth strict]
  (loop [c cursor
         acc []]
    (if-let [sibling-line (scanner/peek-at-depth c depth)]
      (if (list-item? (:content sibling-line))
        acc
        (let [field-evts (object-field-events sibling-line (scanner/advance-cursor c) depth strict)]
          (recur (scanner/advance-cursor c) (into acc field-evts))))
      acc)))

(defn- inline-object-events
  "Events for list item with inline key-value only."
  [item-content cursor depth strict]
  (let [[key-part value-part] (str/split item-content colon-pattern 2)
        {:keys [key was-quoted]} (parser/key-token key-part)
        nested-depth (inc depth)
        sibling-evts (collect-sibling-field-events cursor nested-depth strict)]
    (lazy-cat
     [{:type :start-object}
      (key-evt key was-quoted)
      {:type :primitive :value (parser/primitive-token (str/trim (or value-part "")) strict)}]
     sibling-evts
     [{:type :end-object}])))

(defn- list-item-events
  "Events for a single list item."
  [content cursor depth strict]
  (let [item-content (str/trim (subs content 1))
        has-children? (scanner/has-more-at-depth? cursor (inc depth))
        has-inline-field? (and (not (str/blank? item-content))
                               (str-utils/unquoted-char item-content \:))]
    (cond
      (str/blank? item-content)
      (empty-item-events cursor depth strict has-children?)

      (and has-inline-field? has-children?)
      (inline-object-with-children-events item-content cursor depth strict)

      has-inline-field?
      (inline-object-events item-content cursor depth strict)

      has-children?
      (value-events item-content cursor (inc depth) strict)

      :else
      [{:type :primitive :value (parser/primitive-token item-content strict)}])))

(defn- collect-list-items
  "Collect list array items.
  Returns {:events [...] :count n :cursor <cursor-after>}"
  [cursor depth strict expected-count]
  (loop [c cursor
         acc []
         n 0]
    (if (>= n expected-count)
      {:events acc :count n :cursor c}
      (if-let [line (scanner/peek-at-depth c depth)]
        (let [content (str/trim (:content line))]
          (if (list-item? content)
            (let [next-cursor (scanner/advance-cursor c)
                  item-evts (list-item-events content next-cursor depth strict)
                  ;; Advance past children
                  final-cursor (loop [fc next-cursor]
                                 (if-let [child (scanner/peek-at-depth fc (inc depth))]
                                   (if (list-item? (:content child))
                                     fc
                                     (recur (scanner/advance-cursor fc)))
                                   fc))]
              (recur final-cursor (into acc item-evts) (inc n)))
            {:events acc :count n :cursor c}))
        {:events acc :count n :cursor c}))))

(defn- list-array-events
  "Events for list array: [2]: - item1 / - item2"
  [{:keys [length]} cursor depth strict]
  (let [{:keys [events count]} (collect-list-items cursor depth strict length)]
    (assert-expected-count! count length "list array items" strict)
    (lazy-cat
     [{:type :start-array :length length}]
     events
     [{:type :end-array}])))

(defn- array-events
  "Events for array based on header format."
  [header-info cursor depth strict]
  (cond
    (:inline-values header-info) (inline-array-events header-info strict)
    (:fields header-info)        (tabular-array-events header-info cursor depth strict)
    :else                        (list-array-events header-info cursor depth strict)))

;; ============================================================================
;; Object Events
;; ============================================================================

(defn- object-field-events
  "Events for a single object field (key-value pair)."
  [line cursor depth strict]
  (let [content (:content line)
        [key-part value-part] (str/split content colon-pattern 2)
        key-str (str/trim key-part)
        value-str (when value-part (str/trim value-part))]
    (cond
      ;; Empty array: key[0]: [0]
      (and (str/includes? key-str "[")
           value-str
           (re-matches empty-array-pattern (str/trim value-str)))
      (let [{:keys [key was-quoted]} (parser/key-token key-str)]
        [(key-evt key was-quoted)
         {:type :start-array :length 0}
         {:type :end-array}])

      ;; Array header: key[3]: values or nested
      (and (str/includes? key-str "[") value-str)
      (let [header-info (parser/array-header-line content)]
        (lazy-cat
         [{:type :key :key (:key header-info)}]
         (array-events header-info cursor (inc depth) strict)))

      ;; Nested content
      (scanner/has-more-at-depth? cursor (inc depth))
      (let [{:keys [key was-quoted]} (parser/key-token key-str)]
        (lazy-cat
         [(key-evt key was-quoted)]
         (value-events value-str cursor (inc depth) strict)))

      ;; Inline primitive value
      :else
      (let [{:keys [key was-quoted]} (parser/key-token key-str)
            field-value (when value-str (parser/primitive-token value-str strict))]
        [(key-evt key was-quoted)
         {:type :primitive :value field-value}]))))

(defn- object-fields-events
  "Events for all object fields at depth. Returns lazy sequence."
  [cursor depth strict]
  (lazy-seq
   (when-let [line (scanner/peek-at-depth cursor depth)]
     (lazy-cat
      (object-field-events line (scanner/advance-cursor cursor) depth strict)
      (object-fields-events (scanner/advance-cursor cursor) depth strict)))))

(defn- object-events
  "Events for object (map) structure."
  [cursor depth strict]
  (lazy-cat
   [{:type :start-object}]
   (object-fields-events cursor depth strict)
   [{:type :end-object}]))

;; ============================================================================
;; Value Events (Root Dispatcher)
;; ============================================================================

(defn- value-events
  "Events for a value (dispatches to appropriate handler)."
  [content cursor depth strict]
  (if (and (or (nil? content) (str/blank? content))
           (scanner/has-more-at-depth? cursor depth))
    (object-events cursor depth strict)
    (if (scanner/has-more-at-depth? cursor depth)
      (object-events cursor depth strict)
      [{:type :primitive :value (parser/primitive-token content strict)}])))

;; ============================================================================
;; Public API
;; ============================================================================

(defn cursor->events
  "Generate event sequence from LineCursor.

  Parameters:
    - cursor: LineCursor positioned at start
    - indent: Indent size (unused, kept for API compatibility)
    - strict: Enable strict validation

  Returns:
    Lazy sequence of parse events"
  [cursor _indent strict]
  (let [first-line (scanner/peek-cursor cursor)]
    (if-not first-line
      [{:type :start-object} {:type :end-object}]
      (let [content (:content first-line)]
        (cond
          ;; Empty array at root: [0]
          (and (str/starts-with? (str/trim content) "[")
               (re-matches empty-array-pattern (str/trim content)))
          [{:type :start-array :length 0} {:type :end-array}]

          ;; Array header at root
          (and (str/includes? content "[")
               (str/includes? content "]")
               (str/includes? content ":")
               (str/starts-with? (str/trim content) "["))
          (let [header-info (parser/array-header-line content)]
            (array-events header-info (scanner/advance-cursor cursor) 1 strict))

          ;; Single-line primitive (no colon)
          (and (nil? (scanner/peek-at-depth cursor 1))
               (not (str-utils/unquoted-char content \:)))
          [{:type :primitive :value (parser/primitive-token content strict)}]

          ;; Root object
          :else
          (object-events cursor 0 strict))))))
