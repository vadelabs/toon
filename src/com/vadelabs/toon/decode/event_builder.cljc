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

(declare emit-value-events)
(declare emit-object-events)
(declare emit-object-field-events)
(declare emit-array-events)


;; ============================================================================
;; Constants
;; ============================================================================

(def ^:private empty-array-pattern
  "Regex pattern for matching empty array notation [0]."
  #"^\[0[,\t|]?\]$")

(def ^:private list-item-prefix "- ")
(def ^:private bullet-marker "•")


;; ============================================================================
;; Validation
;; ============================================================================

(defn- assert-expected-count!
  "Validate actual count matches expected. Throws in strict mode."
  [actual expected context strict]
  (when (and strict (not= actual expected))
    (throw (ex-info (str "Expected " expected " " context ", but found " actual)
                    {:type :count-mismatch
                     :expected expected
                     :actual actual
                     :context context}))))


;; ============================================================================
;; Array Event Emission
;; ============================================================================

(defn- emit-inline-array-events
  "Emit events for inline array: [3]: a,b,c
  Returns lazy sequence of events."
  [{:keys [inline-values delimiter length]} strict]
  (let [raw-values (parser/delimited-values inline-values delimiter)
        parsed-values (map #(parser/primitive-token % strict) raw-values)]
    (assert-expected-count! (count parsed-values) length "inline array items" strict)
    (lazy-cat
      [{:type :start-array :length length}]
      (map (fn [v] {:type :primitive :value v}) parsed-values)
      [{:type :end-array}])))


(defn- emit-row-object-events
  "Emit events for a single tabular row as object."
  [fields values]
  (lazy-cat
    [{:type :start-object}]
    (mapcat (fn [field value]
              [{:type :key :key field}
               {:type :primitive :value value}])
            fields values)
    [{:type :end-object}]))


(defn- collect-tabular-rows
  "Collect tabular array rows. Returns [events-vec rows-found]."
  [fields delimiter cursor depth strict expected-count]
  (loop [c cursor
         acc []
         rows-found 0]
    (if (>= rows-found expected-count)
      [acc rows-found]
      (if-let [line (scanner/peek-at-depth c depth)]
        (let [content (:content line)
              raw-values (parser/delimited-values content delimiter)
              parsed-values (map #(parser/primitive-token % strict) raw-values)
              row-events (emit-row-object-events fields parsed-values)]
          (recur (scanner/advance-cursor c)
                 (into acc row-events)
                 (inc rows-found)))
        ;; No more lines at depth - stop
        [acc rows-found]))))


(defn- emit-tabular-array-events
  "Emit events for tabular array: [2]{id,name}: 1,Alice / 2,Bob"
  [{:keys [fields delimiter length]} cursor depth strict]
  (let [[row-events rows-found] (collect-tabular-rows fields delimiter cursor depth strict length)]
    (assert-expected-count! rows-found length "tabular rows" strict)
    (lazy-cat
      [{:type :start-array :length length}]
      row-events
      [{:type :end-array}])))


(defn- list-item?
  "Returns true if content starts with list marker."
  [content]
  (or (str/starts-with? content list-item-prefix)
      (str/starts-with? content bullet-marker)
      (= content "-")))


(defn- emit-list-item-events
  "Emit events for a single list item."
  [content cursor depth strict]
  (let [item-content (str/trim (subs content 1))
        next-cursor cursor
        has-children? (scanner/has-more-at-depth? next-cursor (inc depth))
        has-inline-field? (and (not (str/blank? item-content))
                               (str-utils/unquoted-char item-content \:))]
    (cond
      ;; Empty list item
      (str/blank? item-content)
      (if has-children?
        ;; Has nested fields - emit as object
        (emit-object-events next-cursor (inc depth) strict)
        ;; No children - emit empty object (matches TS behavior)
        [{:type :start-object} {:type :end-object}])

      ;; Object with inline first field + nested children
      (and has-inline-field? has-children?)
      (let [[key-part value-part] (str/split item-content #":" 2)
            {:keys [key was-quoted]} (parser/key-token key-part)
            field-value (when value-part
                          (parser/primitive-token (str/trim value-part) strict))
            child-events (loop [c next-cursor
                                acc []]
                           (if-let [child-line (scanner/peek-at-depth c (inc depth))]
                             (let [field-evts (emit-object-field-events
                                                child-line
                                                (scanner/advance-cursor c)
                                                (inc depth)
                                                strict)]
                               (recur (scanner/advance-cursor c)
                                      (into acc field-evts)))
                             acc))]
        (lazy-cat
          [{:type :start-object}
           (if was-quoted
             {:type :key :key key :was-quoted true}
             {:type :key :key key})
           {:type :primitive :value field-value}]
          child-events
          [{:type :end-object}]))

      ;; Object first field (inline key-value, check for nested)
      has-inline-field?
      (lazy-cat
        [{:type :start-object}]
        (let [[key-part value-part] (str/split item-content #":" 2)
              {:keys [key was-quoted]} (parser/key-token key-part)
              nested-depth (inc depth)]
          (lazy-cat
            [(if was-quoted
               {:type :key :key key :was-quoted true}
               {:type :key :key key})
             {:type :primitive :value (parser/primitive-token (str/trim (or value-part "")) strict)}]
            ;; Check for sibling fields at nested depth
            (loop [c next-cursor
                   acc []]
              (if-let [sibling-line (scanner/peek-at-depth c nested-depth)]
                (if (list-item? (:content sibling-line))
                  acc  ; Stop at next list item
                  (let [field-evts (emit-object-field-events
                                     sibling-line
                                     (scanner/advance-cursor c)
                                     nested-depth
                                     strict)]
                    (recur (scanner/advance-cursor c)
                           (into acc field-evts))))
                acc))))
        [{:type :end-object}])

      ;; Nested structure without inline field
      has-children?
      (emit-value-events item-content next-cursor (inc depth) strict)

      ;; Primitive value
      :else
      [{:type :primitive :value (parser/primitive-token item-content strict)}])))


(defn- collect-list-items
  "Collect list array items. Returns [events-vec items-found cursor-after]."
  [cursor depth strict expected-count]
  (loop [c cursor
         acc []
         items-found 0]
    (if (>= items-found expected-count)
      [acc items-found c]
      (if-let [line (scanner/peek-at-depth c depth)]
        (let [content (str/trim (:content line))]
          (if (list-item? content)
            (let [next-cursor (scanner/advance-cursor c)
                  item-events (emit-list-item-events content next-cursor depth strict)
                  ;; Find cursor position after processing item children
                  final-cursor (loop [fc next-cursor]
                                 (if-let [child-line (scanner/peek-at-depth fc (inc depth))]
                                   (if (list-item? (:content child-line))
                                     fc
                                     (recur (scanner/advance-cursor fc)))
                                   fc))]
              (recur final-cursor
                     (into acc item-events)
                     (inc items-found)))
            ;; Not a list item - stop
            [acc items-found c]))
        ;; No more lines at depth - stop
        [acc items-found c]))))


(defn- emit-list-array-events
  "Emit events for list array: [2]: - item1 / - item2"
  [{:keys [length]} cursor depth strict]
  (let [[item-events items-found _] (collect-list-items cursor depth strict length)]
    (assert-expected-count! items-found length "list array items" strict)
    (lazy-cat
      [{:type :start-array :length length}]
      item-events
      [{:type :end-array}])))


(defn- emit-array-events
  "Emit events for array based on header format."
  [header-info cursor depth strict]
  (cond
    ;; Inline array: [3]: a,b,c
    (:inline-values header-info)
    (emit-inline-array-events header-info strict)

    ;; Tabular array: [2]{id,name}: 1,Alice / 2,Bob
    (:fields header-info)
    (emit-tabular-array-events header-info cursor depth strict)

    ;; List array: [2]: - item1 / - item2
    :else
    (emit-list-array-events header-info cursor depth strict)))


;; ============================================================================
;; Object Event Emission
;; ============================================================================

(defn- emit-object-field-events
  "Emit events for a single object field (key-value pair)."
  [line cursor depth strict]
  (let [content (:content line)
        [key-part value-part] (str/split content #":" 2)
        key-str (str/trim key-part)
        value-str (when value-part (str/trim value-part))]
    (cond
      ;; Empty array: key[0]: [0]
      (and (str/includes? key-str "[")
           value-str
           (re-matches empty-array-pattern (str/trim value-str)))
      (let [{:keys [key was-quoted]} (parser/key-token key-str)]
        [(if was-quoted
           {:type :key :key key :was-quoted true}
           {:type :key :key key})
         {:type :start-array :length 0}
         {:type :end-array}])

      ;; Array header: key[3]: values or nested
      (and (str/includes? key-str "[")
           value-str)
      (let [header-info (parser/array-header-line content)
            field-key (:key header-info)]
        (lazy-cat
          [{:type :key :key field-key}]
          (emit-array-events header-info cursor (inc depth) strict)))

      ;; Check for nested content
      (scanner/has-more-at-depth? cursor (inc depth))
      (let [{:keys [key was-quoted]} (parser/key-token key-str)
            nested-events (emit-value-events value-str cursor (inc depth) strict)]
        (lazy-cat
          [(if was-quoted
             {:type :key :key key :was-quoted true}
             {:type :key :key key})]
          nested-events))

      ;; Inline primitive value
      :else
      (let [{:keys [key was-quoted]} (parser/key-token key-str)
            field-value (if value-str
                          (parser/primitive-token value-str strict)
                          nil)]
        [(if was-quoted
           {:type :key :key key :was-quoted true}
           {:type :key :key key})
         {:type :primitive :value field-value}]))))


(defn- emit-object-fields
  "Emit events for object fields at given depth. Returns lazy sequence."
  [cursor depth strict]
  (lazy-seq
    (when-let [line (scanner/peek-at-depth cursor depth)]
      (let [field-events (emit-object-field-events
                           line
                           (scanner/advance-cursor cursor)
                           depth
                           strict)]
        (lazy-cat
          field-events
          (emit-object-fields (scanner/advance-cursor cursor) depth strict))))))


(defn- emit-object-events
  "Emit events for object (map) structure."
  [cursor depth strict]
  (lazy-cat
    [{:type :start-object}]
    (emit-object-fields cursor depth strict)
    [{:type :end-object}]))


;; ============================================================================
;; Value Event Emission (Root Dispatcher)
;; ============================================================================

(defn- emit-value-events
  "Emit events for a value (dispatches to appropriate handler).

  Parameters:
    - content: Line content string (may be nil for nested structures)
    - cursor: Current line cursor
    - depth: Current nesting depth
    - strict: Enable strict validation

  Returns:
    Lazy sequence of events"
  [content cursor depth strict]
  (cond
    ;; If no content but has children, it's an object
    (and (or (nil? content) (str/blank? content))
         (scanner/has-more-at-depth? cursor depth))
    (emit-object-events cursor depth strict)

    ;; Has nested content - it's an object
    (scanner/has-more-at-depth? cursor depth)
    (emit-object-events cursor depth strict)

    ;; Single primitive value
    :else
    [{:type :primitive :value (parser/primitive-token content strict)}]))


;; ============================================================================
;; Public API
;; ============================================================================

(defn events-from-cursor
  "Generate event sequence from LineCursor.

  Parameters:
    - cursor: LineCursor positioned at start
    - indent: Indent size (for compatibility, not used in events)
    - strict: Enable strict validation

  Returns:
    Lazy sequence of parse events"
  [cursor _indent strict]
  (let [first-line (scanner/peek-cursor cursor)]
    (if-not first-line
      ;; Empty input: empty object
      [{:type :start-object} {:type :end-object}]
      (let [content (:content first-line)]
        (cond
          ;; Empty array at root: [0]
          (and (str/starts-with? (str/trim content) "[")
               (re-matches empty-array-pattern (str/trim content)))
          [{:type :start-array :length 0} {:type :end-array}]

          ;; Array header at root (no key before bracket)
          (and (str/includes? content "[")
               (str/includes? content "]")
               (str/includes? content ":")
               (str/starts-with? (str/trim content) "["))
          (let [header-info (parser/array-header-line content)
                cursor-after-header (scanner/advance-cursor cursor)
                depth 1]
            (emit-array-events header-info cursor-after-header depth strict))

          ;; Single-line primitive (no colon)
          (and (nil? (scanner/peek-at-depth cursor 1))
               (not (str-utils/unquoted-char content \:)))
          [{:type :primitive :value (parser/primitive-token content strict)}]

          ;; Root object
          :else
          (emit-object-events cursor 0 strict))))))
