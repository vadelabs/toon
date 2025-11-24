(ns com.vadelabs.toon.decode.event-builder
  "Event-based streaming decoder for TOON format.

  Processes TOON lines and emits parse events instead of building
  complete value trees. Enables memory-efficient processing of large documents."
  (:require
    [clojure.string :as str]
    [com.vadelabs.toon.decode.events :as events]
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


;; ============================================================================
;; Array Event Emission
;; ============================================================================

(defn- emit-inline-array-events
  "Emit events for inline array: [3]: a,b,c"
  [header-info strict]
  (let [values (:inline-values header-info)
        delimiter (:delimiter header-info)
        raw-values (parser/delimited-values values delimiter)
        parsed-values (map #(parser/primitive-token % strict) raw-values)
        length (:length header-info)]
    (concat
      [(events/start-array length)]
      (map events/primitive parsed-values)
      [(events/end-array)])))


(defn- emit-tabular-array-events
  "Emit events for tabular array: [2]{id,name}: 1,Alice / 2,Bob"
  [header-info cursor depth strict]
  (let [fields (:fields header-info)
        delimiter (:delimiter header-info)
        length (:length header-info)]
    (loop [current-cursor cursor
           events-acc [(events/start-array length)]
           items-seen 0]
      (let [line (scanner/peek-at-depth current-cursor depth)]
        (if-not line
          ;; No more lines at this depth
          (conj events-acc (events/end-array))
          (let [content (:content line)
                raw-values (parser/delimited-values content delimiter)
                parsed-values (map #(parser/primitive-token % strict) raw-values)
                row-events (concat
                             [(events/start-object)]
                             (mapcat (fn [field value]
                                       [(events/key-event field)
                                        (events/primitive value)])
                                     fields
                                     parsed-values)
                             [(events/end-object)])
                new-cursor (scanner/advance-cursor current-cursor)]
            (recur new-cursor
                   (into events-acc row-events)
                   (inc items-seen))))))))


(defn- emit-list-array-events
  "Emit events for list array: [2]: - item1 / - item2"
  [header-info cursor depth strict]
  (let [length (:length header-info)]
    (loop [current-cursor cursor
           events-acc [(events/start-array length)]
           items-seen 0]
      (let [line (scanner/peek-at-depth current-cursor depth)]
        (if-not line
          ;; No more lines at this depth
          (conj events-acc (events/end-array))
          (let [content (str/trim (:content line))
                ;; Check if line starts with list marker (- or •)
                is-list-item? (or (str/starts-with? content "-")
                                  (str/starts-with? content "•"))]
            (if is-list-item?
              ;; List item - emit events for the value
              (let [item-content (str/trim (subs content 1))
                    next-cursor (scanner/advance-cursor current-cursor)
                    has-children? (scanner/has-more-at-depth? next-cursor (inc depth))
                    has-inline-field? (and (not (str/blank? item-content))
                                          (str/includes? item-content ":"))
                    [item-events final-cursor] (cond
                                             ;; Object with inline first field + nested children
                                             (and has-inline-field? has-children?)
                                             (let [[key-part value-part] (str/split item-content #":" 2)
                                                   {field-key :key was-quoted :was-quoted} (parser/key-token key-part)
                                                   field-value (when value-part
                                                                (parser/primitive-token (str/trim value-part) strict))
                                                   ;; Process child fields at next depth
                                                   [child-field-events end-cursor]
                                                   (loop [c next-cursor
                                                          acc []]
                                                     (let [child-line (scanner/peek-at-depth c (inc depth))]
                                                       (if-not child-line
                                                         [acc c]
                                                         (let [field-evts (emit-object-field-events
                                                                           child-line
                                                                           (scanner/advance-cursor c)
                                                                           (inc depth)
                                                                           strict)]
                                                           (recur (scanner/advance-cursor c)
                                                                  (into acc field-evts))))))]
                                               [(concat [(events/start-object)
                                                         (events/key-event field-key was-quoted)
                                                         (events/primitive field-value)]
                                                        child-field-events
                                                        [(events/end-object)])
                                                end-cursor])

                                             ;; Nested structure (no inline field)
                                             has-children?
                                             [(emit-value-events
                                               item-content
                                               next-cursor
                                               (inc depth)
                                               strict)
                                              next-cursor]

                                             ;; Primitive value
                                             :else
                                             [[(events/primitive (parser/primitive-token item-content strict))]
                                              next-cursor])]
                (recur final-cursor
                       (into events-acc item-events)
                       (inc items-seen)))
              ;; Not a list item, end of array
              (conj events-acc (events/end-array)))))))))


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
      ;; Empty array: key[0]
      (and (str/includes? key-str "[")
           value-str
           (re-matches empty-array-pattern (str/trim value-str)))
      (let [{:keys [key was-quoted]} (parser/key-token key-str)]
        [(events/key-event key was-quoted)
         (events/start-array 0)
         (events/end-array)])

      ;; Array header: key[3]: values or nested
      (and (str/includes? key-str "[")
           value-str)
      (let [header-info (parser/array-header-line content)
            field-key (:key header-info)]
        (concat
          [(events/key-event field-key)]
          (emit-array-events header-info cursor (inc depth) strict)))

      ;; Check for nested content
      (scanner/has-more-at-depth? cursor (inc depth))
      (let [{field-key :key was-quoted :was-quoted} (parser/key-token key-str)
            nested-events (emit-value-events
                            value-str
                            cursor
                            (inc depth)
                            strict)]
        (concat
          [(events/key-event field-key was-quoted)]
          nested-events))

      ;; Inline primitive value
      :else
      (let [{field-key :key was-quoted :was-quoted} (parser/key-token key-str)
            field-value (if value-str
                          (parser/primitive-token value-str strict)
                          nil)]
        [(events/key-event field-key was-quoted)
         (events/primitive field-value)]))))


(defn- emit-object-events
  "Emit events for object (map) structure."
  [cursor depth strict]
  (loop [current-cursor cursor
         events-acc [(events/start-object)]]
    (let [line (scanner/peek-at-depth current-cursor depth)]
      (if-not line
        ;; No more lines at this depth
        (conj events-acc (events/end-object))
        (let [field-events (emit-object-field-events
                             line
                             (scanner/advance-cursor current-cursor)
                             depth
                             strict)
              new-cursor (scanner/advance-cursor current-cursor)]
          (recur new-cursor
                 (into events-acc field-events)))))))


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
    Sequence of events"
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
    [(events/primitive (parser/primitive-token content strict))]))


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
      [(events/start-object) (events/end-object)]
      (let [content (:content first-line)]
        (cond
          ;; Empty array at root: [0]
          (and (str/starts-with? (str/trim content) "[")
               (re-matches empty-array-pattern (str/trim content)))
          [(events/start-array 0) (events/end-array)]

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
          [(events/primitive (parser/primitive-token content strict))]

          ;; Root object
          :else
          (emit-object-events cursor 0 strict))))))
