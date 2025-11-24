(ns com.vadelabs.toon.interface
  (:require
    [clojure.string :as str]
    [com.vadelabs.toon.decode.decoders :as decoders]
    [com.vadelabs.toon.decode.keys :as keys]
    [com.vadelabs.toon.decode.scanner :as scanner]
    [com.vadelabs.toon.decode.stream :as stream]
    [com.vadelabs.toon.decode.value-builder :as value-builder]
    [com.vadelabs.toon.encode.encoders :as encoders]
    [com.vadelabs.toon.encode.normalize :as norm]
    [com.vadelabs.toon.encode.writer :as writer]))


;; ============================================================================
;; Public API
;; ============================================================================

(defn encode
  "Encodes a Clojure data structure to TOON (Token-Oriented Object Notation) format.

  TOON is a compact, human-readable format designed for passing structured data
  to LLMs with 30-60% fewer tokens than JSON.

  Parameters:
    - input: Any Clojure value (will be normalized to JSON-compatible types)
    - options: Optional map with keys:
      - :indent - Number of spaces per indentation level (default: 2)
      - :delimiter - Delimiter for array values: \",\" (default), \"\\t\", or \"|\"

  Returns:
    String in TOON format with no trailing newline or spaces.

  Examples:
    (encode {:name \"Ada\" :tags [\"reading\" \"gaming\"]})
    ;=> \"name: Ada\\ntags[2]: reading,gaming\"

    (encode [{:id 1 :name \"Alice\"}
             {:id 2 :name \"Bob\"}])
    ;=> \"[2]{id,name}:\\n  1,Alice\\n  2,Bob\"

    (encode {:tags [\"a\" \"b\" \"c\"]} {:delimiter \"\\t\"})
    ;=> \"tags[3\\t]: a\\tb\\tc\""
  [input & [options]]
  (let [opts (merge {:indent 2
                     :delimiter ","
                     :key-collapsing :off
                     :flatten-depth ##Inf}
                    options)]
    (-> input
        norm/normalize-value
        (encoders/value opts 0 (writer/create (:indent opts)))
        writer/to-string)))


(defn encode-lines
  "Encode value into sequence of TOON lines.

  Following Stuart Sierra's naming: pure function, returns lines (noun).

  Yields TOON lines one at a time without building the full string.
  Useful for streaming large outputs to files, HTTP responses, or stdout.
  Each line is a string without trailing newline character.

  Parameters:
    - input: Any Clojure value (maps, vectors, primitives)
    - options: Optional map with keys (same as encode):
      - :indent - Number of spaces per indentation level (default: 2)
      - :delimiter - Delimiter for array values: \",\" (default), \"\\t\", or \"|\"
      - :key-collapsing - Key collapsing mode (default: :off)
      - :flatten-depth - Depth for array flattening (default: Infinity)

  Returns:
    Sequence of strings (one per line, no newline characters)

  Example:
    (doseq [line (encode-lines {:name \"Alice\" :age 30})]
      (println line))
    ; name: Alice
    ; age: 30

    ;; Equivalence with encode
    (= (str/join \"\\n\" (encode-lines data))
       (encode data))

  See also: encode"
  [input & [options]]
  (let [opts (merge {:indent 2
                     :delimiter ","
                     :key-collapsing :off
                     :flatten-depth ##Inf}
                    options)
        writer (-> input
                   norm/normalize-value
                   (encoders/value opts 0 (writer/create (:indent opts))))]
    (:lines writer)))


(defn decode
  "Decodes TOON (Token-Oriented Object Notation) format to Clojure data structures.

  TOON is a compact, human-readable format designed for passing structured data
  to LLMs with 30-60% fewer tokens than JSON.

  Parameters:
    - input: String in TOON format
    - options: Optional map with keys:
      - :indent - Number of spaces per indentation level (default: 2)
      - :strict - Enable strict validation (default: true)

  Returns:
    Clojure data structure (maps, vectors, primitives)

  Examples:
    (decode \"name: Ada\\ntags[2]: reading,gaming\")
    ;=> {\"name\" \"Ada\", \"tags\" [\"reading\" \"gaming\"]}

    (decode \"[2]{id,name}:\\n  1,Alice\\n  2,Bob\")
    ;=> [{\"id\" 1.0, \"name\" \"Alice\"} {\"id\" 2.0, \"name\" \"Bob\"}]

    (decode \"[3]: 1,2,3\")
    ;=> [1.0 2.0 3.0]

    (decode \"name: Ada\" {:strict false})
    ;=> {\"name\" \"Ada\"}"
  [input & [options]]
  (let [opts (merge {:indent 2
                     :strict true
                     :expand-paths :off}
                    options)]
    (-> input
        (scanner/to-parsed-lines (:indent opts) (:strict opts))
        scanner/cursor-from-scan-result
        (decoders/value-from-lines (:indent opts) (:strict opts))
        (keys/expand (:strict opts) (:expand-paths opts)))))


(defn events
  "Decode TOON into lazy sequence of parse events (memory-efficient).

  Following Stuart Sierra's naming: pure function returning events uses a noun.

  Instead of building complete value trees, emits parse events that can be
  consumed incrementally. Useful for processing large TOON documents without
  loading entire structure into memory.

  Parameters:
    - input: String in TOON format or sequence of lines
    - options: Optional map with keys:
      - :indent - Number of spaces per indentation level (default: 2)
      - :strict - Enable strict validation (default: true)

  Returns:
    Lazy sequence of events. Event types:
      - {:type :start-object}
      - {:type :end-object}
      - {:type :start-array}
      - {:type :end-array}
      - {:type :key :key \"field-name\"}
      - {:type :primitive :value <value>}

  Examples:
    (events \"name: Alice\\nage: 30\")
    ;=> ({:type :start-object}
         {:type :key :key \"name\"}
         {:type :primitive :value \"Alice\"}
         {:type :key :key \"age\"}
         {:type :primitive :value 30}
         {:type :end-object})

    (events \"[3]: a,b,c\")
    ;=> ({:type :start-array}
         {:type :primitive :value \"a\"}
         {:type :primitive :value \"b\"}
         {:type :primitive :value \"c\"}
         {:type :end-array})

  See also: events-ch for async version, events->value to reconstruct"
  [input & [options]]
  (stream/decode-stream-sync input options))


(defn events-ch
  "Decode TOON asynchronously into core.async channel of parse events.

  Following Stuart Sierra's naming: function returns a channel (suffix -ch).

  Processes TOON content from either a sequence or async channel, emitting
  parse events to an output channel. Useful for streaming processing of
  large documents.

  Parameters:
    - source: Either:
      - String in TOON format
      - Sequence of TOON lines
      - core.async channel of strings
    - options: Optional map with keys:
      - :indent - Number of spaces per indentation level (default: 2)
      - :strict - Enable strict validation (default: true)
      - :buf-size - Channel buffer size (default: 32)

  Returns:
    core.async channel of events (same format as events function)

  Example:
    (require '[clojure.core.async :as async])

    (let [ch (events-ch \"name: Alice\\nage: 30\")]
      (async/<!! (async/into [] ch)))
    ;=> [{:type :start-object}
         {:type :key :key \"name\"}
         {:type :primitive :value \"Alice\"}
         {:type :key :key \"age\"}
         {:type :primitive :value 30}
         {:type :end-object}]

  See also: events for synchronous lazy sequence, events->value to reconstruct"
  [source & [options]]
  (stream/decode-stream source options))


(defn events->value
  "Reconstruct value from event stream.

  Takes a sequence of parse events (from decode-stream-sync or decode-stream)
  and reconstructs the original data structure. Useful for custom event
  processing workflows where you want to filter/transform events before
  building the final value.

  Parameters:
    - events: Sequence or iterable of parse events

  Returns:
    Reconstructed value (map, vector, or primitive)

  Example:
    (let [input \"name: Alice\\nage: 30\"
          events (decode-stream-sync input)
          value (events->value events)]
      value)
    ;=> {\"name\" \"Alice\", \"age\" 30}

  Example with filtering:
    (let [input \"name: Alice\\nage: 30\\ncity: NYC\"
          events (decode-stream-sync input)
          ;; Filter out the 'city' field
          filtered (remove #(and (= :key (:type %))
                                 (= \"city\" (:key %)))
                           events)
          value (events->value filtered)]
      value)
    ;=> {\"name\" \"Alice\", \"age\" 30}

  Throws:
    - ex-info on malformed event streams (unmatched brackets, missing keys, etc.)

  See also: events, events-ch"
  [events]
  (value-builder/events->value events))


(defn lines->value
  "Decode TOON from pre-split lines with full decode options.

  Convenience function that bridges streaming and regular decode.
  Unlike events->value, this supports path expansion options.

  Parameters:
    - lines: Sequence of TOON lines (strings)
    - options: Optional map with keys:
      - :indent - Number of spaces per indentation level (default: 2)
      - :strict - Enable strict validation (default: true)
      - :expand-paths - Path expansion mode: :off (default) or :safe

  Returns:
    Decoded value (map, vector, or primitive)

  Example:
    (lines->value [\"name: Alice\" \"age: 30\"])
    ;=> {\"name\" \"Alice\", \"age\" 30}

    (lines->value [\"user.name: Alice\" \"user.age: 30\"] {:expand-paths :safe})
    ;=> {\"user\" {\"name\" \"Alice\", \"age\" 30}}

  See also: decode, events, events->value"
  [lines & [options]]
  (let [opts (merge {:indent 2
                     :strict true
                     :expand-paths :off}
                    options)
        input (str/join "\n" lines)]
    (-> input
        (events opts)
        events->value
        (keys/expand (:strict opts) (:expand-paths opts)))))
