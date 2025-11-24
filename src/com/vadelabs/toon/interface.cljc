(ns com.vadelabs.toon.interface
  (:require
    [com.vadelabs.toon.decode.decoders :as decoders]
    [com.vadelabs.toon.decode.keys :as keys]
    [com.vadelabs.toon.decode.scanner :as scanner]
    [com.vadelabs.toon.decode.stream :as stream]
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


(defn decode-stream-sync
  "Decode TOON into lazy sequence of parse events (memory-efficient).

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
    (decode-stream-sync \"name: Alice\\nage: 30\")
    ;=> ({:type :start-object}
         {:type :key :key \"name\"}
         {:type :primitive :value \"Alice\"}
         {:type :key :key \"age\"}
         {:type :primitive :value 30}
         {:type :end-object})

    (decode-stream-sync \"[3]: a,b,c\")
    ;=> ({:type :start-array}
         {:type :primitive :value \"a\"}
         {:type :primitive :value \"b\"}
         {:type :primitive :value \"c\"}
         {:type :end-array})

  See also: decode-stream for async version"
  [input & [options]]
  (stream/decode-stream-sync input options))


(defn decode-stream
  "Decode TOON asynchronously into core.async channel of parse events.

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
    core.async channel of events (same format as decode-stream-sync)

  Example:
    (require '[clojure.core.async :as async])

    (let [events-ch (decode-stream \"name: Alice\\nage: 30\")]
      (async/<!! (async/into [] events-ch)))
    ;=> [{:type :start-object}
         {:type :key :key \"name\"}
         {:type :primitive :value \"Alice\"}
         {:type :key :key \"age\"}
         {:type :primitive :value 30}
         {:type :end-object}]

  See also: decode-stream-sync for synchronous lazy sequence version"
  [source & [options]]
  (stream/decode-stream source options))
