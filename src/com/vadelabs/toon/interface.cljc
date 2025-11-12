(ns com.vadelabs.toon.interface
  (:require
    [com.vadelabs.toon.decode.decoders :as decoders]
    [com.vadelabs.toon.decode.keys :as keys]
    [com.vadelabs.toon.decode.scanner :as scanner]
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
