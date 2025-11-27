(ns com.vadelabs.toon.decode.stream
  "Streaming decoder for TOON format.

  Provides memory-efficient event-based decoding for large TOON documents.
  Instead of building complete value trees, emits parse events that can be
  consumed incrementally."
  (:require
    #?(:clj [clojure.core.async :as async :refer [go]]
       :cljs [cljs.core.async :as async])
    [clojure.string :as str]
    [com.vadelabs.toon.decode.event-builder :as event-builder]
    [com.vadelabs.toon.decode.scanner :as scanner])
  #?(:cljs
     (:require-macros
       [cljs.core.async.macros :refer [go]])))


;; ============================================================================
;; Internal Helpers
;; ============================================================================

(def ^:private default-options
  {:indent 2
   :strict true
   :buf-size 32})


(defn- normalize-source
  "Normalize input to a sequence of lines.
  Returns nil for non-string, non-sequential sources (e.g., channels)."
  [source]
  (cond
    (string? source) (str/split-lines source)
    (sequential? source) source
    :else nil))


;; ============================================================================
;; Synchronous Streaming (Lazy Sequence)
;; ============================================================================

(defn decode-stream-sync
  "Decode TOON lines into lazy sequence of parse events.

  Processes TOON content line-by-line, emitting parse events instead of
  building complete value trees. Memory-efficient for large documents.

  Parameters:
    - lines: String or sequence of TOON lines (strings)
    - options: Optional map with keys:
      - :indent - Number of spaces per indentation level (default: 2)
      - :strict - Enable strict validation (default: true)

  Returns:
    Lazy sequence of events. Event types:
      {:type :start-object}
      {:type :end-object}
      {:type :start-array :length n}
      {:type :end-array}
      {:type :key :key \"field-name\"}           ; :was-quoted true when quoted
      {:type :primitive :value <value>}

  Example:
    (decode-stream-sync \"name: Alice\\nage: 30\")
    => ({:type :start-object}
        {:type :key :key \"name\"}
        {:type :primitive :value \"Alice\"}
        {:type :key :key \"age\"}
        {:type :primitive :value 30}
        {:type :end-object})"
  ([lines]
   (decode-stream-sync lines {}))
  ([lines options]
   (let [opts (merge default-options options)
         input (if (string? lines) lines (str/join "\n" lines))
         cursor (-> input
                    (scanner/to-parsed-lines (:indent opts) (:strict opts))
                    scanner/cursor-from-scan-result)]
     (event-builder/events-from-cursor cursor (:indent opts) (:strict opts)))))


;; ============================================================================
;; Asynchronous Streaming (core.async)
;; ============================================================================

(defn- emit-events-to-channel!
  "Emit all events from lines to output channel.
  Returns a go block that closes the channel when done."
  [lines opts out-ch]
  (go
    (try
      (doseq [event (decode-stream-sync lines opts)]
        (async/>! out-ch event))
      (catch #?(:clj Exception :cljs js/Error) e
        #?(:clj (println "Error in decode-stream:" e)
           :cljs (js/console.error "Error in decode-stream:" e))
        (throw e))
      (finally
        (async/close! out-ch)))))


(defn decode-stream
  "Decode TOON lines asynchronously into core.async channel of events.

  Processes TOON content from either a string, lazy sequence, or async channel,
  emitting parse events to an output channel.

  Parameters:
    - source: Either:
      - String in TOON format
      - Sequence of strings (TOON lines)
      - core.async channel of strings
    - options: Optional map with keys:
      - :indent - Number of spaces per indentation level (default: 2)
      - :strict - Enable strict validation (default: true)
      - :buf-size - Channel buffer size (default: 32)

  Returns:
    core.async channel of events (same format as decode-stream-sync)

  Example:
    (let [lines [\"name: Alice\" \"age: 30\"]
          events-ch (decode-stream lines)]
      (async/<!! (async/into [] events-ch)))
    => [{:type :start-object}
        {:type :key :key \"name\"}
        {:type :primitive :value \"Alice\"}
        {:type :key :key \"age\"}
        {:type :primitive :value 30}
        {:type :end-object}]

  Example with async source:
    (let [source-ch (async/to-chan! [\"name: Alice\" \"age: 30\"])
          events-ch (decode-stream source-ch)]
      (async/<!! (async/into [] events-ch)))"
  ([source]
   (decode-stream source {}))
  ([source options]
   (let [opts (merge default-options options)
         out-ch (async/chan (:buf-size opts))]
     (if-let [lines (normalize-source source)]
       ;; Sync source - emit directly
       (emit-events-to-channel! lines opts out-ch)
       ;; Async source (channel) - collect first then emit
       (go
         (try
           (let [lines (async/<! (async/into [] source))]
             (doseq [event (decode-stream-sync lines opts)]
               (async/>! out-ch event)))
           (catch #?(:clj Exception :cljs js/Error) e
             #?(:clj (println "Error in decode-stream:" e)
                :cljs (js/console.error "Error in decode-stream:" e))
             (throw e))
           (finally
             (async/close! out-ch)))))
     out-ch)))
