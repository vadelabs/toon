(ns com.vadelabs.toon.encode.writer
  "Line writer for managing indented TOON output.

  The LineWriter handles:
  - Indentation management (configurable spaces per level)
  - Line accumulation
  - Whitespace invariants (no trailing spaces or newlines)"
  (:require
   [clojure.string :as str]))

;; ============================================================================
;; LineWriter deftype
;; ============================================================================

(defrecord LineWriter
           [lines indent-string indent-cache])

(defn create
  "Creates a new LineWriter with the specified indentation size.

  Parameters:
    - indent-size: Number of spaces per indentation level (default: 2)

  Returns:
    A new LineWriter instance."
  ([]
   (create 2))
  ([indent-size]
   (->LineWriter [] (apply str (repeat indent-size " ")) {})))

(defn push
  "Adds a line to the writer at the specified depth.

  Parameters:
    - writer: LineWriter instance
    - depth: Indentation level (0 = no indent, 1 = one indent, etc.)
    - content: String content for the line (must not contain newlines)

  Returns:
    Updated LineWriter with the new line added.

  Notes:
    - Trailing spaces are automatically removed from content
    - Empty content is allowed
    - Indentation strings are cached per depth level for performance"
  [^LineWriter writer depth content]
  (let [indent-str (:indent-string writer)
        cache (:indent-cache writer)
        ;; Use cached indentation or compute and cache it
        [indentation new-cache] (if-let [cached (get cache depth)]
                                  [cached cache]
                                  (let [computed (str/join (repeat depth indent-str))]
                                    [computed (assoc cache depth computed)]))
        ;; Remove trailing spaces from content
        trimmed-content (str/trimr content)
        line (str indentation trimmed-content)]
    (->LineWriter
     (conj (:lines writer) line)
     indent-str
     new-cache)))

(defn to-string
  "Converts the LineWriter to a string.

  Parameters:
    - writer: LineWriter instance

  Returns:
    String representation with lines joined by newlines.
    No trailing newline at the end."
  [^LineWriter writer]
  (let [lines (:lines writer)]
    (if (empty? lines)
      ""
      (str/join "\n" lines))))

(defn line-count
  [^LineWriter writer]
  (count (:lines writer)))

(defn empty-writer?
  [^LineWriter writer]
  (empty? (:lines writer)))
