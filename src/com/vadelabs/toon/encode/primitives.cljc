(ns com.vadelabs.toon.encode.primitives
  "Primitive value encoding for TOON format.

  Handles encoding of nil, booleans, numbers, and strings."
  (:require
    [com.vadelabs.toon.constants :as const]
    [com.vadelabs.toon.utils :as quote]))


;; ============================================================================
;; Primitive Encoding
;; ============================================================================

(defn encode
  "Encodes a primitive value to its TOON string representation.

  Encoding rules:
  - nil → \"null\"
  - true → \"true\"
  - false → \"false\"
  - numbers → string representation
  - strings → quoted if necessary based on delimiter

  Parameters:
    - value: Primitive value (nil, boolean, number, or string)
    - delimiter: Delimiter character being used (default: comma)

  Returns:
    String representation of the primitive value.

  Examples:
    (encode nil)          ;=> \"null\"
    (encode true)         ;=> \"true\"
    (encode 42)           ;=> \"42\"
    (encode \"simple\")   ;=> \"simple\"
    (encode \"has, comma\") ;=> \"\\\"has, comma\\\"\""
  ([value]
   (encode value const/default-delimiter))
  ([value delimiter]
   (cond
     ;; nil → "null"
     (nil? value)
     const/null-literal

     ;; Booleans → "true" or "false"
     (boolean? value)
     (if value const/true-literal const/false-literal)

     ;; Numbers → string representation
     (number? value)
     (str value)

     ;; Strings → potentially quoted
     (string? value)
     (quote/maybe-quote value delimiter)

     ;; Should not reach here if normalized properly
     :else
     (throw (ex-info "Cannot encode non-primitive value"
                     {:value value
                      :type (type value)})))))
