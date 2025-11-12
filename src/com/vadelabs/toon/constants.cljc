(ns com.vadelabs.toon.constants)


;; List markers
(def list-item-marker "-")
(def list-item-prefix "- ")


;; Structural characters
(def comma ",")
(def colon ":")
(def space " ")
(def pipe "|")
(def tab "\t")
(def dot ".")


;; Brackets and braces
(def open-bracket "[")
(def close-bracket "]")
(def open-brace "{")
(def close-brace "}")


;; Literals
(def null-literal "null")
(def true-literal "true")
(def false-literal "false")


;; Escape characters
(def backslash "\\")
(def double-quote "\"")
(def newline-char "\n")
(def carriage-return "\r")


;; Delimiters
(def delimiters
  {:comma comma
   :tab tab
   :pipe pipe})


(def default-delimiter (:comma delimiters))


;; Array literals
(def empty-array "[]")
(def empty-array-with-length "[0]")


;; Default options
(def default-indent 2)
