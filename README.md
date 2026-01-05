# TOON: Token-Oriented Object Notation

[![Clojars Project](https://img.shields.io/clojars/v/com.vadelabs/toon.svg)](https://clojars.org/com.vadelabs/toon)
[![CI](https://github.com/vadelabs/toon/actions/workflows/ci.yml/badge.svg)](https://github.com/vadelabs/toon/actions/workflows/ci.yml)
[![SPEC v3.0](https://img.shields.io/badge/spec-v3.0-lightgray)](https://github.com/toon-format/spec)

A Clojure/ClojureScript library for [TOON](https://github.com/toon-format/spec) - a data format that uses ~50% fewer tokens than JSON when talking to LLMs.

```clojure
(require '[com.vadelabs.toon.core :as toon])

(toon/encode {:name "Alice" :age 30 :tags ["dev" "clj"]})
;=> "name: Alice\nage: 30\ntags[2]: dev,clj"

(toon/decode "name: Alice\nage: 30\ntags[2]: dev,clj")
;=> {"name" "Alice", "age" 30.0, "tags" ["dev" "clj"]}
```

## Why?

LLM tokens cost money. JSON is verbose. TOON fixes that.

The format borrows YAML's indentation for nesting and CSV's row-based structure for tabular data. The sweet spot is **arrays of uniform objects** - declare the keys once, then stream values as rows:

```
users[3]{id,name,role}:
  1,Alice,admin
  2,Bob,user
  3,Carol,user
```

vs JSON:

```json
{"users":[{"id":1,"name":"Alice","role":"admin"},{"id":2,"name":"Bob","role":"user"},{"id":3,"name":"Carol","role":"user"}]}
```

Benchmarks using GPT's `o200k_base` tokenizer show 49% fewer tokens than formatted JSON, 28% fewer than minified JSON.

## Installation

```clojure
;; deps.edn
com.vadelabs/toon {:mvn/version "2026.01.05"}

;; Leiningen
[com.vadelabs/toon "2026.01.05"]
```

Works on Clojure, ClojureScript, and Babashka.

## Format Overview

**Objects** - key-value pairs, indentation for nesting:
```
name: Alice
address:
  city: Portland
  zip: 97201
```

**Inline arrays** - primitives on one line:
```
tags[3]: reading,gaming,coding
```

**Tabular arrays** - uniform objects, keys declared once:
```
users[2]{id,name}:
  1,Alice
  2,Bob
```

**List arrays** - mixed or nested items:
```
items[2]:
  - name: Laptop
    price: 999
  - name: Mouse
    price: 29
```

Strings get quoted when they contain special characters (commas, colons, etc).

## API

### encode

```clojure
(toon/encode data)
(toon/encode data {:indent 2
                   :delimiter ","      ; or "\t" or "|"
                   :key-collapsing :off ; or :safe
                   :replacer (fn [k v path] v)})
```

The `:replacer` option works like JSON.stringify's replacer - return `nil` to omit a field:

```clojure
(toon/encode {:name "Alice" :password "secret"}
             {:replacer (fn [k v _] (when-not (= k "password") v))})
;=> "name: Alice"
```

### decode

```clojure
(toon/decode toon-string)
(toon/decode toon-string {:indent 2
                          :strict true})
```

### Streaming

For large documents, use event-based decoding:

```clojure
(toon/events toon-string)
;=> ({:type :start-object} {:type :key :key "name"} ...)

(toon/events->value (toon/events toon-string))
;=> {"name" "Alice" ...}
```

## Type Mapping

Clojure types are normalized to JSON-compatible values:

| Clojure | TOON/JSON |
|---------|-----------|
| `:keyword` | `"keyword"` |
| `#{1 2 3}` | `[1, 2, 3]` (sorted) |
| `42` | `42.0` |
| `{:a 1}` | `{"a": 1}` |

## When to Use

TOON shines with uniform, tabular data - think database rows, API responses with repeated structure, analytics data.

For deeply nested or irregular data, the savings are smaller. For flat CSVs with no nesting, plain CSV is more compact.

## Testing

```bash
bb test       # Run all tests (clj + bb + cljs)
bb test:clj   # Clojure only
bb coverage   # Generate coverage report
```

540+ tests, 90%+ coverage.

## Spec & Other Implementations

This implements [TOON v3.0](https://github.com/toon-format/spec). The reference implementation is in [TypeScript](https://github.com/toon-format/toon).

Community implementations exist for Go, Python, Ruby, Swift, PHP, and others - see the [spec repo](https://github.com/toon-format/spec) for a full list.

## License

MIT. Copyright 2025 Vade Labs.
