# TOON: Token-Oriented Object Notation

[![Clojars Project](https://img.shields.io/clojars/v/com.vadelabs/toon.svg)](https://clojars.org/com.vadelabs/toon)
[![CI](https://github.com/vadelabs/toon/actions/workflows/ci.yml/badge.svg)](https://github.com/vadelabs/toon/actions/workflows/ci.yml)
[![SPEC v1.3](https://img.shields.io/badge/spec-v1.3-lightgray)](https://github.com/toon-format/spec)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

A Clojure/ClojureScript implementation of **Token-Oriented Object Notation** – a compact, human-readable serialization format designed for passing structured data to Large Language Models with significantly reduced token usage.

TOON achieves **49% fewer tokens than formatted JSON** (28% vs compact JSON) while maintaining explicit structure that helps LLMs parse and validate data reliably. It's intended for *LLM input* as a lossless, drop-in representation of JSON data.

> **Specification:** This library implements [TOON v1.3](https://github.com/toon-format/spec) specification
> **Reference Implementation:** [TypeScript/JavaScript](https://github.com/toon-format/toon)

## Why TOON?

When working with Large Language Models, token efficiency directly impacts cost, context window usage, and processing speed. **LLM tokens still cost money** – and standard JSON is verbose and token-expensive.

TOON's sweet spot is **uniform arrays of objects** – multiple fields per row, same structure across items. It borrows YAML's indentation-based structure for nested objects and CSV's tabular format for uniform data rows, then optimizes both for token efficiency in LLM contexts.

### Token Efficiency

Based on benchmarks using the GPT-5 `o200k_base` tokenizer:

- **49.1%** reduction vs formatted JSON (2-space indentation)
- **28.0%** reduction vs compact JSON (minified)
- **39.4%** reduction vs YAML
- **56.0%** reduction vs XML

Real-world examples:

- **GitHub repositories** (100 items): 42.3% fewer tokens than JSON
- **Daily analytics** (180 days): 58.9% fewer tokens than JSON
- **E-commerce orders**: 35.4% fewer tokens than JSON

### Key Features

- 💸 **Token-efficient:** Eliminates redundant punctuation and repeated keys
- 🤿 **LLM-friendly guardrails:** Explicit lengths and fields enable validation
- 🍱 **Minimal syntax:** Removes braces, brackets, and most quotes
- 📐 **Indentation-based:** Uses whitespace like YAML instead of braces
- 🧺 **Tabular arrays:** Declare keys once, stream data as rows

### When to Use TOON

**TOON excels at:**

- Uniform arrays of objects (same fields, primitive values)
- Large datasets with consistent structure
- Tabular data with multiple rows

**JSON is better for:**

- Non-uniform data with varying field sets
- Deeply nested structures
- Mixed-type collections

**CSV is more compact for:**

- Flat, uniform tables without any nesting
- Data without nested objects or arrays

## Installation

### Clojure CLI/deps.edn

```clojure
com.vadelabs/toon {:mvn/version "2025.11.05-43"}
```

### Leiningen/Boot

```clojure
[com.vadelabs/toon "2025.11.05-43"]
```

## Quick Start

```clojure
(require '[com.vadelabs.toon.interface :as toon])

;; Encode Clojure data to TOON
(toon/encode {:name "Alice" :age 30 :tags ["dev" "rust"]})
;=> "name: Alice\nage: 30\ntags[2]: dev,rust"

;; Decode TOON to Clojure data
(toon/decode "name: Alice\nage: 30\ntags[2]: dev,rust")
;=> {"name" "Alice", "age" 30.0, "tags" ["dev" "rust"]}
```

## Format Examples

### Objects

**JSON:**
```json
{
  "name": "Alice",
  "age": 30,
  "active": true
}
```

**TOON:**
```
name: Alice
age: 30
active: true
```

### Nested Objects

**JSON:**
```json
{
  "user": {
    "name": "Alice",
    "email": "alice@example.com"
  }
}
```

**TOON:**
```
user:
  name: Alice
  email: alice@example.com
```

### Arrays of Primitives (Inline)

**JSON:**
```json
{
  "tags": ["reading", "gaming", "coding"]
}
```

**TOON:**
```
tags[3]: reading,gaming,coding
```

### Arrays of Objects (Tabular Format)

This is TOON's sweet spot – uniform arrays of objects with consistent fields:

**JSON:**
```json
{
  "users": [
    {"id": 1, "name": "Alice", "role": "admin"},
    {"id": 2, "name": "Bob", "role": "user"}
  ]
}
```

**TOON:**
```
users[2]{id,name,role}:
  1,Alice,admin
  2,Bob,user
```

The tabular format eliminates repeated keys, providing significant token savings for large datasets.

### Arrays of Mixed Items (List Format)

For non-uniform data, TOON uses list format:

**TOON:**
```
items[3]:
  - name: Laptop
    price: 999
  - name: Mouse
    price: 29
  - name: Keyboard
    price: 79
```

## API Reference

### `encode`

Encodes Clojure data structures to TOON format.

```clojure
(encode input)
(encode input options)
```

**Parameters:**
- `input` - Any Clojure value (normalized to JSON-compatible types)
- `options` - Optional map:
  - `:indent` - Spaces per indentation level (default: 2)
  - `:delimiter` - Array value delimiter: `","` (default), `"\t"`, or `"|"`
  - `:length-marker` - Array length marker: `"#"` or `false` (default: false)

**Returns:** String in TOON format

**Examples:**

```clojure
;; Basic encoding
(encode {:name "Ada" :tags ["reading" "gaming"]})
;=> "name: Ada\ntags[2]: reading,gaming"

;; Custom delimiter
(encode {:tags ["a" "b" "c"]} {:delimiter "\t"})
;=> "tags[3\t]: a\tb\tc"

;; Length marker prefix
(encode {:items [1 2 3]} {:length-marker "#"})
;=> "items[#3]: 1,2,3"

;; Tabular array format
(encode [{:id 1 :name "Alice"}
         {:id 2 :name "Bob"}])
;=> "[2]{id,name}:\n  1,Alice\n  2,Bob"
```

### `decode`

Decodes TOON format to Clojure data structures.

```clojure
(decode input)
(decode input options)
```

**Parameters:**
- `input` - String in TOON format
- `options` - Optional map:
  - `:indent` - Spaces per indentation level (default: 2)
  - `:strict` - Enable strict validation (default: true)

**Returns:** Clojure data structure (maps, vectors, primitives)

**Examples:**

```clojure
;; Basic decoding
(decode "name: Ada\ntags[2]: reading,gaming")
;=> {"name" "Ada", "tags" ["reading" "gaming"]}

;; Tabular array
(decode "[2]{id,name}:\n  1,Alice\n  2,Bob")
;=> [{"id" 1.0, "name" "Alice"} {"id" 2.0, "name" "Bob"}]

;; Inline array
(decode "[3]: 1,2,3")
;=> [1.0 2.0 3.0]

;; Relaxed mode (allows tabs, inconsistent indentation)
(decode "name: Ada" {:strict false})
;=> {"name" "Ada"}
```

## Format Specification

### Primitives

```
string: Hello World
number: 42
float: 3.14
boolean: true
nil: null
```

### Quoted Strings

Strings are quoted when they contain special characters:

```
comma: "a,b"
colon: "key:value"
reserved: "true"
newline: "line1\nline2"
```

### Objects

Key-value pairs separated by colons:

```
name: Alice
age: 30
```

Nested objects use indentation:

```
user:
  name: Alice
  email: alice@example.com
```

### Arrays

**Inline format** (primitives):
```
tags[3]: reading,gaming,coding
```

**Tabular format** (objects with same keys):
```
[3]{id,name}:
  1,Alice
  2,Bob
  3,Carol
```

**List format** (mixed items):
```
items[2]:
  - name: Laptop
    price: 999
  - name: Mouse
    price: 29
```

### Options

**Custom delimiter:**
```
tags[3|]: a|b|c
tags[3\t]: a\tb\tc
```

**Length marker:**
```
items[#3]: 1,2,3
```

## Type Normalization

TOON normalizes Clojure types to JSON-compatible values:

- Keywords → Strings: `:name` → `"name"`
- Sets → Sorted vectors: `#{3 1 2}` → `[1 2 3]`
- All numbers → Doubles: `42` → `42.0`
- Maps → String-keyed maps: `{:a 1}` → `{"a" 1.0}`

## Testing

```bash
# Run all Clojure tests
bb test

# Run all tests (Clojure + Babashka)
bb test:all

# Run CI pipeline with tests
bb ci

# Generate test coverage report
bb coverage
```

The library includes:
- 340+ unit tests with **90%+ code coverage**
- Property-based tests using test.check
- Comprehensive roundtrip testing
- Edge case coverage

Coverage reports are generated in `target/coverage/` including:
- HTML report: `target/coverage/index.html`
- Codecov JSON: `target/coverage/codecov.json`

## Contributing

We welcome contributions! Please see [CONTRIBUTING.md](CONTRIBUTING.md) for:
- Development setup
- Coding guidelines
- Testing requirements
- Pull request process

### Quick Contribution Guide

1. Fork the repository
2. Create a feature branch: `git checkout -b feature/my-feature`
3. Make your changes with tests
4. Run tests: `bb test`
5. Commit with clear messages: `git commit -m "add feature X"`
6. Push and create a pull request

## Specification

This implementation follows the [TOON v1.3 specification](https://github.com/toon-format/spec/blob/main/SPEC.md) (2025-10-31).

For detailed format rules, edge cases, and conformance requirements, see:
- **[Full Specification](https://github.com/toon-format/spec/blob/main/SPEC.md)** - Complete technical specification
- **[Conformance Tests](https://github.com/toon-format/spec/tree/main/tests)** - Language-agnostic test fixtures
- **[Examples](https://github.com/toon-format/spec/tree/main/examples)** - Example TOON files
- **[Changelog](https://github.com/toon-format/spec/blob/main/CHANGELOG.md)** - Spec version history

## Benchmarks

Detailed benchmarks comparing TOON against JSON, YAML, XML, and CSV across multiple datasets and LLM models are available in the [reference implementation repository](https://github.com/toon-format/toon/tree/main/benchmarks).

Key findings:
- **Token efficiency:** 49% fewer tokens than formatted JSON on average
- **Retrieval accuracy:** 70.1% (TOON) vs 65.4% (JSON) across 4 LLMs
- **Best case:** 58.9% reduction for uniform tabular data (daily analytics)

Token counts are measured using the GPT-5 `o200k_base` tokenizer. Actual savings vary by model and tokenizer.

## Other Implementations

### Official Implementations

- **TypeScript/JavaScript:** [toon-format/toon](https://github.com/toon-format/toon) (reference implementation)
- **Python:** [toon-format/toon-python](https://github.com/toon-format/toon-python) *(in development)*
- **Rust:** [toon-format/toon-rust](https://github.com/toon-format/toon-rust) *(in development)*

### Community Implementations

- **.NET:** [ToonSharp](https://github.com/0xZunia/ToonSharp)
- **C++:** [ctoon](https://github.com/mohammadraziei/ctoon)
- **Crystal:** [toon-crystal](https://github.com/mamantoha/toon-crystal)
- **Dart:** [toon](https://github.com/wisamidris77/toon)
- **Elixir:** [toon_ex](https://github.com/kentaro/toon_ex)
- **Gleam:** [toon_codec](https://github.com/axelbellec/toon_codec)
- **Go:** [gotoon](https://github.com/alpkeskin/gotoon)
- **Java:** [JToon](https://github.com/felipestanzani/JToon)
- **Lua/Neovim:** [toon.nvim](https://github.com/thalesgelinger/toon.nvim)
- **OCaml:** [ocaml-toon](https://github.com/davesnx/ocaml-toon)
- **PHP:** [toon-php](https://github.com/HelgeSverre/toon-php)
- **Python:** [python-toon](https://github.com/xaviviro/python-toon)
- **Ruby:** [toon-ruby](https://github.com/andrepcg/toon-ruby)
- **Swift:** [TOONEncoder](https://github.com/mattt/TOONEncoder)

> **Note:** When implementing TOON in other languages, follow the [specification](https://github.com/toon-format/spec/blob/main/SPEC.md) to ensure compatibility. The [conformance tests](https://github.com/toon-format/spec/tree/main/tests) provide language-agnostic validation.

## Roadmap

- [ ] Conformance test suite integration
- [ ] Performance benchmarks vs JSON for Clojure
- [ ] ClojureScript browser optimization
- [ ] Streaming encoder/decoder
- [ ] Custom type handlers

## License

Copyright © 2025 Vade Labs Pvt. Ltd.

Distributed under the MIT License. See [LICENSE](LICENSE) for details.

## Links

- [GitHub Repository](https://github.com/vadelabs/toon)
- [Clojars Package](https://clojars.org/com.vadelabs/toon)
- [Issue Tracker](https://github.com/vadelabs/toon/issues)
- [Changelog](CHANGELOG.md)
