# Changelog

All notable changes to this project will be documented in this file.

This project uses date-based versioning: `YYYY.MM.DD-N` where N is the number of commits since the last release.

This library implements [TOON v1.4 specification](https://github.com/toon-format/spec) (2025-11-05).

## [2025.11.11-3] - 2025-11-11

### Changed

- **TOON v1.4 compliance** - Updated to TOON specification v1.4 (2025-11-05)
- **Negative zero normalization** - Parser now normalizes `-0` to `0` per v1.4 spec requirement
- **Updated documentation** - All spec version references updated from v1.3 to v1.4

### Technical Details

- Updated `com.vadelabs.toon.decode.parser/number` to normalize negative zero
- Updated README.md spec badges and references to v1.4
- Updated SPEC.md version from 1.3 to 1.4
- Updated build.clj pom description to reference v1.4
- All 340 tests passing with 792 assertions

## [2025.11.05-43] - 2025-11-05

First public release! 🎉

A Clojure/ClojureScript implementation of TOON (Token-Oriented Object Notation) - a compact format for passing data to LLMs with significantly fewer tokens than JSON.

### What's included

- **Full TOON v1.3 support** - encode and decode between Clojure data and TOON format
- **Three array styles** - inline for primitives, tabular for uniform objects, list for mixed data
- **Flexible options** - choose your delimiter (comma, tab, pipe), add length markers, adjust indentation
- **Smart string handling** - only quotes when necessary, supports Unicode and emoji
- **Both platforms** - works in Clojure (JVM) and ClojureScript
- **Well tested** - 340+ tests with 90%+ code coverage including property-based roundtrip testing
- **Great errors** - helpful messages with suggestions when things go wrong
- **Comprehensive docs** - README with examples, API reference, and contribution guidelines
- **CI/CD** - Automated testing and deployment to Clojars via GitHub Actions
- **Smart versioning** - Version number reflects commits since last release

### Why use TOON?

Saves tokens when sending structured data to LLMs:
- 49% fewer tokens than formatted JSON
- 28% fewer than minified JSON
- Works best for uniform arrays of objects (like database query results)

### Getting started

```clojure
;; Add to deps.edn
com.vadelabs/toon {:mvn/version "2025.11.05-43"}

;; Use it
(require '[com.vadelabs.toon.interface :as toon])

(toon/encode {:users [{:id 1 :name "Alice"} {:id 2 :name "Bob"}]})
;=> "users[2]{id,name}:\n  1,Alice\n  2,Bob"
```

### Links

- [TOON Specification](https://github.com/toon-format/spec)
- [Reference Implementation (TypeScript)](https://github.com/toon-format/toon)
- [Other Implementations](https://github.com/toon-format/toon#other-implementations)

[2025.11.05-43]: https://github.com/vadelabs/toon/releases/tag/v2025.11.05-43
