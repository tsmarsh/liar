# Unicode-Correct String Support in lIR

## Summary

Add first-class string support to lIR with proper Unicode semantics. Strings iterate by grapheme cluster (not byte or codepoint), comparisons are Unicode-aware, and the implementation delegates to ICU/OS facilities for correctness.

## Motivation

Strings are foundational — parsing, I/O, error messages, and eventually bootstrapping a compiler all depend on them. Most languages get strings wrong:

- C: null-terminated bytes, buffer overflows, no encoding
- Java/JavaScript: UTF-16 with surrogate pair hell
- Most others: UTF-8 bytes but indexing by byte not character

"Best in class" means:
- UTF-8 storage (the world has settled on this)
- Grapheme cluster iteration ("👨‍👩‍👧‍👦" is ONE character, not 7 codepoints)
- Unicode-aware comparison and normalization
- Efficient construction and concatenation

Getting this right requires ICU or OS Unicode facilities — the grapheme break algorithm (UAX #29) and character database are not something to hand-roll.

## Design Principles

1. **Grapheme clusters are the unit of iteration** — `first` on a string returns a grapheme, not a byte
2. **UTF-8 is the storage format** — compact, compatible with I/O
3. **ICU/OS does the hard work** — grapheme breaking, normalization, collation
4. **O(n) indexing is acceptable** — most string operations are iteration, not random access
5. **Immutable by default** — builders for efficient construction

## Representation

```
;; Core string structure
;; Opaque from user perspective — implementation may cache grapheme info
struct String {
  bytes: ptr,              ; UTF-8 data (owned)
  byte_len: i64,           ; length in bytes
  flags: i64               ; encoding validation status, interning, etc.
}

;; Iterator for grapheme traversal (wraps ICU break iterator)
struct GraphemeIterator {
  string: ptr,             ; reference to source string
  byte_pos: i64,           ; current position in bytes
  icu_handle: ptr          ; ICU UBreakIterator* or platform equivalent
}

;; Builder for efficient construction
struct StringBuilder {
  chunks: ptr,             ; list of string fragments
  total_bytes: i64         ; running total for final allocation
}
```

## lIR Operations

### String Creation

```lisp
;; From UTF-8 bytes (validates encoding, returns nil on invalid)
(string-from-utf8 ptr len)         ; (ptr, i64) -> String | nil

;; String literal (compile-time validated)
(string "hello")                    ; -> String

;; From single codepoint (for building graphemes)
(string-from-codepoint cp)          ; (i64) -> String

;; Empty string constant
(string-empty)                      ; -> String
```

### Byte-Level Access (for I/O, FFI)

```lisp
;; Get underlying UTF-8 bytes (borrowed pointer, valid while string lives)
(string-bytes s)                    ; (String) -> ptr

;; Byte length (NOT character count)
(string-byte-len s)                 ; (String) -> i64

;; Check if empty
(string-empty? s)                   ; (String) -> i1
```

### Grapheme Iteration (the core interface)

```lisp
;; Create iterator positioned at start
(string-grapheme-iterator s)        ; (String) -> GraphemeIterator

;; Get current grapheme without advancing (nil at end)
(grapheme-iterator-peek iter)       ; (GraphemeIterator) -> String | nil

;; Advance and return next grapheme (nil at end)
(grapheme-iterator-next iter)       ; (GraphemeIterator) -> String | nil

;; Check if exhausted
(grapheme-iterator-done? iter)      ; (GraphemeIterator) -> i1

;; Get remainder as new string (from current position to end)
(grapheme-iterator-rest iter)       ; (GraphemeIterator) -> String

;; Clone iterator (for backtracking)
(grapheme-iterator-clone iter)      ; (GraphemeIterator) -> GraphemeIterator
```

### Grapheme Counting and Indexing (O(n), that's fine)

```lisp
;; Count graphemes (iterates whole string)
(string-grapheme-count s)           ; (String) -> i64

;; Get nth grapheme (iterates n times)
(string-grapheme-at s n)            ; (String, i64) -> String | nil
```

### Comparison (Unicode-aware)

```lisp
;; Equality (after NFC normalization)
(string-equal? s1 s2)               ; (String, String) -> i1

;; Byte-exact equality (faster, for interned/known-normalized)
(string-equal-exact? s1 s2)         ; (String, String) -> i1

;; Ordering (locale-independent, suitable for sorting)
(string-compare s1 s2)              ; (String, String) -> i64 (-1, 0, 1)

;; Locale-aware ordering (for user-facing sorts)
(string-compare-locale s1 s2 loc)   ; (String, String, ptr) -> i64
```

### Concatenation and Building

```lisp
;; Concatenate two strings (allocates new)
(string-concat s1 s2)               ; (String, String) -> String

;; Builder for many concatenations
(string-builder-new)                ; () -> StringBuilder
(string-builder-append b s)         ; (StringBuilder, String) -> StringBuilder
(string-builder-append-codepoint b cp) ; (StringBuilder, i64) -> StringBuilder
(string-builder-finish b)           ; (StringBuilder) -> String
```

### Substring (by grapheme position)

```lisp
;; Substring from grapheme start to end (exclusive)
;; O(end) since we must iterate to find positions
(string-substring s start end)      ; (String, i64, i64) -> String
```

### Searching

```lisp
;; Find first occurrence (returns grapheme index or -1)
(string-find s needle)              ; (String, String) -> i64

;; Check prefix/suffix
(string-starts-with? s prefix)      ; (String, String) -> i1
(string-ends-with? s suffix)        ; (String, String) -> i1
```

### Case Conversion (Unicode-aware)

```lisp
;; These can change grapheme count! (ß -> SS)
(string-upcase s)                   ; (String) -> String
(string-downcase s)                 ; (String) -> String
(string-titlecase s)                ; (String) -> String

;; Case-insensitive comparison
(string-equal-casefold? s1 s2)      ; (String, String) -> i1
```

### Normalization

```lisp
;; Unicode normalization forms
(string-normalize-nfc s)            ; (String) -> String
(string-normalize-nfd s)            ; (String) -> String
(string-normalize-nfkc s)           ; (String) -> String
(string-normalize-nfkd s)           ; (String) -> String
```

## Implementation Strategy

### Phase 1: Core with ICU

Link against ICU4C and implement:

```c
// C runtime functions that lIR calls
LiarString* liar_string_from_utf8(const uint8_t* bytes, int64_t len);
LiarGraphemeIterator* liar_grapheme_iterator_new(LiarString* s);
LiarString* liar_grapheme_iterator_next(LiarGraphemeIterator* iter);
int64_t liar_string_compare(LiarString* a, LiarString* b);
// ... etc
```

ICU provides:
- `ubrk_open(UBRK_CHARACTER, ...)` for grapheme break iteration
- `unorm2_normalize()` for normalization
- `ucol_strcoll()` for collation

### Phase 2: Platform-Native Options

Abstract the Unicode backend:
- ICU on Linux (always available, comprehensive)
- CoreFoundation/Foundation on macOS (CFStringGetRangeOfComposedCharactersAtIndex)
- ICU or Windows.Globalization on Windows

### Phase 3: Compile-Time Optimization

For string literals:
- Validate UTF-8 at compile time
- Pre-compute grapheme count and boundaries for small strings
- Intern common strings

## Memory Management

Strings follow lIR's ownership model:

```lisp
;; Owned string — caller must drop
(let ((s (string "hello")))
  (use s)
  (drop s))

;; Borrowed iteration — iterator borrows from string
(let ((s (string "hello"))
      (iter (string-grapheme-iterator s)))  ; iter borrows s
  (grapheme-iterator-next iter)              ; fine, s still alive
  (drop s))                                  ; drop s, iter invalid
```

Iterators are lightweight (pointer + position + ICU handle) and tied to their source string's lifetime.

## Acceptance Criteria

### Phase 1 (Core)
- [ ] `String` struct defined in lIR with UTF-8 storage
- [ ] `string-from-utf8` validates and constructs
- [ ] String literals work: `(string "hello")`
- [ ] `string-bytes` and `string-byte-len` for I/O
- [ ] `GraphemeIterator` with ICU backend
- [ ] `grapheme-iterator-next` returns single graphemes
- [ ] "👨‍👩‍👧‍👦" iterates as ONE grapheme
- [ ] "é" (composed) equals "é" (decomposed) with `string-equal?`
- [ ] Feature tests for all operations

### Phase 2 (Building)
- [ ] `string-concat` works
- [ ] `StringBuilder` for efficient multi-concat
- [ ] `string-substring` by grapheme position

### Phase 3 (Rich Operations)
- [ ] Case conversion (upcase, downcase, titlecase)
- [ ] Searching (find, starts-with?, ends-with?)
- [ ] Normalization (NFC, NFD, NFKC, NFKD)
- [ ] Locale-aware comparison

## Test Scenarios

```gherkin
Feature: Unicode-correct strings in lIR

  Scenario: Family emoji is one grapheme
    Given the lIR program:
      """
      (define (count-graphemes String) i64
        (param s String)
        (block entry
          (ret (string-grapheme-count (local s)))))
      """
    When I call count-graphemes with "👨‍👩‍👧‍👦"
    Then the result should be 1

  Scenario: Combining characters form single grapheme
    Given the lIR program:
      """
      (define (first-grapheme String) String
        (param s String)
        (block entry
          (let ((iter (string-grapheme-iterator (local s))))
            (ret (grapheme-iterator-next (local iter))))))
      """
    When I call first-grapheme with "e\u0301rest"  # é as e + combining acute
    Then the result should be "é"

  Scenario: Normalized equality
    Given the lIR program:
      """
      (define (same? String String) i1
        (params s1 String s2 String)
        (block entry
          (ret (string-equal? (local s1) (local s2)))))
      """
    When I call same? with "é" and "e\u0301"
    Then the result should be true

  Scenario: String iteration with ZWJ sequences
    Given a string containing "👩‍💻👨‍🔬" (woman technologist, man scientist)
    When I iterate graphemes
    Then I should get exactly 2 graphemes

  Scenario: German sharp S case conversion
    Given the string "straße"
    When I call string-upcase
    Then the result should be "STRASSE"
    And the grapheme count should increase from 6 to 7
```

## Performance Notes

- **Grapheme iteration**: O(1) per `next` call (ICU break iterator is incremental)
- **Grapheme count**: O(n) — must iterate whole string. Cache if needed.
- **Grapheme indexing**: O(k) where k is index — don't use for random access
- **Comparison**: O(n) but fast path for identical pointers and byte-equal+normalized strings
- **Concatenation**: O(n+m) — use StringBuilder for many appends

This is slower than byte indexing but *correct*. For byte-level manipulation (parsing, I/O), use `string-bytes` and work with raw memory.

## Dependencies

- **ICU4C** (libicu) — the standard Unicode library
  - Ubuntu/Debian: `apt install libicu-dev`
  - macOS: `brew install icu4c` or use system CoreFoundation
  - ~25MB runtime dependency

## References

- UAX #29: Unicode Text Segmentation (grapheme clusters)
- UAX #15: Unicode Normalization Forms
- ICU User Guide: https://unicode-org.github.io/icu/
- Swift String Manifesto (good design discussion)
- Rust `unicode-segmentation` crate (pure Rust alternative to ICU)

## Notes

This is foundational infrastructure. Once lIR has correct strings, liar can build its `Seq` implementation on top and get Unicode-correct `map`, `filter`, `reduce` for free.

The decision to make grapheme iteration the primary interface (rather than byte or codepoint) is deliberate — it pushes complexity to the lIR layer where we have C/ICU, leaving liar to think in terms of "characters" that actually correspond to what humans see.
