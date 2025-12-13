# String Protocol Implementation in liar

## Summary

Implement the core protocols for `String` in liar, building on the lIR Unicode string primitives. Strings become first-class sequences where iteration yields grapheme clusters, enabling `map`, `filter`, `reduce` and all Seq operations to work correctly with Unicode text.

## Prerequisites

- **003-xxxxx-high-lir_unicode_strings.md** — lIR must provide the grapheme-aware primitives
- **001-xxxxx-high-protocol_default_implementations.md** — protocol defaults enable String to get `map`/`filter`/`reduce` from Seq

## Design

Strings in liar are:
- **Immutable** — all operations return new strings
- **Iterable by grapheme** — `(first "👨‍👩‍👧‍👦")` returns `"👨‍👩‍👧‍👦"`, not a byte
- **Protocol-based** — implements Seq, Counted, Indexed, Eq, Ord, Hash, Show

Since lIR handles the Unicode complexity, liar's job is to wire up the protocols and provide ergonomic functions.

## Protocol Implementations

### Seq (the core abstraction)

```lisp
(extend-protocol Seq String
  (first [self]
    (if (string-empty? self)
        nil
        (grapheme-iterator-peek (string-grapheme-iterator self))))
  
  (rest [self]
    (if (string-empty? self)
        nil
        (let ((iter (string-grapheme-iterator self)))
          (grapheme-iterator-next iter)  ; skip first
          (grapheme-iterator-rest iter)))))
```

With protocol defaults, this gives us `map`, `filter`, `reduce`, `take`, `drop`, etc. for free:

```lisp
;; All of these just work:
(map identity "hello")           ; => ("h" "e" "l" "l" "o")
(filter (fn (g) (not (= g " "))) "a b c")  ; => ("a" "b" "c")
(reduce str "" "abc")            ; => "abc"
(take 2 "hello")                 ; => ("h" "e")
```

### Counted

```lisp
(extend-protocol Counted String
  (count [self]
    (string-grapheme-count self)))  ; O(n), that's fine
```

Note: This is grapheme count, not byte count. `(count "👨‍👩‍👧‍👦")` is 1.

### Indexed

```lisp
(extend-protocol Indexed String
  (nth [self n]
    (string-grapheme-at self n)))  ; O(n), that's fine
```

Random access is O(n) because we must iterate to find grapheme boundaries. This is correct behavior — don't use `nth` in a loop, use `Seq` iteration instead.

### Eq (Unicode-normalized equality)

```lisp
(extend-protocol Eq String
  (eq [self other]
    (string-equal? self other)))  ; NFC-normalized comparison
```

This means `"é"` (single codepoint) equals `"é"` (e + combining acute).

For byte-exact comparison (faster, for interned strings):

```lisp
(defun string=? (a b)
  (string-equal-exact? a b))
```

### Ord (Unicode collation)

```lisp
(extend-protocol Ord String
  (cmp [self other]
    (let ((result (string-compare self other)))
      (cond
        ((< result 0) :lt)
        ((> result 0) :gt)
        (true :eq)))))
```

### Hash

```lisp
(extend-protocol Hash String
  (hash [self]
    (string-hash self)))  ; lIR primitive, hashes normalized form
```

### Show

```lisp
(extend-protocol Show String
  (str [self] self)  ; strings are already strings
  
  (repr [self]
    ;; Quoted representation for debugging
    (string-concat "\"" (string-concat (string-escape self) "\""))))
```

## String Functions (lib/string.liar)

Beyond protocols, provide convenient string functions:

### Construction

```lisp
;; String from sequence of graphemes/strings
(defun str (&rest parts)
  (reduce string-concat "" parts))

;; Repeat a string n times
(defun string-repeat (s n)
  (if (<= n 0)
      ""
      (reduce string-concat "" (take n (repeat s)))))

;; Join sequence with separator
(defun string-join (sep xs)
  (if (empty? xs)
      ""
      (reduce (fn (acc x) (str acc sep x))
              (first xs)
              (rest xs))))
```

### Predicates

```lisp
(defun blank? (s)
  "True if string is empty or only whitespace."
  (or (string-empty? s)
      (every? whitespace? s)))

(defun whitespace? (grapheme)
  "True if grapheme is whitespace."
  (string-whitespace? grapheme))  ; lIR primitive using Unicode properties
```

### Searching

```lisp
(defun includes? (s needle)
  (>= (string-find s needle) 0))

(defun starts-with? (s prefix)
  (string-starts-with? s prefix))

(defun ends-with? (s suffix)
  (string-ends-with? s suffix))
```

### Transformation

```lisp
(defun trim (s)
  "Remove leading and trailing whitespace."
  (string-trim s))  ; lIR primitive

(defun trim-left (s)
  (string-trim-left s))

(defun trim-right (s)
  (string-trim-right s))

(defun upper (s)
  "Convert to uppercase (may change grapheme count)."
  (string-upcase s))

(defun lower (s)
  (string-downcase s))

(defun capitalize (s)
  "Capitalize first grapheme."
  (if (string-empty? s)
      s
      (str (upper (first s)) (rest-as-string s))))

(defun reverse (s)
  "Reverse by graphemes (not bytes!)."
  (reduce (fn (acc g) (str g acc)) "" s))
```

### Splitting

```lisp
(defun split (s sep)
  "Split string by separator, return list of strings."
  (string-split s sep))  ; lIR primitive for efficiency

(defun lines (s)
  "Split by newlines."
  (split s "\n"))

(defun words (s)
  "Split by whitespace."
  (filter (fn (w) (not (blank? w))) 
          (string-split-whitespace s)))
```

### Parsing

```lisp
(defun parse-int (s)
  "Parse string as integer, nil on failure."
  (string-parse-int s))  ; lIR primitive

(defun parse-float (s)
  "Parse string as float, nil on failure."
  (string-parse-float s))
```

### Building Strings Efficiently

```lisp
;; For building large strings, use a builder to avoid O(n²) concat
(defmacro with-string-builder (name &rest body)
  `(let ((,name (string-builder-new)))
     ,@body
     (string-builder-finish ,name)))

;; Example usage:
(with-string-builder sb
  (string-builder-append sb "Hello, ")
  (string-builder-append sb name)
  (string-builder-append sb "!"))
```

## String Interpolation (Future)

With macros, we could add string interpolation:

```lisp
;; Reader macro: #"Hello, {name}!"
;; Expands to: (str "Hello, " name "!")

(defmacro str-interp (template)
  ;; Parse template, find {} sections, build str call
  ...)
```

This depends on having a reader and reader macros — future work.

## Regex (Future)

Unicode-aware regex would build on ICU's regex engine:

```lisp
(defprotocol Pattern
  (match [self s] "Returns match result or nil")
  (match-all [self s] "Returns seq of all matches")
  (replace [self s replacement] "Replace matches"))

(extend-protocol Pattern Regex
  (match [self s] (regex-match self s))
  ...)

;; Constructor
(defun regex (pattern)
  (regex-compile pattern))  ; lIR primitive wrapping ICU regex

;; Usage
(match (regex "\\d+") "abc123def")  ; => {:start 3 :end 6 :text "123"}
```

## Acceptance Criteria

### Core Protocols
- [ ] String implements Seq with grapheme iteration
- [ ] `(first "👨‍👩‍👧‍👦")` returns `"👨‍👩‍👧‍👦"` (single grapheme)
- [ ] `(count "héllo")` returns 5 (not byte count)
- [ ] `(nth "abc" 1)` returns `"b"`
- [ ] `(eq "é" "e\u0301")` returns true (normalized)
- [ ] `(map identity "hi")` returns `("h" "i")`

### String Functions
- [ ] `str` concatenates multiple arguments
- [ ] `string-join` joins with separator
- [ ] `split` returns list of strings
- [ ] `trim`, `upper`, `lower` work correctly
- [ ] `includes?`, `starts-with?`, `ends-with?` work

### Edge Cases
- [ ] Empty string works with all operations
- [ ] ZWJ sequences (emoji families) are single graphemes
- [ ] Combining characters form single graphemes
- [ ] Case conversion can change length (`"ß"` -> `"SS"`)

## Test Scenarios

```gherkin
Feature: String protocols in liar

  Scenario: Map over Unicode string
    Given the program:
      """
      (defun main ()
        (count (map identity "👨‍👩‍👧‍👦a")))
      """
    When I compile and run it
    Then the result should be 2

  Scenario: Filter graphemes
    Given the program:
      """
      (defun vowel? (g)
        (includes? "aeiouAEIOU" g))
      
      (defun main ()
        (count (filter vowel? "hello")))
      """
    When I compile and run it
    Then the result should be 2

  Scenario: String equality with normalization
    Given the program:
      """
      (defun main ()
        (if (eq "café" "cafe\u0301")
            1
            0))
      """
    When I compile and run it
    Then the result should be 1

  Scenario: Reverse preserves graphemes
    Given the program:
      """
      (defun main ()
        (first (reverse "a👨‍👩‍👧‍👦b")))
      """
    When I compile and run it
    Then the result should be "b"
    # NOT a broken emoji from reversing bytes

  Scenario: Join strings
    Given the program:
      """
      (defun main ()
        (string-join ", " (list "a" "b" "c")))
      """
    When I compile and run it
    Then the result should be "a, b, c"

  Scenario: Split string
    Given the program:
      """
      (defun main ()
        (count (split "a,b,c" ",")))
      """
    When I compile and run it
    Then the result should be 3
```

## File Structure

```
lib/
  string.liar          ; String functions and protocol implementations
  
liar-cert/features/
  string_protocols.feature    ; Protocol behavior tests
  string_functions.feature    ; Function tests
  string_unicode.feature      ; Unicode edge case tests
```

## Dependencies

- lIR Unicode string primitives (003-xxxxx)
- Protocol default implementations (001-xxxxx)
- Core protocols: Seq, Counted, Indexed, Eq, Ord, Hash, Show

## Notes

The key insight is that liar doesn't need to understand Unicode — it just needs to call the right lIR primitives. The `Seq` protocol abstraction means grapheme iteration "just works" with all the existing sequence functions.

This is the payoff for getting the lIR layer right: liar code like `(map f some-string)` automatically does the right thing with emoji, combining characters, and all of Unicode's complexity, because `first` and `rest` return graphemes.

The O(n) cost of `count` and `nth` is a deliberate tradeoff — correctness over raw speed. For performance-critical byte manipulation (parsing, binary protocols), users can drop to `string-bytes` and work with raw memory.
