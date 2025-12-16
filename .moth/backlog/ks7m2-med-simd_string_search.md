# SIMD-Accelerated String Search

Implement high-performance string search operations using liar's SIMD vector support, inspired by [StringZilla's approach](https://ashvardanian.com/posts/search-utf8/).

**Priority:** MEDIUM (performance optimization, not blocking)

## Motivation

StringZilla achieves 50-150× speedups over ICU for Unicode case-insensitive search by:
1. Finding "safe windows" in needles that can be SIMD-scanned
2. Using script-specific folding kernels
3. Probing last bytes of UTF-8 characters (higher entropy)
4. Avoiding port pressure with ternary logic operations

liar already has lIR vector operations (`<N x type>`, `extractelement`, `insertelement`, `shufflevector`) that map directly to LLVM's SIMD support. This moth adds high-level string search built on those primitives.

## Phase 1: ASCII Case-Insensitive Search

Start with the simplest case - ASCII-only case-insensitive substring search.

### Core Algorithm

```clojure
;; The 0x20 toggle trick: ASCII uppercase and lowercase differ by bit 5
;; 'A' = 0x41, 'a' = 0x61 → differ by 0x20

(defun ascii-tolower-mask (byte: i8) -> i8
  ;; Set bit 5 for A-Z, leave others unchanged
  (let ((is-upper (and (>= byte 65) (<= byte 90))))
    (if is-upper (or byte 32) byte)))
```

### Vector Operations Needed

```clojure
;; Load 16/32/64 bytes at once
(defun simd-load-256 (ptr: ptr) -> <32 x i8>
  ;; Maps to lIR vector load
  ...)

;; Broadcast single byte to all lanes
(defun simd-broadcast-8 (byte: i8) -> <32 x i8>
  ;; Using shufflevector with splat mask
  ...)

;; XOR vectors for equality detection
(defun simd-xor-256 (a: <32 x i8> b: <32 x i8>) -> <32 x i8>
  ;; Direct lIR xor on vectors
  ...)

;; Find first zero byte (match position)
(defun simd-find-zero-mask (v: <32 x i8>) -> i32
  ;; Convert to bitmask, count trailing zeros
  ...)
```

### Implementation Strategy

```clojure
(defun ascii-case-insensitive-find (haystack: ptr hlen: i64 
                                    needle: ptr nlen: i64) -> i64
  ;; 1. Case-fold needle to lowercase (once)
  ;; 2. For each 32-byte chunk of haystack:
  ;;    a. Load chunk
  ;;    b. Fold to lowercase in-register
  ;;    c. Compare against needle bytes using probe positions
  ;;    d. If probes match, verify full needle
  ;; 3. Return byte offset or -1
  ...)
```

## Phase 2: UTF-8 Aware Search

Extend to handle multi-byte UTF-8 sequences safely.

### Safe Window Selection

Not all needle positions are safe for SIMD scanning. Need to identify:
- Positions that are valid UTF-8 character boundaries
- Characters that don't expand/contract under case folding
- Sufficient byte diversity for good filtering

```clojure
(defstruct SafeWindow (start: i64 len: i64 script: i8))

(defun find-safe-window (needle: ptr nlen: i64) -> SafeWindow
  ;; Walk needle by UTF-8 characters
  ;; Score each position by:
  ;;   - Byte diversity (more unique bytes = better filter)
  ;;   - Folding safety (no expansions like ß→ss)
  ;;   - Script compatibility (ASCII vs Latin Extended vs Cyrillic)
  ...)
```

### Script-Specific Kernels

Following StringZilla, different scripts need different folding logic:

| Script | Lead Bytes | Folding Strategy |
|--------|------------|------------------|
| ASCII | 00-7F | Toggle bit 5 |
| Latin-1 Supplement | C2-C3 | Table lookup |
| Cyrillic | D0-D1 | Offset addition (+0x20, etc.) |
| Greek | CE-CF | Shuffle-based LUT |

```clojure
;; Cyrillic folding using byte offset
(defun fold-cyrillic-block (text: <64 x i8>) -> <64 x i8>
  ;; All Cyrillic uppercase lives in D0 80-D0 AF
  ;; Lowercase is +0x10, +0x20, or -0x20 offset
  (let ((high-nibble (simd-and (simd-srli text 4) 0x0F))
        (offset-lut (simd-make-lut ...)))  ;; 16-entry lookup
    (simd-add text (simd-shuffle offset-lut high-nibble))))
```

## Phase 3: Full Unicode Case Folding

Implement the full Unicode 17 case folding rules for locale-agnostic search.

### Challenges

1. **Expanding folds**: ß → ss, ﬃ → ffi
2. **Shrinking folds**: ſ (long s) → s
3. **Cross-script targets**: Kelvin sign K → k

### Solution: Alarm Masks

```clojure
;; Detect "dangerous" bytes that need slow-path verification
(defun compute-alarm-mask (chunk: <64 x i8>) -> i64
  ;; Check for:
  ;; - Ligature lead bytes (EF AC = ﬁﬂﬃ range)
  ;; - Eszett (C3 9F = ß, E1 BA 9E = ẞ)
  ;; - Long s (C5 BF = ſ)
  ;; - Kelvin sign (E2 84 AA = K)
  ...)
```

## Dependencies

- Builtins: `load-byte`, `store-byte`, `ptr+`
- lIR: Vector types, `extractelement`, `shufflevector`
- New: Vector load/store intrinsics (may need to add)

## Required lIR Extensions

The current lIR vector support may need extensions for optimal codegen:

```clojure
;; Horizontal operations
(llvm.vector.reduce.or <N x i8>)   ;; Fold OR across lanes
(llvm.cttz <N x i8>)               ;; Count trailing zeros

;; Comparison to mask
(icmp eq <32 x i8> a b)            ;; Returns <32 x i1>
(bitcast <32 x i1> to i32)         ;; Convert mask to scalar

;; Aligned/unaligned loads
(load <32 x i8> ptr align 1)       ;; Unaligned vector load
```

## Test Cases

### Phase 1 (ASCII)
- Find "hello" in "Hello World" → 0
- Find "WORLD" in "Hello World" → 6
- Find "xyz" in "Hello World" → -1
- Find needle longer than haystack → -1
- Find at end of buffer → correct offset
- Empty needle → 0

### Phase 2 (UTF-8)
- Find "große" in "Der GROSSE Hund" → match (ß↔SS)
- Find "STRASSE" in "Straße" → 0
- Find "café" in "CAFÉ" → 0
- Needle with mixed scripts → uses appropriate kernel

### Phase 3 (Full Unicode)
- Find "efficient" in "eﬃcient" → 0 (ligature)
- Find "ΜΙΚΡΟΣ" in "μικρος" → 0 (Greek)
- Find "kelvin" in "kelvin K" → handles Kelvin sign

## Performance Targets

| Operation | Target | Notes |
|-----------|--------|-------|
| ASCII search | >5 GB/s | AVX2/AVX-512 |
| Latin search | >3 GB/s | With case folding |
| Full Unicode | >1 GB/s | Script-specific paths |

## File Organization

```
lib/
├── liar.string.liar          # High-level API
├── liar.string.simd.liar     # SIMD kernels
└── liar.string.unicode.liar  # Case folding tables
```

## References

- [StringZilla UTF-8 Search](https://ashvardanian.com/posts/search-utf8/)
- [Unicode CaseFolding.txt](https://www.unicode.org/Public/UCD/latest/ucd/CaseFolding.txt)
- [LLVM Vector Operations](https://llvm.org/docs/LangRef.html#vector-operations)

## Ordering

Depends on: `cj5a8` (String Builder) for output formatting
Required by: None (pure optimization)
