# StringMap: String-keyed Hash Map

Create a dedicated hash map implementation for string keys, providing the string key support that HashMap (integer-only HAMT) lacks.

## Background

The existing HashMap (`lib/liar.hashmap.liar`) only supports integer keys due to:
1. `hash-key` function: `(bit-xor key (shr key 16))` - integer arithmetic
2. Key comparison: done implicitly via hash/position equality
3. Return type: `hm-get` returns `i64` (0 for not found)

StringMap will use a simpler chained hash table (open chaining) which is easier to implement and debug for string keys.

## Design

### Data Structures

```lisp
(defstruct StringEntry (key: ptr hash: i64 value: ptr next: ptr))
(defstruct StringMap (cnt: i64 buckets: ptr nbuckets: i64))
```

- `StringEntry`: linked list node for chaining collisions
- `StringMap`: container with count, bucket array, and bucket count
- Fixed 32 buckets initially (can add resizing later)

### Hash Function

Use djb2 algorithm - simple, fast, good distribution for strings:

```lisp
(defun hash-string (s: ptr) -> i64
  (hash-string-loop s 0 5381))

(defun hash-string-loop (s: ptr i hash) -> i64
  (let ((c (load-byte (ptr+ s i))))
    (if (= c 0)
        hash
        (hash-string-loop s (+ i 1) (+ (* hash 33) c)))))
```

### String Comparison

Need `streq` for comparing string keys:

```lisp
(defun streq (a: ptr b: ptr) -> i64
  (streq-loop a b 0))

(defun streq-loop (a: ptr b: ptr i) -> i64
  (let ((ca (load-byte (ptr+ a i)))
        (cb (load-byte (ptr+ b i))))
    (if (= ca cb)
        (if (= ca 0) 1 (streq-loop a b (+ i 1)))
        0)))
```

## Public API

| Function | Signature | Description |
|----------|-----------|-------------|
| `string-map` | `() -> ptr` | Create empty string map |
| `sm-get` | `(m: ptr key: ptr) -> ptr` | Get value for key, nil if not found |
| `sm-assoc` | `(m: ptr key: ptr val: ptr) -> ptr` | Return new map with key→value |
| `sm-contains?` | `(m: ptr key: ptr) -> i64` | 1 if key exists, 0 otherwise |
| `sm-count` | `(m: ptr) -> i64` | Number of entries |
| `sm-empty?` | `(m: ptr) -> i64` | 1 if empty |

## Protocol Implementation

StringMap can correctly implement Lookup protocol since it returns `ptr` (nil for not found):

```lisp
(extend-protocol Lookup StringMap
  (get [self key] (sm-get self key)))

(extend-protocol Associative StringMap
  (assoc [self key val] (sm-assoc self key val))
  (dissoc [self key] self)  ;; TODO
  (contains-key? [self key] (= 1 (sm-contains? self key))))

(extend-protocol Countable StringMap
  (count [self] (sm-count self)))

(extend-protocol Emptyable StringMap
  (empty [self] (string-map)))
```

## Reader/Sugar Integration (Future)

Once StringMap exists, we can add reader macro support:
- `{1 "bar"}` → HashMap (integer keys detected)
- `{"foo" "bar"}` → StringMap (string keys detected)

This is out of scope for this moth - requires codegen changes.

## Implementation Tasks

1. Create `lib/liar.stringmap.liar`
2. Implement `hash-string` and `streq` helper functions
3. Implement `string-map` constructor
4. Implement `sm-get` with bucket lookup and chain traversal
5. Implement `sm-assoc` with path copying
6. Implement `sm-contains?`, `sm-count`, `sm-empty?`
7. Add protocol implementations (Lookup, Associative, Countable, Emptyable)
8. Add tests

## Dependencies

- `liar.core` (basic operations)
- `liar.seq` (protocols)

## Testing

```lisp
(let ((m (string-map)))
  (let ((m2 (sm-assoc m "hello" "world")))
    (let ((v (sm-get m2 "hello")))
      (streq v "world"))))  ;; => 1
```
