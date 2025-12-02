# ADR 014: async/await Requires plet

## Status

Accepted

## Context

We want non-blocking I/O with async/await syntax. But async functions have a threading implication: the continuation after an `await` may resume on a different thread.

```lisp
(let ((cache []))
  (afn ()
    (let ((data (await (fetch-data))))
      (append cache data)        ; cache might be accessed from wrong thread
      (sum cache))))
```

If the event loop is multi-threaded, this is a data race.

## Decision

**Any code containing `await` must use `plet` for mutable captures.**

```lisp
; WRONG - let with await
(let ((cache []))
  (afn ()
    (await something)       ; ERROR: await in let-closure
    (append cache x)))

; RIGHT - plet with await  
(plet ((cache (atom [])))
  (afn ()
    (await something)       ; OK: plet-closure
    (swap! cache (fn (c) (append c x)))))
```

**Constants are still allowed without atom:**

```lisp
(plet ((url "https://api.example.com")    ; immutable — fine
       (cache (atom [])))                  ; mutable — needs atom
  (afn ()
    (let ((data (await (fetch url))))
      (swap! cache (fn (c) (append c data))))))
```

**The rule:** Same as pmap — if work can happen on another thread, captures must be thread-safe.

## Consequences

### Positive

- **Consistent model**: Same rules as pmap/pfilter
- **Compile-time safety**: Can't accidentally create async data races
- **Reuses closure color**: `plet`-closures already work, just extend to async

### Negative

- **More friction for async**: Must use atoms even for single-producer state
- **Learning curve**: Must understand threading implications of async

### Neutral

- Multi-threaded event loop is a choice — could also do single-threaded like Node.js
- But multi-threaded utilizes multiple cores better
- Atoms have low overhead for uncontended access

## Example: Async Accumulator

```lisp
(defun async-accumulate ()
  (plet ((cache (atom [])))
    (afn (url)
      (let ((data (await (fetch url))))
        (swap! cache (fn (c) (append c data)))
        (sum @cache)))))

(let ((acc (async-accumulate)))
  (await (acc "http://a.com"))    ; fetches complete in parallel
  (await (acc "http://b.com"))    ; but cache updates are atomic
  (await (acc "http://c.com")))
```
