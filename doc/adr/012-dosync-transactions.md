# ADR 012: dosync for Coordinated Updates

## Status

Accepted

## Context

Atoms (ADR-011) are great for independent mutable values, but sometimes multiple values must be updated together atomically:

```lisp
(plet ((balance-a (atom 100))
       (balance-b (atom 100)))
  (fn (amount)
    (swap! balance-a (fn (a) (- a amount)))
    ; <- another thread could read here, see inconsistent state
    (swap! balance-b (fn (b) (+ b amount)))))
```

A bank transfer needs both updates to be visible atomically.

## Decision

Use **`dosync`** for coordinated updates to multiple atoms.

```lisp
(plet ((balance-a (atom 100))
       (balance-b (atom 100)))
  (fn (amount)
    (dosync
      (swap! balance-a (fn (a) (- a amount)))
      (swap! balance-b (fn (b) (+ b amount))))))
```

All updates inside `dosync` are:
- **Atomic**: Other threads see all or none
- **Consistent**: Intermediate states are not visible
- **Isolated**: Concurrent transactions don't interfere

## Consequences

### Positive

- **Composable transactions**: Multiple updates appear atomic
- **No explicit locks**: Can't deadlock
- **Familiar model**: Same as Clojure's refs/dosync

### Negative

- **Performance overhead**: Transactions have bookkeeping cost
- **Retry on conflict**: Conflicting transactions retry
- **Not always needed**: Single-atom updates don't need dosync

### Neutral

- **Programmer choice**: You can skip `dosync` if inconsistency is acceptable for your use case
- **TOCTOU still applies within swap!**: Check-and-update logic belongs inside the swap! function, not outside

```lisp
; WRONG - race condition even with dosync
(dosync
  (if (> @balance-a amount)           ; check
      (swap! balance-a (fn (a) (- a amount)))))  ; use — balance may have changed

; RIGHT - check inside swap
(dosync
  (swap! balance-a (fn (a) (if (> a amount) (- a amount) a))))
```
