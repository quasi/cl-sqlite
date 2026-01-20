# Property: Step State Machine

**ID:** EXEC-002
**Confidence:** 0.95
**Status:** Verified
**Type:** Behavioral Property

---

## Statement

```
Statement stepping follows deterministic state machine:
[Prepared] --step--> [Row Available] --step--> [Next Row] ... --step--> [Done]
```

---

## States

### 1. Prepared
- Statement compiled, no rows fetched
- Parameters may be bound
- Calling `step-statement` transitions to row state

### 2. Row Available (T)
- `step-statement` returned `T`
- Current row accessible via `statement-column-value`
- Calling `step-statement` fetches next row

### 3. Done (NIL)
- `step-statement` returned `NIL`
- No more rows available
- Calling `step-statement` again returns `NIL` (idempotent)

---

## Transitions

**Normal progression:**
```
[Prepared]
    ↓ step (T returned)
[Row 1 Available]
    ↓ step (T returned)
[Row 2 Available]
    ↓ step (NIL returned)
[Done]
    ↓ step (NIL returned)
[Done]  -- idempotent
```

---

## Examples

### SELECT with Multiple Rows
```common-lisp
(let ((stmt (prepare-statement db "SELECT id FROM users")))
  ;; State: [Prepared]

  (step-statement stmt)        ; => T, State: [Row 1]
  (statement-column-value stmt 0)  ; => 1

  (step-statement stmt)        ; => T, State: [Row 2]
  (statement-column-value stmt 0)  ; => 2

  (step-statement stmt)        ; => NIL, State: [Done]
  (step-statement stmt)        ; => NIL, State: [Done] (idempotent)

  (finalize-statement stmt))
```

---

### Empty Result Set
```common-lisp
(let ((stmt (prepare-statement db "SELECT * FROM users WHERE id = ?" 999)))
  (bind-parameter stmt 1 999)
  ;; State: [Prepared]

  (step-statement stmt)        ; => NIL (no rows), State: [Done]
  (step-statement stmt)        ; => NIL (idempotent)

  (finalize-statement stmt))
```

---

## Reset Interaction

**reset-statement returns to [Prepared]:**
```common-lisp
(let ((stmt (prepare-statement db "SELECT id FROM users")))
  (step-statement stmt)        ; => T, State: [Row 1]
  (reset-statement stmt)       ; State: [Prepared]
  (step-statement stmt)        ; => T (row 1 again), State: [Row 1]

  (finalize-statement stmt))
```

---

## Usage Patterns

### Loop Until Done
```common-lisp
(loop while (step-statement stmt)
      collect (statement-column-value stmt 0))
```

### Manual Stepping
```common-lisp
(when (step-statement stmt)
  (format t "First: ~A~%" (statement-column-value stmt 0))
  (when (step-statement stmt)
    (format t "Second: ~A~%" (statement-column-value stmt 0))))
```

---

## Invariants

- **Idempotency:** After [Done], stepping returns `NIL` forever
- **Determinism:** Same sequence of parameters → same sequence of results
- **Atomicity:** Each step is atomic (all-or-nothing)

---

## Related Properties

- **reset-statement:** Returns to [Prepared] state

---

**Document Status:** Canonical
**Last Verified:** 2026-01-20 (Canon Initiation Pass 4)
