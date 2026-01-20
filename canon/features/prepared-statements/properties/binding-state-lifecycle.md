# Property: Parameter Binding State Lifecycle

**ID:** INV-002
**Confidence:** 0.95
**Status:** Verified
**Type:** Invariant

---

## Statement

```
Parameter binding state persists across step-statement calls
until explicitly cleared via reset-statement or clear-bindings.
```

---

## Lifecycle

**State progression:**

```
[Prepared]
    ↓
[Bind Param 1] --step--> [Executed with Param 1]
    ↓                            ↓
[Bind Param 2]            [Reset (bindings stay)]
    ↓                            ↓
[Ready to Step]           [Step again with Param 1]
    |
    +-- clear-bindings --> [All params NULL]
    |
    +-- reset-statement --> [Results cleared, bindings stay]
```

---

## Key Properties

### Bindings Persist Through Step

**First execution:**
```common-lisp
(bind-parameter stmt 1 42)
(step-statement stmt)          ; Uses binding (42)
```

**After step, binding still present:**
```common-lisp
(reset-statement stmt)
(step-statement stmt)          ; Still uses binding (42)
```

---

### Reset vs. Clear-Bindings

**reset-statement:** Clears results, keeps bindings
```common-lisp
(bind-parameter stmt 1 42)
(step-statement stmt)
(reset-statement stmt)
(step-statement stmt)          ; Uses same binding (42)
```

**clear-bindings:** Clears bindings, keeps results
```common-lisp
(bind-parameter stmt 1 42)
(step-statement stmt)
(clear-bindings stmt)
(step-statement stmt)          ; ERROR: Param 1 now NULL
```

---

### Multiple Reuse

**Same binding, multiple executions:**
```common-lisp
(let ((stmt (prepare-statement db "INSERT INTO logs VALUES (?)")))
  (bind-parameter stmt 1 "Event")
  (dotimes (i 100)
    (step-statement stmt)      ; Uses "Event" each time
    (reset-statement stmt)))
;; All 100 inserts use same "Event" value
```

---

## Implications

### Performance Optimization

**Reuse binding across operations:**
```common-lisp
;; Efficient: Bind once
(bind-parameter stmt 1 1000)
(dotimes (i 100)
  (step-statement stmt)
  (reset-statement stmt))

;; Inefficient: Rebind each time
(dotimes (i 100)
  (clear-bindings stmt)
  (bind-parameter stmt 1 1000)
  (step-statement stmt)
  (reset-statement stmt))
```

---

### State Tracking

**Users must track binding state:**
```common-lisp
(let ((stmt (prepare-statement db "INSERT INTO t VALUES (?), (?)")))
  (bind-parameter stmt 1 "Alice")
  (bind-parameter stmt 2 "Bob")
  (step-statement stmt)
  ;; Bindings still present
  (reset-statement stmt)
  ;; Can step again with same bindings
  (step-statement stmt))
```

---

## Enforcement

**Implementation:** Binding stored in statement foreign object
- Persists until explicitly cleared
- Not affected by stepping
- Not affected by reset

---

## Related Properties

- **reset-statement behavior:** Bindings preserved
- **clear-bindings behavior:** Bindings cleared

---

**Document Status:** Canonical
**Last Verified:** 2026-01-20 (Canon Initiation Pass 4)
