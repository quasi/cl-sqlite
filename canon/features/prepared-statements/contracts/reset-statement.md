# Contract: reset-statement

**Signature:** `(reset-statement stmt) => nil`
**Confidence:** 0.95
**Status:** Stable

---

## Parameters

### stmt
**Type:** `sqlite-statement`
**Required:** Yes
**Description:** Prepared statement to reset

**Constraints:**
- Must be valid `sqlite-statement` instance

---

## Returns

**Type:** `nil`
**Description:** No meaningful return value

---

## Behavior

**Reset Effect:**
- Clears query result set (previous rows no longer accessible)
- Resets internal query state
- Prepared statement ready for re-execution
- Parameter bindings preserved (not cleared)

**Contrast with finalize:**
- `finalize-statement` → releases resources, statement unusable
- `reset-statement` → resets state, statement reusable

**Contrast with clear-bindings:**
- `reset-statement` → clears results, keeps bindings
- `clear-bindings` → keeps results, clears bindings
- `reset-statement` + `clear-bindings` → both

---

## Side Effects

- Clears current row state
- Clears query results
- Preserves parameter bindings
- May release internal row buffers

---

## Errors

None documented. Always succeeds.

---

## Preconditions

- Statement must be valid `sqlite-statement` instance

---

## Postconditions

**Success:**
- Statement reset to prepared state
- Ready for re-stepping
- Returns `nil`

---

## Implementation

**Location:** `sqlite.lisp:312-315`

**Key Code:**
```common-lisp
(defun reset-statement (stmt)
  (sqlite-ffi:sqlite3-reset (stmt stmt)))
```

**Direct FFI Call:** Thin wrapper over `sqlite3_reset`

---

## Usage Examples

### Batch Operations with Reset
```common-lisp
(let ((stmt (prepare-statement db "INSERT INTO logs (ts, msg) VALUES (?, ?)")))
  (dotimes (i 100)
    (bind-parameter stmt 1 (+ 1000 i))
    (bind-parameter stmt 2 (format nil "Event ~D" i))
    (step-statement stmt)
    (reset-statement stmt))  ; Reset for next iteration
  (finalize-statement stmt))
;; Without reset: would keep accumulating results
```

---

### Re-Execute SELECT Query
```common-lisp
(let ((stmt (prepare-statement db "SELECT * FROM users WHERE age > ?")))
  ;; First execution
  (bind-parameter stmt 1 18)
  (loop while (step-statement stmt)
        collect (statement-column-value stmt 0))
  ;; Reset and re-execute with different parameter
  (reset-statement stmt)
  (clear-bindings stmt)
  (bind-parameter stmt 1 21)
  (loop while (step-statement stmt)
        collect (statement-column-value stmt 0))
  (finalize-statement stmt))
```

---

### Iterating Over Query Results Twice
```common-lisp
(let ((stmt (prepare-statement db "SELECT id FROM users")))
  ;; First iteration
  (loop while (step-statement stmt)
        do (format t "First: ~D~%" (statement-column-value stmt 0)))

  ;; Reset and iterate again
  (reset-statement stmt)
  (loop while (step-statement stmt)
        do (format t "Second: ~D~%" (statement-column-value stmt 0)))

  (finalize-statement stmt))
```

---

## Related Contracts

- `finalize-statement` - Release statement (vs. reset)
- `step-statement` - Execute after reset
- `clear-bindings` - Clear parameters (often used with reset)
- `bind-parameter` - Re-bind after reset (if needed)

---

## Patterns

**Recommended:** Use with loop for batch operations
```common-lisp
(let ((stmt (prepare-statement db "INSERT INTO t VALUES (?)")))
  (unwind-protect
      (dotimes (i 1000)
        (bind-parameter stmt 1 i)
        (step-statement stmt)
        (reset-statement stmt))
    (finalize-statement stmt)))
```

**Alternative:** Use high-level execute functions (automatic)
```common-lisp
(dotimes (i 1000)
  (execute-non-query db "INSERT INTO t VALUES (?)" i))
;; Reset handled internally
```

---

## Performance Notes

- **Reset overhead:** Minimal (clear internal state)
- **Reuse benefit:** Reset + re-bind faster than re-prepare
- **Memory:** May release internal buffers

**Comparison:**
```common-lisp
;; Inefficient - Reprepares each time
(dotimes (i 1000)
  (execute-non-query db "INSERT INTO t VALUES (?)" i))

;; Efficient - Prepares once, reuses with reset
(let ((stmt (prepare-statement db "INSERT INTO t VALUES (?)")))
  (unwind-protect
      (dotimes (i 1000)
        (bind-parameter stmt 1 i)
        (step-statement stmt)
        (reset-statement stmt))
    (finalize-statement stmt)))
```

---

## State Machine

**Statement lifecycle:**

```
[Prepared] --step--> [Rows Available]
    ↑                      |
    |--reset-----------<---|
    |
    +--clear-bindings (keeps results)
    |
    +--finalize (end of life)
```

**Flow:**
1. Prepare: Statement ready
2. Step: Row available
3. Reset: Back to prepared state (results cleared)
4. Step again: New query execution
5. Or: Finalize (release resources)

---

## Edge Cases

### Multiple Resets
```common-lisp
(let ((stmt (prepare-statement db "SELECT 1")))
  (step-statement stmt)
  (reset-statement stmt)
  (reset-statement stmt))  ; Second reset is no-op
;; No error
```

---

### Reset Without Step
```common-lisp
(let ((stmt (prepare-statement db "SELECT 1")))
  (reset-statement stmt)  ; No-op (no results to reset)
  (step-statement stmt))  ; Executes normally
;; Works fine
```

---

### Reset with Bindings
```common-lisp
(let ((stmt (prepare-statement db "INSERT INTO t VALUES (?)")))
  (bind-parameter stmt 1 42)
  (step-statement stmt)
  (reset-statement stmt)
  ;; Binding still present!
  (step-statement stmt))  ; Uses same binding (42) again
```

---

## Comparison: reset-statement vs. clear-bindings

| Function | Results | Bindings | Use Case |
|----------|---------|----------|----------|
| reset-statement | Clear | Keep | Iterate again with same params |
| clear-bindings | Keep | Clear | Change params, keep results |
| Both | Clear | Clear | Complete reset |

---

## Validation

**Tests:**
- `test-multi-insert` (sqlite-tests.lisp:40): Reset in loop

**Documentation:**
- README.md (implicit in batch examples)
- REFERENCE.md:133-138
- CL-SQLITE.agent.md:888

**Verified Properties:**
- Reset clears results
- Bindings preserved
- Allows re-execution

---

**Document Status:** Canonical
**Last Verified:** 2026-01-20 (Canon Initiation Pass 2)
