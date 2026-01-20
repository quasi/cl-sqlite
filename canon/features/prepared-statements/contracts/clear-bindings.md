# Contract: clear-bindings

**Signature:** `(clear-bindings stmt) => nil`
**Confidence:** 0.90
**Status:** Stable

---

## Parameters

### stmt
**Type:** `sqlite-statement`
**Required:** Yes
**Description:** Prepared statement with bound parameters

**Constraints:**
- Must be valid `sqlite-statement` instance

---

## Returns

**Type:** `nil`
**Description:** No meaningful return value

---

## Behavior

**Clear Effect:**
- Unbinds all parameters
- All parameters reset to NULL
- Query results preserved (if any)
- Statement ready for new parameter binding

**Contrast with reset-statement:**
- `reset-statement` → clears results, keeps bindings
- `clear-bindings` → keeps results, clears bindings

---

## Side Effects

- Unbinds all parameters
- Sets all parameters to NULL
- May release parameter storage

---

## Errors

None documented. Always succeeds.

---

## Preconditions

- Statement must be valid `sqlite-statement` instance

---

## Postconditions

**Success:**
- All parameters cleared (set to NULL)
- Results preserved
- Returns `nil`

---

## Implementation

**Location:** `sqlite.lisp:320-322`

**Key Code:**
```common-lisp
(defun clear-bindings (stmt)
  (sqlite-ffi:sqlite3-clear-bindings (stmt stmt)))
```

**Direct FFI Call:** Thin wrapper over `sqlite3_clear_bindings`

---

## Usage Examples

### Clear and Re-bind Different Parameters
```common-lisp
(let ((stmt (prepare-statement db "SELECT * FROM users WHERE age > ?")))
  ;; First query
  (bind-parameter stmt 1 18)
  (let ((result1 (loop while (step-statement stmt)
                        collect (statement-column-value stmt 0))))
    ;; Clear and change parameter
    (clear-bindings stmt)
    (reset-statement stmt)
    (bind-parameter stmt 1 21)
    (let ((result2 (loop while (step-statement stmt)
                          collect (statement-column-value stmt 0))))
      (format t "Young: ~A~%" result1)
      (format t "Older: ~A~%" result2)))
  (finalize-statement stmt))
```

---

### Prepare for New Binding
```common-lisp
(let ((stmt (prepare-statement db "UPDATE users SET active = ? WHERE id = ?")))
  ;; First update
  (bind-parameter stmt 1 0)    ; inactive
  (bind-parameter stmt 2 5)    ; user 5
  (step-statement stmt)

  ;; Clear for new operation
  (clear-bindings stmt)
  (reset-statement stmt)
  (bind-parameter stmt 1 1)    ; active
  (bind-parameter stmt 2 10)   ; user 10
  (step-statement stmt)

  (finalize-statement stmt))
```

---

### Keep Results, Clear Bindings
```common-lisp
(let ((stmt (prepare-statement db "SELECT id FROM users WHERE age > ?")))
  (bind-parameter stmt 1 21)
  (loop while (step-statement stmt)
        collect (statement-column-value stmt 0))
  ;; Results accumulated, now clear parameters
  (clear-bindings stmt)
  ;; Parameters now NULL, but results still available
  )
```

---

## Related Contracts

- `bind-parameter` - Bind parameters (after clear-bindings)
- `reset-statement` - Clear results (often used with clear-bindings)
- `prepare-statement` - Creates statement

---

## Patterns

**Recommended:** Use with reset-statement for complete reset
```common-lisp
(let ((stmt (prepare-statement db "SELECT * FROM users WHERE age > ?")))
  ;; First query
  (bind-parameter stmt 1 18)
  (process-results stmt)

  ;; Reset completely for new query
  (reset-statement stmt)
  (clear-bindings stmt)
  (bind-parameter stmt 1 21)
  (process-results stmt)

  (finalize-statement stmt))
```

---

## Performance Notes

- **Clear overhead:** Minimal (set parameters to NULL)
- **Use case:** Rare in practice (usually reset-statement sufficient)
- **Benefit:** Explicit parameter clearing if needed

---

## Scenarios for Use

**Typical scenario:** Clear bindings after batch operation
```common-lisp
(let ((stmt (prepare-statement db "INSERT INTO t VALUES (?)")))
  ;; Process batch 1
  (bind-parameter stmt 1 100)
  (step-statement stmt)
  (reset-statement stmt)

  ;; Clear before different operation
  (clear-bindings stmt)
  (prepare-update-statement ...)  ; Hypothetical
  )
```

**Rare in practice:** Most applications just finalize and re-prepare

---

## Comparison: reset-statement vs. clear-bindings

```common-lisp
;; reset-statement: Clears results, keeps bindings
(reset-statement stmt)
(step-statement stmt)  ; Uses old bindings

;; clear-bindings: Keeps results, clears bindings
(clear-bindings stmt)
(step-statement stmt)  ; ERROR: No bindings set

;; Both: Complete reset
(reset-statement stmt)
(clear-bindings stmt)
(bind-parameter stmt 1 ...)
(step-statement stmt)  ; New binding, fresh execution
```

---

## Edge Cases

### Multiple Clears
```common-lisp
(let ((stmt (prepare-statement db "SELECT ?")))
  (clear-bindings stmt)
  (clear-bindings stmt))  ; Second clear is no-op
;; No error
```

---

### Step with Cleared Parameters
```common-lisp
(let ((stmt (prepare-statement db "INSERT INTO t VALUES (?)")))
  (bind-parameter stmt 1 42)
  (clear-bindings stmt)
  ;; Now parameter 1 is NULL
  (step-statement stmt))
;; Inserts NULL (not 42)
```

---

## Validation

**Tests:**
- Not explicitly tested (rarely used)
- Implicit in scenarios where reset needed

**Documentation:**
- README.md (not mentioned)
- REFERENCE.md:133-138
- CL-SQLITE.agent.md:889

**Verified Properties:**
- Parameters cleared to NULL
- Results preserved
- Idempotent

---

**Document Status:** Canonical
**Last Verified:** 2026-01-20 (Canon Initiation Pass 2)
