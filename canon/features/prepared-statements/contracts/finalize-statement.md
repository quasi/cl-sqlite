# Contract: finalize-statement

**Signature:** `(finalize-statement stmt) => nil`
**Confidence:** 0.95
**Status:** Stable

---

## Parameters

### stmt
**Type:** `sqlite-statement`
**Required:** Yes
**Description:** Prepared statement to finalize

**Constraints:**
- Must be valid `sqlite-statement` instance
- May be already finalized (idempotent)

---

## Returns

**Type:** `nil`
**Description:** No meaningful return value

**Side Effects:**
- Releases SQLite statement resources (C level)
- Frees foreign pointer
- Marks statement as finalized

---

## Behavior

**Finalization:**
- Calls `sqlite3_finalize` on foreign pointer
- Releases internal query plan
- Releases any partial query results
- Invalidates statement handle

**Idempotent:**
- Can be called multiple times safely
- Second call is no-op (handle already freed)

---

## Side Effects

- Releases C-level `sqlite3_stmt*` resource
- May free internal memory used by query plan
- Statement becomes unusable

---

## Errors

### sqlite-error
**When:** Finalization fails

**Error Code:** `:BUSY` (rare)
**Scenario:** Statement still has active locks
**Mitigation:** Uncommon in practice

---

## Preconditions

- Statement must be valid `sqlite-statement` instance (may be finalized)

---

## Postconditions

**Success:**
- Statement finalized
- Foreign resources released
- Returns `nil`

**Failure:**
- `sqlite-error` signaled (rare)

---

## Implementation

**Location:** `sqlite.lisp:161-170`

**Key Code:**
```common-lisp
(defun finalize-statement (stmt)
  (when (slot-boundp stmt 'stmt)
    (let ((error-code (sqlite-ffi:sqlite3-finalize (stmt stmt))))
      (unless (eq error-code :ok)
        (sqlite-error error-code (db stmt) "Could not finalize statement")))
    (slot-makunbound stmt 'stmt)))
```

**Mechanism:**
1. Check if statement is already finalized (slot-boundp)
2. Call `sqlite3_finalize` on foreign pointer
3. Check for errors
4. Unbind `stmt` slot (mark as finalized)

---

## Usage Examples

### Manual Finalization
```common-lisp
(let ((stmt (prepare-statement db "SELECT * FROM users WHERE id = ?")))
  (bind-parameter stmt 1 42)
  (when (step-statement stmt)
    (format t "Found user at index 0: ~A~%" (statement-column-value stmt 0)))
  (finalize-statement stmt))
```

### Via execute Functions (Automatic)
```common-lisp
;; finalize-statement called automatically via unwind-protect
(execute-single db "SELECT * FROM users WHERE id = ?" 42)
```

### Explicit cleanup in unwind-protect
```common-lisp
(let ((stmt (prepare-statement db "SELECT * FROM users")))
  (unwind-protect
      (loop while (step-statement stmt)
            collect (statement-column-value stmt 0))
    (finalize-statement stmt)))
```

---

## Related Contracts

- `prepare-statement` - Creates statement
- `reset-statement` - Resets for re-execution (without finalization)
- `step-statement` - Executes statement

---

## Patterns

**Recommended:** Use high-level execute functions (automatic cleanup)
```common-lisp
(execute-to-list db "SELECT * FROM users")
;; Finalization automatic
```

**Manual finalization:** For explicit control
```common-lisp
(let ((stmt (prepare-statement db "INSERT INTO logs VALUES (?, ?)")))
  (unwind-protect
      (dotimes (i 100)
        (bind-parameter stmt 1 (+ 1000 i))
        (bind-parameter stmt 2 (format nil "Event ~D" i))
        (step-statement stmt)
        (reset-statement stmt))
    (finalize-statement stmt)))
```

---

## Performance Notes

- **Resource cleanup:** Essential for long-running operations
- **Cache interaction:** Evicted statements are finalized via destructor
- **Memory:** Finalized statements release internal query plan memory

---

## Edge Cases

### Multiple Finalizations

```common-lisp
(let ((stmt (prepare-statement db "SELECT 1")))
  (finalize-statement stmt)
  (finalize-statement stmt))  ; Second call is no-op
;; No error
```

---

### Finalization Failure

```common-lisp
;; Hypothetical (rare in practice)
(finalize-statement stmt)
;; => sqlite-error :BUSY (statement still has active locks)
```

---

## Validation

**Tests:**
- `test-statement-finalization` (sqlite-tests.lisp:20): Basic finalization
- `test-disconnect-with-statements` (sqlite-tests.lisp:17): Finalization during disconnect

**Documentation:**
- README.md:175-180
- REFERENCE.md:101-108
- CL-SQLITE.agent.md:884

**Verified Properties:**
- Statements finalized properly
- Resources released
- Idempotent behavior

---

**Document Status:** Canonical
**Last Verified:** 2026-01-20 (Canon Initiation Pass 2)
