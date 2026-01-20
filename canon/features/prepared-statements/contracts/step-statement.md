# Contract: step-statement

**Signature:** `(step-statement stmt) => boolean`
**Confidence:** 0.95
**Status:** Stable

---

## Parameters

### stmt
**Type:** `sqlite-statement`
**Required:** Yes
**Description:** Prepared statement to step

**Constraints:**
- Must be valid `sqlite-statement` instance
- Must be prepared (compiled from SQL)
- Must have parameters bound before stepping (for parameterized queries)

---

## Returns

**Type:** `boolean`
**Description:** `T` if row available, `NIL` if no more rows

**Semantics:**
- **`T` (true):** Current row available, can call `statement-column-value`
- **`NIL` (false):** No more rows, iteration complete

---

## Side Effects

- Executes query (if first step)
- Advances to next row
- Updates internal row state
- May lock database (for write queries)

---

## Errors

### sqlite-error
**When:** Query execution fails

**Common error codes:**
- `:CONSTRAINT` - Constraint violation (for INSERT/UPDATE/DELETE)
- `:BUSY` - Database locked
- `:READONLY` - Read-only database
- `:CANTOPEN` - Cannot open required file

**Context:**
- `db-handle` - Database handle
- `error-code` - SQLite error code
- `error-msg` - Human-readable message

---

## Preconditions

- Statement prepared and compiled
- Parameters bound (if query uses parameters)

---

## Postconditions

**Success:**
- Statement stepped once
- Returns `T` (row available) or `NIL` (no more rows)
- If `T` returned: current row columns accessible via `statement-column-value`

**Failure:**
- `sqlite-error` signaled
- Statement state undefined

---

## Implementation

**Location:** `sqlite.lisp:175-180`

**Key Code:**
```common-lisp
(defun step-statement (stmt)
  (let ((error-code (sqlite-ffi:sqlite3-step (stmt stmt))))
    (case error-code
      (:row t)     ; Row available
      (:done nil)  ; No more rows
      (t (sqlite-error error-code (db stmt)
                       "Could not step statement")))))
```

**Mechanism:**
- Calls `sqlite3_step` on prepared statement
- `:row` → returns `T` (data available)
- `:done` → returns `NIL` (iteration complete)
- Other codes → error

---

## State Machine

**Stepping progression:**

```
[Prepared] --step--> [Row Available] --step--> [Next Row] --step--> ... --step--> [Done]
                          ↑ (T)                     ↑ (T)                         ↓ (NIL)
                          |___________________________|
                                  (reset)
```

**Behavior:**
1. First `step-statement` executes query, returns first row (T) or no rows (NIL)
2. Each subsequent `step-statement` returns next row (T) or no more rows (NIL)
3. After `NIL` returned, stepping again returns `NIL` (no-op)
4. `reset-statement` returns to prepared state

---

## Usage Examples

### Loop Over Rows
```common-lisp
(let ((stmt (prepare-statement db "SELECT id, name FROM users")))
  (loop while (step-statement stmt)
        collect (loop for i from 0 below 2
                      collect (statement-column-value stmt i)))
  (finalize-statement stmt))
;; => ((1 "Alice") (2 "Bob") (3 "Charlie"))
```

---

### Single Row Check
```common-lisp
(let ((stmt (prepare-statement db "SELECT name FROM users WHERE id = ?" 42)))
  (bind-parameter stmt 1 42)
  (if (step-statement stmt)
      (format t "Found: ~A~%" (statement-column-value stmt 0))
      (format t "Not found~%"))
  (finalize-statement stmt))
```

---

### Batch Insert
```common-lisp
(let ((stmt (prepare-statement db "INSERT INTO logs VALUES (?, ?)")))
  (dotimes (i 100)
    (bind-parameter stmt 1 (+ 1000 i))
    (bind-parameter stmt 2 (format nil "Event ~D" i))
    (step-statement stmt)           ; Execute INSERT
    (reset-statement stmt))         ; Reset for next bind
  (finalize-statement stmt))
```

---

### Update with Verification
```common-lisp
(let ((stmt (prepare-statement db "UPDATE users SET active = 0 WHERE id = ?")))
  (bind-parameter stmt 1 42)
  (if (step-statement stmt)
      ;; UPDATE succeeded (but UPDATE always succeeds if no constraint violation)
      (format t "Updated~%")
      (format t "Update failed~%"))
  (finalize-statement stmt))
```

---

## Related Contracts

- `prepare-statement` - Prepares statement for stepping
- `bind-parameter` - Binds parameters before stepping
- `reset-statement` - Resets statement without re-preparing
- `statement-column-value` - Reads column from current row
- `statement-column-count` - Gets column count

---

## Patterns

**Recommended:** Use high-level execute functions
```common-lisp
(execute-to-list db "SELECT * FROM users")
;; Stepping handled automatically
```

**Manual stepping:** For complex iteration logic
```common-lisp
(let ((stmt (prepare-statement db "SELECT * FROM users WHERE active = ?")))
  (bind-parameter stmt 1 1)
  (unwind-protect
      (loop while (step-statement stmt)
            for count from 1
            do (process-user (statement-column-value stmt 0) count))
    (finalize-statement stmt)))
```

---

## Performance Notes

- **Query execution:** Happens on first `step-statement`
- **Incremental:** Each step fetches next row
- **Efficient:** Better than fetching all rows at once for large result sets (with iterate)

---

## SQLite Semantics

**SQLite step behavior:**
- `:row` (100) → row available
- `:done` (101) → no more rows
- `:busy` (5) → database locked (retry)
- Other codes → error

---

## Validation

**Tests:**
- `test-select` (sqlite-tests.lisp:50): SELECT with stepping
- `test-multi-insert` (sqlite-tests.lisp:40): INSERT with stepping and reset

**Documentation:**
- README.md:181-190
- REFERENCE.md:109-120
- CL-SQLITE.agent.md:885

**Verified Properties:**
- Rows stepped correctly
- Iteration completes naturally
- Return value (T/NIL) semantics

---

**Document Status:** Canonical
**Last Verified:** 2026-01-20 (Canon Initiation Pass 2)
