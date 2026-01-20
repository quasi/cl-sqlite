# Contract: statement-column-count

**Signature:** `(statement-column-count stmt) => integer`
**Confidence:** 0.95
**Status:** Stable

---

## Parameters

### stmt
**Type:** `sqlite-statement`
**Required:** Yes
**Description:** Prepared statement to query

**Constraints:**
- Must be valid `sqlite-statement` instance
- Must be prepared (compiled from SQL)

---

## Returns

**Type:** `integer`
**Description:** Number of columns in result set

**Behavior:**
- Returns count of columns: 0, 1, 2, ..., N
- For SELECT queries: typically > 0
- For non-SELECT (INSERT, UPDATE, DELETE): 0
- Valid column indices: 0 to (count - 1)

**Examples:**
```common-lisp
(statement-column-count (prepare-statement db "SELECT id, name FROM users"))
;; => 2

(statement-column-count (prepare-statement db "INSERT INTO users VALUES (?, ?)"))
;; => 0

(statement-column-count (prepare-statement db "SELECT * FROM big_table"))
;; => 50 (if table has 50 columns)
```

---

## Side Effects

None (read-only query)

---

## Errors

None documented. Always succeeds.

---

## Preconditions

- Statement prepared and compiled

---

## Postconditions

**Success:**
- Returns column count
- Database state unchanged

---

## Implementation

**Location:** `sqlite.lisp:308-310`

**Key Code:**
```common-lisp
(defun statement-column-count (stmt)
  (sqlite-ffi:sqlite3-column-count (stmt stmt)))
```

**Direct FFI Call:** Thin wrapper over `sqlite3_column_count`

---

## Usage Examples

### Verify Expected Column Count
```common-lisp
(let ((stmt (prepare-statement db "SELECT id, name, age FROM users")))
  (assert (= 3 (statement-column-count stmt)))
  ...)
```

---

### Dynamic Column Access
```common-lisp
(let ((stmt (prepare-statement db "SELECT * FROM table")))
  (step-statement stmt)
  (let ((col-count (statement-column-count stmt)))
    (loop for i from 0 below col-count
          collect (statement-column-value stmt i))))
```

---

### Loop Over Columns
```common-lisp
(let ((stmt (prepare-statement db "SELECT * FROM users")))
  (when (step-statement stmt)
    (dotimes (i (statement-column-count stmt))
      (format t "Column ~D: ~A~%" i (statement-column-value stmt i)))))
```

---

### In execute-to-list
```common-lisp
;; Uses statement-column-count internally
(loop for i from 0 below (statement-column-count stmt)
      collect (statement-column-value stmt i))
```

---

## Related Contracts

- `prepare-statement` - Creates statement (column count available after prepare)
- `statement-column-value` - Accesses column value (need count for loop bounds)
- `execute-to-list` - Uses count internally

---

## Patterns

**Recommended:** Use high-level execute functions (count handled automatically)
```common-lisp
(execute-to-list db "SELECT * FROM users")
;; Column count determined automatically
```

**Manual column access:** Need to know count
```common-lisp
(let ((stmt (prepare-statement db "SELECT * FROM users")))
  (when (step-statement stmt)
    (loop for i from 0 below (statement-column-count stmt)
          do (process-column (statement-column-value stmt i))))
  (finalize-statement stmt))
```

---

## Performance Notes

- **Execution:** O(1) - Direct lookup
- **Timing:** Can call before stepping (schema available after prepare)
- **Caching:** Stored in `column-count` slot after prepare

---

## Column Index Space

**Column indices are 0-based (RULE-005):**
```common-lisp
(let ((col-count (statement-column-count stmt)))
  ;; Valid indices: 0 to (col-count - 1)
  (loop for i from 0 below col-count
        do (statement-column-value stmt i)))
```

**NOT 1-based:**
```common-lisp
;; This would miss first column and overflow
(loop for i from 1 to (statement-column-count stmt)
      do (statement-column-value stmt i))  ; WRONG!
```

---

## Edge Cases

### No Columns (Non-SELECT)
```common-lisp
(statement-column-count (prepare-statement db "INSERT INTO t VALUES (1)"))
;; => 0

(statement-column-count (prepare-statement db "UPDATE t SET x = 1"))
;; => 0

(statement-column-count (prepare-statement db "DELETE FROM t"))
;; => 0
```

---

### Single Column
```common-lisp
(statement-column-count (prepare-statement db "SELECT COUNT(*) FROM t"))
;; => 1

(statement-column-count (prepare-statement db "SELECT id FROM t"))
;; => 1
```

---

### Many Columns (Wide Table)
```common-lisp
(statement-column-count (prepare-statement db "SELECT * FROM wide_table"))
;; => 100 (or however many columns)
```

---

## Validation

**Tests:**
- Implicit in all tests that loop over columns
- `execute-to-list` relies on this for collecting rows

**Documentation:**
- README.md (implicit in examples)
- REFERENCE.md:128-132
- CL-SQLITE.agent.md:886

**Verified Properties:**
- Correct count for various query types
- Used correctly in column loops

---

**Document Status:** Canonical
**Last Verified:** 2026-01-20 (Canon Initiation Pass 2)
