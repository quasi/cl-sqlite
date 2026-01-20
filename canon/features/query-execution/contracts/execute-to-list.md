# Contract: execute-to-list

**Signature:** `(execute-to-list db sql &rest parameters) => list-of-rows`
**Confidence:** 0.95
**Status:** Stable

---

## Parameters

### db
**Type:** `sqlite-handle`
**Required:** Yes
**Description:** Connected database handle

---

### sql
**Type:** `string`
**Required:** Yes
**Description:** SQL SELECT statement

**Constraints:**
- Must contain exactly one SQL statement (RULE-001)
- Typically SELECT query
- May return zero, one, or many rows
- May return one or many columns

**Examples:**
```sql
"SELECT * FROM users"
"SELECT name, age FROM users WHERE age > ?"
"SELECT id FROM users ORDER BY name"
```

---

### parameters
**Type:** `&rest` (variable arguments)
**Required:** No
**Description:** Values to bind to SQL placeholders

---

## Returns

**Type:** `list`
**Description:** List of rows, where each row is a list of column values

**Structure:**
```common-lisp
(
  (col1-row1 col2-row1 ... colN-row1)
  (col1-row2 col2-row2 ... colN-row2)
  ...
  (col1-rowM col2-rowM ... colN-rowM)
)
```

**Empty result:** Returns `nil` (empty list)

**Single row:** Returns `((col1 col2 ...))` (list with one row)

**Single column:** Returns `((val1) (val2) ...)` (list of single-element lists)

**Type mapping:** Same as `execute-single`
- INTEGER → integer
- REAL → double-float
- TEXT → string
- BLOB → byte vector
- NULL → `:null`

---

## Side Effects

- Executes SQL query
- Reads entire result set into memory
- Finalizes statement after reading all rows

---

## Errors

### sqlite-error
**When:** SQL execution fails

**Common error codes:**
- `:SQL` - SQL syntax error
- `:BUSY` - Database locked

---

## Preconditions

- Database connected
- SQL is valid SELECT statement
- Parameter count matches placeholders

---

## Postconditions

**Success:**
- Returns list of all rows
- Empty list if no rows
- Statement finalized

**Failure:**
- `sqlite-error` signaled
- Database unchanged

---

## Implementation

**Location:** `sqlite.lisp:213-231`

**Key Code:**
```common-lisp
(defun execute-to-list (db sql &rest parameters)
  (let ((stmt (prepare-statement db sql)))
    (loop for i from 1
          for param in parameters
          do (bind-parameter stmt i param))
    (unwind-protect
        (loop while (step-statement stmt)
              collect (loop for i from 0 below (statement-column-count stmt)
                            collect (statement-column-value stmt i)))
      (finalize-statement stmt))))
```

**Mechanism:**
1. Prepare statement
2. Bind parameters
3. Loop: step and collect rows
   - For each row: collect all columns (0-indexed)
4. Finalize statement
5. Return accumulated list

---

## Usage Examples

### All Rows, All Columns
```common-lisp
(execute-to-list db "SELECT * FROM users")
;; => ((1 "Alice" 30) (2 "Bob" 25) (3 "Charlie" 35))
```

---

### Filtered Query
```common-lisp
(execute-to-list db "SELECT name, age FROM users WHERE age > ?" 28)
;; => (("Alice" 30) ("Charlie" 35))
```

---

### Empty Result
```common-lisp
(execute-to-list db "SELECT * FROM users WHERE id = ?" 999)
;; => nil
```

---

### Single Row
```common-lisp
(execute-to-list db "SELECT * FROM users WHERE id = ?" 1)
;; => ((1 "Alice" 30))
```

---

### Single Column
```common-lisp
(execute-to-list db "SELECT name FROM users ORDER BY name")
;; => (("Alice") ("Bob") ("Charlie"))
```

---

### With NULL Values
```common-lisp
(execute-to-list db "SELECT name, age FROM users")
;; => (("Alice" 30) ("Bob" :null) ("Charlie" 35))
```

---

### Processing Results
```common-lisp
(dolist (row (execute-to-list db "SELECT name, age FROM users"))
  (destructuring-bind (name age) row
    (format t "~A is ~A years old~%" name age)))
```

---

### Mapping Over Results
```common-lisp
(mapcar #'first
        (execute-to-list db "SELECT name FROM users ORDER BY age DESC"))
;; => ("Charlie" "Alice" "Bob")
```

---

## Related Contracts

- `execute-single` - Return single value
- `execute-one-row-m-v` - Return one row as multiple values
- `execute-non-query` - Execute non-query SQL
- Iterate integration - Stream results without materializing full list

---

## Patterns

**Recommended:** Use for queries with moderate result sets
```common-lisp
(let ((users (execute-to-list db "SELECT * FROM users")))
  (dolist (user users)
    (process-user user)))
```

**Pattern: Extract column**
```common-lisp
(defun get-all-names (db)
  (mapcar #'first (execute-to-list db "SELECT name FROM users")))
```

**Pattern: Count results**
```common-lisp
(length (execute-to-list db "SELECT * FROM users WHERE active = 1"))
;; Better: Use execute-single with COUNT(*)
(execute-single db "SELECT COUNT(*) FROM users WHERE active = 1")
```

---

## Anti-patterns

❌ **Large result sets (memory issue):**
```common-lisp
;; WRONG - Loads 1M rows into memory
(execute-to-list db "SELECT * FROM logs")  ; 1 million rows

;; RIGHT - Use iterate for streaming
(iter (for (timestamp message) in-sqlite-query db "SELECT * FROM logs")
      (process-log timestamp message))
```

❌ **Counting via length:**
```common-lisp
;; WRONG - Fetches all rows just to count
(length (execute-to-list db "SELECT * FROM users"))

;; RIGHT - Use SQL COUNT
(execute-single db "SELECT COUNT(*) FROM users")
```

❌ **Nested column access:**
```common-lisp
;; AWKWARD
(let ((first-name (first (first (execute-to-list db "SELECT name FROM users LIMIT 1")))))
  ...)

;; BETTER - Use execute-single
(execute-single db "SELECT name FROM users LIMIT 1")
```

---

## Performance Notes

**Memory usage:**
- Materializes entire result set in memory
- Each row allocated as list
- O(n) memory where n = number of rows

**When to use:**
- Result set fits comfortably in memory
- Need to process all rows multiple times
- Random access to results needed

**When NOT to use:**
- Large result sets (>10,000 rows)
- Streaming processing (use iterate)
- Only need first few rows (use LIMIT + execute-to-list, or execute-single)

**Statement caching:** Applies (same SQL string reuses prepared statement)

---

## Comparison with Alternatives

| Use Case | Function | Result |
|----------|----------|--------|
| Single scalar value | `execute-single` | value |
| One row, multiple columns | `execute-one-row-m-v` | (values col1 col2 ...) |
| All rows, all columns | `execute-to-list` | list of lists |
| Large result set | `iter (... in-sqlite-query ...)` | streaming |

---

## Validation

**Tests:**
- `test-select` (sqlite-tests.lisp:50): Basic SELECT
- `test-multi-row-query` (sqlite-tests.lisp:55): Multiple rows
- Many tests use execute-to-list for verification

**Documentation:**
- README.md:136-145
- REFERENCE.md:64-71
- CL-SQLITE.agent.md:880

**Verified Properties:**
- Returns empty list for no rows
- Each row is list of columns
- Columns 0-indexed (RULE-005)
- Type conversion consistent

---

**Document Status:** Canonical
**Last Verified:** 2026-01-20 (Canon Initiation Pass 2)
