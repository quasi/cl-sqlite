# Contract: execute-single

**Signature:** `(execute-single db sql &rest parameters) => value`
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
- Must be SELECT query
- Should return exactly one row with one column

**Examples:**
```sql
"SELECT COUNT(*) FROM users"
"SELECT name FROM users WHERE id = ?"
"SELECT MAX(age) FROM users"
```

---

### parameters
**Type:** `&rest` (variable arguments)
**Required:** No
**Description:** Values to bind to SQL placeholders

**Constraints:**
- Same as `execute-non-query`

---

## Returns

**Type:** `T` (any type)
**Description:** Single value from first column of first row

**Type mapping:**
- INTEGER → Lisp integer
- REAL → Lisp float (double-float)
- TEXT → Lisp string
- BLOB → Lisp byte vector `(simple-array (unsigned-byte 8) (*))`
- NULL → `:null`

**Behavior:**
- If no rows returned: returns `:null`
- If multiple rows returned: returns first row's value, ignores rest
- If multiple columns returned: returns first column's value, ignores rest

---

## Side Effects

- Executes SQL query
- Reads database (no modifications)

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
- Returns single value
- Value type depends on column type

**Failure:**
- `sqlite-error` signaled
- Database unchanged

---

## Implementation

**Location:** `sqlite.lisp:197-211`

**Key Code:**
```common-lisp
(defun execute-single (db sql &rest parameters)
  (let ((stmt (prepare-statement db sql)))
    (loop for i from 1
          for param in parameters
          do (bind-parameter stmt i param))
    (unwind-protect
        (progn
          (if (step-statement stmt)
              (statement-column-value stmt 0)  ; First column (0-indexed)
              :null))  ; No rows
      (finalize-statement stmt))))
```

**Mechanism:**
1. Prepare statement
2. Bind parameters
3. Step once
4. If row available: return first column value
5. If no row: return `:null`
6. Finalize statement

---

## Usage Examples

### Count Rows
```common-lisp
(execute-single db "SELECT COUNT(*) FROM users")
;; => 42
```

---

### Lookup Single Value
```common-lisp
(execute-single db "SELECT name FROM users WHERE id = ?" 1)
;; => "Alice"
```

---

### Aggregate Function
```common-lisp
(execute-single db "SELECT MAX(age) FROM users")
;; => 65
```

---

### Check Existence
```common-lisp
(execute-single db "SELECT 1 FROM users WHERE name = ? LIMIT 1" "Alice")
;; => 1 (if exists) or :null (if not)
```

---

### No Rows
```common-lisp
(execute-single db "SELECT name FROM users WHERE id = ?" 999)
;; => :null
```

---

### Multiple Rows (First Only)
```common-lisp
(execute-single db "SELECT name FROM users")
;; => "Alice" (first row, ignores rest)
```

---

### Multiple Columns (First Only)
```common-lisp
(execute-single db "SELECT name, age FROM users WHERE id = ?" 1)
;; => "Alice" (first column, ignores age)
```

---

## Related Contracts

- `execute-non-query` - Execute non-query SQL
- `execute-to-list` - Return all rows
- `execute-one-row-m-v` - Return all columns from one row
- `statement-column-value` - Underlying column retrieval

---

## Patterns

**Recommended:** Use for scalar queries
```common-lisp
(let ((count (execute-single db "SELECT COUNT(*) FROM users")))
  (format t "Total users: ~D~%" count))
```

**Use case: Existence check**
```common-lisp
(defun user-exists-p (db name)
  (not (eq :null (execute-single db
                                 "SELECT 1 FROM users WHERE name = ? LIMIT 1"
                                 name))))
```

**Use case: Get max ID**
```common-lisp
(defun get-next-id (db)
  (let ((max-id (execute-single db "SELECT MAX(id) FROM users")))
    (if (eq max-id :null) 1 (1+ max-id))))
```

---

## Anti-patterns

❌ **Using for multi-column results:**
```common-lisp
;; WRONG - Ignores age column
(execute-single db "SELECT name, age FROM users WHERE id = ?" 1)

;; RIGHT - Use execute-one-row-m-v
(execute-one-row-m-v db "SELECT name, age FROM users WHERE id = ?" 1)
;; => "Alice", 30 (multiple values)
```

❌ **Using for multi-row results:**
```common-lisp
;; WRONG - Only returns first name
(execute-single db "SELECT name FROM users")

;; RIGHT - Use execute-to-list
(execute-to-list db "SELECT name FROM users")
;; => (("Alice") ("Bob") ("Charlie"))
```

---

## Performance Notes

- Efficient for scalar queries (count, max, min, etc.)
- Avoids allocating list for single value
- Statement caching applies (same as execute-non-query)

---

## Validation

**Tests:**
- `test-count` (sqlite-tests.lisp:46): Counting rows
- Various tests use execute-single for assertions

**Documentation:**
- README.md:127-135
- REFERENCE.md:57-63
- CL-SQLITE.agent.md:879

**Verified Properties:**
- Returns `:null` for empty results
- Returns first column only (RULE-005: 0-indexed)
- Type conversion consistency

---

**Document Status:** Canonical
**Last Verified:** 2026-01-20 (Canon Initiation Pass 2)
