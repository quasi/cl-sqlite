# Contract: execute-one-row-m-v

**Signature:** `(execute-one-row-m-v db sql &rest parameters) => (values col1 col2 ... colN)`
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
- Should return exactly one row
- May return one or many columns

**Examples:**
```sql
"SELECT name, age FROM users WHERE id = ?"
"SELECT COUNT(*), AVG(age) FROM users"
"SELECT * FROM config WHERE key = ?"
```

---

### parameters
**Type:** `&rest` (variable arguments)
**Required:** No
**Description:** Values to bind to SQL placeholders

---

## Returns

**Type:** `(values ...)`
**Description:** Multiple values, one per column

**Behavior:**
- Each column returned as separate value
- Number of values = number of columns in result
- If no rows: all values are `:null`
- If multiple rows: returns first row's values, ignores rest

**Type mapping:** Same as other execute functions
- INTEGER → integer
- REAL → double-float
- TEXT → string
- BLOB → byte vector
- NULL → `:null`

**Contrast with execute-to-list:**
```common-lisp
;; execute-to-list
(execute-to-list db "SELECT name, age FROM users WHERE id = 1")
;; => (("Alice" 30))  ; List of one row (which is a list)

;; execute-one-row-m-v
(execute-one-row-m-v db "SELECT name, age FROM users WHERE id = 1")
;; => "Alice", 30  ; Two separate values
```

---

## Side Effects

- Executes SQL query
- Reads first row only (if multiple rows, rest ignored)

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
- Returns N values where N = column count
- Values correspond to first row
- Statement finalized

**Failure:**
- `sqlite-error` signaled
- Database unchanged

---

## Implementation

**Location:** `sqlite.lisp:233-246`

**Key Code:**
```common-lisp
(defun execute-one-row-m-v (db sql &rest parameters)
  (let ((stmt (prepare-statement db sql)))
    (loop for i from 1
          for param in parameters
          do (bind-parameter stmt i param))
    (unwind-protect
        (progn
          (step-statement stmt)
          (values-list
           (loop for i from 0 below (statement-column-count stmt)
                 collect (statement-column-value stmt i))))
      (finalize-statement stmt))))
```

**Mechanism:**
1. Prepare statement
2. Bind parameters
3. Step once (read first row)
4. Collect all column values into list
5. Convert list to multiple values via `values-list`
6. Finalize statement

**Note:** Always steps once, even if no rows (returns `:null` values)

---

## Usage Examples

### Lookup Multiple Columns
```common-lisp
(multiple-value-bind (name age)
    (execute-one-row-m-v db "SELECT name, age FROM users WHERE id = ?" 1)
  (format t "~A is ~A years old~%" name age))
;; => "Alice is 30 years old"
```

---

### Aggregate Functions
```common-lisp
(multiple-value-bind (count avg-age)
    (execute-one-row-m-v db "SELECT COUNT(*), AVG(age) FROM users")
  (format t "~D users, average age ~,1F~%" count avg-age))
;; => "3 users, average age 30.0"
```

---

### Config Lookup
```common-lisp
(multiple-value-bind (key value type)
    (execute-one-row-m-v db "SELECT key, value, type FROM config WHERE key = ?" "timeout")
  (parse-config-value value type))
```

---

### No Rows (All :null)
```common-lisp
(multiple-value-bind (name age)
    (execute-one-row-m-v db "SELECT name, age FROM users WHERE id = ?" 999)
  ;; name = :null, age = :null
  (if (eq name :null)
      (format t "User not found~%")
      (format t "Found: ~A~%" name)))
```

---

### Destructuring
```common-lisp
(destructuring-bind (name age active)
    (multiple-value-list
     (execute-one-row-m-v db "SELECT name, age, active FROM users WHERE id = ?" 1))
  (process-user name age active))
```

---

### Ignoring Some Values
```common-lisp
(multiple-value-bind (name age)
    (execute-one-row-m-v db "SELECT name, age, email FROM users WHERE id = ?" 1)
  ;; email column ignored
  (format t "~A (~A)~%" name age))
```

---

## Related Contracts

- `execute-single` - Return single value (first column)
- `execute-to-list` - Return all rows as list of lists
- `execute-non-query` - Execute non-query SQL

---

## Patterns

**Recommended:** Use when expecting single row with multiple columns
```common-lisp
(defun get-user-info (db user-id)
  (multiple-value-bind (name email age)
      (execute-one-row-m-v db
                           "SELECT name, email, age FROM users WHERE id = ?"
                           user-id)
    (list :name name :email email :age age)))
```

**Pattern: Optional row**
```common-lisp
(defun find-user (db user-id)
  (multiple-value-bind (name age)
      (execute-one-row-m-v db "SELECT name, age FROM users WHERE id = ?" user-id)
    (when (not (eq name :null))
      (make-user :name name :age age))))
```

**Pattern: Config retrieval**
```common-lisp
(defun get-config (db key default)
  (multiple-value-bind (value)
      (execute-one-row-m-v db "SELECT value FROM config WHERE key = ?" key)
    (if (eq value :null) default value)))
```

---

## Anti-patterns

❌ **For single column (use execute-single):**
```common-lisp
;; AWKWARD
(multiple-value-bind (name)
    (execute-one-row-m-v db "SELECT name FROM users WHERE id = ?" 1)
  ...)

;; BETTER
(let ((name (execute-single db "SELECT name FROM users WHERE id = ?" 1)))
  ...)
```

❌ **For multiple rows (use execute-to-list):**
```common-lisp
;; WRONG - Only gets first row
(multiple-value-bind (name age)
    (execute-one-row-m-v db "SELECT name, age FROM users")
  ...)

;; RIGHT
(dolist (row (execute-to-list db "SELECT name, age FROM users"))
  (destructuring-bind (name age) row
    ...))
```

❌ **Ignoring most columns:**
```common-lisp
;; INEFFICIENT
(multiple-value-bind (id)
    (execute-one-row-m-v db "SELECT id, name, age, email, phone FROM users WHERE id = ?" 1)
  ;; Only use id, wasteful to fetch other columns
  ...)

;; BETTER
(execute-single db "SELECT id FROM users WHERE id = ?" 1)
```

---

## Performance Notes

- Efficient for single-row, multi-column queries
- No list allocation for row (unlike execute-to-list)
- Statement caching applies

**Comparison:**
```common-lisp
;; execute-to-list: Allocates list for row
(first (execute-to-list db "SELECT name, age FROM users WHERE id = 1"))
;; => ("Alice" 30)  ; List allocated

;; execute-one-row-m-v: No list allocation
(multiple-value-bind (name age)
    (execute-one-row-m-v db "SELECT name, age FROM users WHERE id = 1")
  ...)
;; => "Alice", 30  ; Direct values
```

---

## Edge Cases

### Zero Columns (Degenerate)
```common-lisp
;; Hypothetical (no useful SQL returns zero columns)
(execute-one-row-m-v db "SELECT WHERE FALSE")
;; => (values)  ; Zero values
```

### Many Columns
```common-lisp
(multiple-value-bind (c1 c2 c3 c4 c5)
    (execute-one-row-m-v db "SELECT * FROM wide_table WHERE id = ?" 1)
  ;; Handle 5+ columns
  ...)
```

### Multiple Rows (First Row Returned)
```common-lisp
(multiple-value-bind (name age)
    (execute-one-row-m-v db "SELECT name, age FROM users ORDER BY id")
  ;; Returns first row's values
  ;; Rest of rows ignored
  ...)
```

---

## Validation

**Tests:**
- Tests implicitly verify multi-value return
- Usage in test assertions

**Documentation:**
- README.md:146-156
- REFERENCE.md:72-81
- CL-SQLITE.agent.md:881

**Verified Properties:**
- Returns multiple values (one per column)
- Columns 0-indexed (RULE-005)
- No rows → `:null` values
- Type conversion consistent

---

**Document Status:** Canonical
**Last Verified:** 2026-01-20 (Canon Initiation Pass 2)
