# Contract: execute-non-query

**Signature:** `(execute-non-query db sql &rest parameters) => nil`
**Confidence:** 0.95
**Status:** Stable

---

## Parameters

### db
**Type:** `sqlite-handle`
**Required:** Yes
**Description:** Connected database handle

**Constraints:**
- Must be valid `sqlite-handle` instance
- Connection must be active

---

### sql
**Type:** `string`
**Required:** Yes
**Description:** SQL statement to execute

**Constraints:**
- Must contain exactly one SQL statement (RULE-001)
- Typically DDL (CREATE, DROP, ALTER) or DML (INSERT, UPDATE, DELETE)
- May contain parameter placeholders (`?`)

**Examples:**
```sql
"CREATE TABLE users (id INTEGER, name TEXT)"
"INSERT INTO users VALUES (?, ?)"
"UPDATE users SET name = ? WHERE id = ?"
"DELETE FROM users WHERE id = ?"
```

---

### parameters
**Type:** `&rest` (variable arguments)
**Required:** No
**Description:** Values to bind to SQL placeholders

**Constraints:**
- Count must match number of `?` in SQL
- Types: integer, float, string, null, blob (byte vector)
- Parameters are 1-indexed (RULE-004)

**Examples:**
```common-lisp
(execute-non-query db "INSERT INTO users VALUES (?, ?)" 1 "Alice")
(execute-non-query db "UPDATE users SET active = ?" 1)
(execute-non-query db "DELETE FROM users WHERE id = ?" 42)
```

---

## Returns

**Type:** `nil`
**Description:** No meaningful return value

**Note:** Use `last-insert-rowid` to get auto-generated keys

---

## Side Effects

- Executes SQL statement
- Modifies database (for DML)
- Creates/alters schema (for DDL)
- May trigger constraints, indexes, triggers

---

## Errors

### sqlite-error
**When:** SQL execution fails

**Common error codes:**
- `:SQL` - SQL syntax error
- `:CONSTRAINT` - Constraint violation (PRIMARY KEY, FOREIGN KEY, NOT NULL, etc.)
- `:BUSY` - Database locked
- `:READONLY` - Attempt to write read-only database
- `:MISMATCH` - Type affinity mismatch

**Error Context:**
- `db-handle` - Database handle
- `error-code` - SQLite error code keyword
- `error-msg` - Human-readable message

**Examples:**
```common-lisp
;; Syntax error
(execute-non-query db "CRATE TABLE users ...")  ; Typo
;; => sqlite-error :SQL "near \"CRATE\": syntax error"

;; Constraint violation
(execute-non-query db "INSERT INTO users VALUES (1, 'Alice')")
(execute-non-query db "INSERT INTO users VALUES (1, 'Bob')")
;; => sqlite-error :CONSTRAINT "UNIQUE constraint failed: users.id"

;; Parameter count mismatch
(execute-non-query db "INSERT INTO users VALUES (?, ?)" 1)
;; => sqlite-error (insufficient parameters)
```

---

### type-error
**When:** Invalid parameter type

**Example:**
```common-lisp
(execute-non-query db "INSERT INTO users VALUES (?)"
                   (make-instance 'my-class))
;; => type-error (unsupported type)
```

---

## Preconditions

- Database connected (`(slot-boundp db 'handle)` is `T`)
- SQL is valid SQLite syntax
- Parameter count matches placeholders
- Parameter types supported

---

## Postconditions

**Success:**
- SQL executed
- Database modified (for DML)
- Schema changed (for DDL)
- Returns `nil`

**Failure:**
- `sqlite-error` or `type-error` signaled
- Database unchanged (rolled back)

---

## Implementation

**Location:** `sqlite.lisp:180-195`

**Key Code:**
```common-lisp
(defun execute-non-query (db sql &rest parameters)
  (let ((stmt (prepare-statement db sql)))
    (loop for i from 1
          for param in parameters
          do (bind-parameter stmt i param))
    (unwind-protect
        (progn
          (step-statement stmt)
          (finalize-statement stmt))
      (finalize-statement stmt))))
```

**Mechanism:**
1. Prepare statement (cached if repeated)
2. Bind all parameters (1-indexed)
3. Step statement once (execute)
4. Finalize statement
5. Cleanup via `unwind-protect`

---

## Usage Examples

### DDL: Create Table
```common-lisp
(execute-non-query db
  "CREATE TABLE users (
     id INTEGER PRIMARY KEY AUTOINCREMENT,
     name TEXT NOT NULL,
     age INTEGER)")
```

---

### DML: Insert
```common-lisp
(execute-non-query db
  "INSERT INTO users (name, age) VALUES (?, ?)"
  "Alice" 30)
```

---

### DML: Update
```common-lisp
(execute-non-query db
  "UPDATE users SET age = ? WHERE name = ?"
  31 "Alice")
```

---

### DML: Delete
```common-lisp
(execute-non-query db
  "DELETE FROM users WHERE age < ?"
  18)
```

---

### With NULL
```common-lisp
(execute-non-query db
  "INSERT INTO users (name, age) VALUES (?, ?)"
  "Bob" :null)
```

---

### With BLOB
```common-lisp
(execute-non-query db
  "INSERT INTO files (name, content) VALUES (?, ?)"
  "image.png"
  (read-file-to-byte-vector "image.png"))
```

---

## Related Contracts

- `prepare-statement` - Underlying statement preparation
- `bind-parameter` - Parameter binding
- `step-statement` - Statement execution
- `finalize-statement` - Statement cleanup
- `last-insert-rowid` - Retrieve auto-generated keys
- `execute-single` - Execute query, return single value
- `execute-to-list` - Execute query, return all rows

---

## Patterns

**Recommended:** Use for all non-query SQL
```common-lisp
(with-open-database (db "app.db")
  (execute-non-query db "CREATE TABLE logs (ts INTEGER, msg TEXT)")
  (execute-non-query db "INSERT INTO logs VALUES (?, ?)"
                     (get-universal-time) "Application started"))
```

**Anti-pattern:** String concatenation (SQL injection risk)
```common-lisp
;; WRONG - SQL injection vulnerability
(execute-non-query db
  (format nil "INSERT INTO users (name) VALUES ('~A')" user-input))

;; RIGHT - Parameterized
(execute-non-query db
  "INSERT INTO users (name) VALUES (?)" user-input)
```

---

## Performance Notes

- **Statement caching:** Repeated SQL strings reuse prepared statements
- **Cache size:** 16 entries (LRU eviction)
- **Recommendation:** Use consistent SQL strings to leverage cache

**Example:**
```common-lisp
;; GOOD: Same SQL string (cached)
(dotimes (i 1000)
  (execute-non-query db "INSERT INTO users VALUES (?, ?)" i (format nil "User~D" i)))

;; BAD: Different SQL strings (no caching benefit)
(dotimes (i 1000)
  (execute-non-query db
    (format nil "INSERT INTO users VALUES (~D, 'User~D')" i i)))
```

---

## Validation

**Tests:**
- `test-create-table` (sqlite-tests.lisp:22): Basic DDL
- `test-insert` (sqlite-tests.lisp:26): Basic DML
- `test-multi-insert` (sqlite-tests.lisp:40): Multiple inserts

**Documentation:**
- README.md:113-126
- REFERENCE.md:46-56
- CL-SQLITE.agent.md:878

**Verified Properties:**
- Single statement requirement (RULE-001)
- Parameter binding safety (INV-003)
- Parameter indexing (RULE-004)

---

**Document Status:** Canonical
**Last Verified:** 2026-01-20 (Canon Initiation Pass 2)
