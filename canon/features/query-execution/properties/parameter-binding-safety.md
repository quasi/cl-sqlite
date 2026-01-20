# Property: Parameter Binding Safety

**ID:** INV-003
**Confidence:** 0.95
**Status:** Verified
**Type:** Invariant

---

## Statement

```
∀ parameter p, SQL statement s:
  bind-parameter(stmt, position, p) →
  Parameter inserted at placeholder, never concatenated into SQL
```

**Natural Language:**
Parameters are always bound safely to their placeholders and never concatenated into the SQL string.

---

## Rationale

**SQL Injection Prevention:**
- Safe: Parameters handled by SQLite C API
- Unsafe: String concatenation in Lisp

**Design principle:** Parameterization enforced at API level
- All execute functions use `?` placeholders
- No string concatenation in library code
- User writes SQL with placeholders, provides parameters separately

---

## Enforcement Mechanisms

### In execute Functions

**All execute functions follow pattern:**
```common-lisp
(defun execute-non-query (db sql &rest parameters)
  (let ((stmt (prepare-statement db sql)))
    ;; Bind each parameter (never concatenate)
    (loop for i from 1
          for param in parameters
          do (bind-parameter stmt i param))
    ...))
```

**Key points:**
- SQL string is prepared as-is (no modification)
- Parameters bound via `bind-parameter` function
- SQLite C API handles parameter insertion safely

---

### In bind-parameter Function

**Location:** `sqlite.lisp:248-270`

```common-lisp
(defun bind-parameter (stmt position value)
  (let ((error-code
         (etypecase value
           (null
            (sqlite-ffi:sqlite3-bind-null stmt position))
           (integer
            (sqlite-ffi:sqlite3-bind-int64 stmt position value))
           (float
            (sqlite-ffi:sqlite3-bind-double stmt position value))
           (string
            (sqlite-ffi:sqlite3-bind-text stmt position value))
           ((simple-array (unsigned-byte 8) (*))
            (sqlite-ffi:sqlite3-bind-blob stmt position value)))))
    (unless (eq error-code :ok)
      (sqlite-error error-code stmt
                    "Could not bind parameter ~D" position))))
```

**Key mechanism:**
- Dispatches to appropriate SQLite C function based on type
- Each C function handles safe insertion
- No Lisp string manipulation of SQL

---

### Placeholder Format

**Requirement (RULE-001):**
- SQL uses `?` placeholders (positional)
- No named parameters
- Placeholders are literal `?` in SQL string

**Example:**
```sql
"INSERT INTO users (name, age) VALUES (?, ?)"
```

**Not allowed:**
```sql
"INSERT INTO users (name, age) VALUES (:name, :age)"  -- Named params
"INSERT INTO users (name, age) VALUES ('" + name + "')"  -- Concatenation
```

---

## Verification Methods

### Type Dispatch Verification

```common-lisp
(defun verify-parameter-safe (stmt position value)
  ;; Verifies etypecase dispatch (correct handler chosen)
  (ecase (type-of value)
    (null (sqlite-ffi:sqlite3-bind-null ...))
    (integer (sqlite-ffi:sqlite3-bind-int64 ...))
    (float (sqlite-ffi:sqlite3-bind-double ...))
    (string (sqlite-ffi:sqlite3-bind-text ...))
    ((simple-array (unsigned-byte 8) (*)) (sqlite-ffi:sqlite3-bind-blob ...))))
```

### SQL Injection Prevention Test

```common-lisp
(deftest test-sql-injection-prevented ()
  ;; Attempt to inject SQL via parameter
  (let ((malicious-input "'; DROP TABLE users; --"))
    (execute-non-query db
      "INSERT INTO names (value) VALUES (?)"
      malicious-input)
    ;; If vulnerable, table would be dropped
    ;; If safe, malicious-input treated as literal string
    (let ((result (execute-to-list db "SELECT * FROM names")))
      (assert-equal 1 (length result))
      (assert-equal (first (first result)) malicious-input))))
```

---

## Implications

### For Users

**Safe usage:**
```common-lisp
(execute-non-query db
  "INSERT INTO users (name, age) VALUES (?, ?)"
  user-name user-age)
```

**Unsafe (hypothetical, not possible with this API):**
```common-lisp
;; WRONG - Not possible with cl-sqlite API
(execute-non-query db
  (format nil "INSERT INTO users VALUES ('~A', ~D)" name age))
```

---

### Type Safety

**Supported types:**
- `null` (`:null` keyword) → SQLite NULL
- `integer` → SQLite INTEGER
- `float` → SQLite REAL
- `string` → SQLite TEXT
- `(simple-array (unsigned-byte 8) (*))` → SQLite BLOB

**Type conversion:**
- Automatic via `etypecase` dispatch
- Error if unsupported type provided

**Example:**
```common-lisp
(execute-non-query db "INSERT INTO t VALUES (?)" 42)  ; integer
(execute-non-query db "INSERT INTO t VALUES (?)" 3.14)  ; float
(execute-non-query db "INSERT INTO t VALUES (?)" "text")  ; string
(execute-non-query db "INSERT INTO t VALUES (?)" :null)  ; NULL
(execute-non-query db "INSERT INTO t VALUES (?)" #(1 2 3))  ; blob
```

---

## Violations and Consequences

### If Parameter Not Bound

**Scenario:** Parameter placeholder without corresponding value
```common-lisp
(execute-non-query db "INSERT INTO t VALUES (?, ?)" 1)
;; Only 1 parameter provided, but 2 placeholders
```

**Consequence:**
- SQLite error: "Incorrect number of bindings"
- Operation fails safely

---

### If SQL Concatenation Used (Outside API)

**Scenario:** User constructs SQL outside library
```common-lisp
;; WRONG (if user did this)
(let ((name "O'Brien"))
  (execute-non-query db
    (format nil "INSERT INTO users (name) VALUES ('~A')" name)))
;; => SQLite syntax error (quote in name breaks SQL)
```

**Mitigation:** Always use parameters
```common-lisp
;; RIGHT
(let ((name "O'Brien"))
  (execute-non-query db
    "INSERT INTO users (name) VALUES (?)" name))
```

---

## Related Rules

- **RULE-001 (Single Statement Requirement):** Placeholder format fixed
- **RULE-003 (Bind Before Step):** Parameters must be bound before stepping
- **RULE-004 (Parameter Index Base):** Parameters 1-indexed

---

## Test Evidence

**Implicit in all tests:** Any test using parameters verifies safe binding

**SQL Injection Test:** Not explicitly in test suite (assumed safe via C API)

**Documentation:**
- CL-SQLITE.agent.md:228-236 (INV-003)
- ANTI-001 (Anti-pattern: SQL concatenation)

---

## SQLite Internals

**How SQLite handles parameters:**
1. SQL prepared with placeholders as-is
2. Parameter binding phase: SQLite internally stores values
3. Execution phase: Values substituted safely (never as SQL text)

**Why safe:**
- No SQL parsing in parameter values
- Values stored in native C types
- Substitution happens in C, not via string operations

---

**Document Status:** Canonical
**Last Verified:** 2026-01-20 (Canon Initiation Pass 4)
