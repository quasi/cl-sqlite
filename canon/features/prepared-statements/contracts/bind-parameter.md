# Contract: bind-parameter

**Signature:** `(bind-parameter stmt position value) => nil`
**Confidence:** 0.95
**Status:** Stable

---

## Parameters

### stmt
**Type:** `sqlite-statement`
**Required:** Yes
**Description:** Prepared statement with parameter placeholders

**Constraints:**
- Must be valid `sqlite-statement` instance
- Must be prepared

---

### position
**Type:** `integer`
**Required:** Yes
**Description:** Parameter placeholder position (1-indexed, RULE-004)

**Constraints:**
- Must be between 1 and parameter count
- First placeholder: position 1 (not 0)
- Last placeholder: position (parameter-count)

**Example:**
```sql
"INSERT INTO users VALUES (?, ?, ?)"
```
- Position 1: first `?`
- Position 2: second `?`
- Position 3: third `?`

---

### value
**Type:** `(or null integer float string blob)`
**Required:** Yes
**Description:** Value to bind to placeholder

**Supported types:**
- `null` (`:null` keyword) → SQLite NULL
- `integer` → SQLite INTEGER
- `float` → SQLite REAL
- `string` → SQLite TEXT
- `(simple-array (unsigned-byte 8) (*))` → SQLite BLOB

**Type mapping:**
```common-lisp
(bind-parameter stmt 1 :null)           ; NULL
(bind-parameter stmt 1 42)              ; 42
(bind-parameter stmt 1 3.14)            ; 3.14
(bind-parameter stmt 1 "hello")         ; "hello"
(bind-parameter stmt 1 #(1 2 3))        ; Binary blob
```

---

## Returns

**Type:** `nil`
**Description:** No meaningful return value

---

## Side Effects

- Binds value to parameter placeholder
- Stores value internally in prepared statement
- Persists until statement reset or finalized

---

## Errors

### sqlite-error
**When:** Binding fails

**Common error codes:**
- `:RANGE` - Parameter index out of range
- `:NOMEM` - Out of memory

**Error Context:**
- `stmt` - Statement
- `error-code` - SQLite error code
- `error-msg` - "Could not bind parameter N"

### type-error
**When:** Unsupported value type provided

**Example:**
```common-lisp
(bind-parameter stmt 1 (make-instance 'my-class))
;; => type-error (unsupported type)
```

---

## Preconditions

- Statement prepared and compiled
- Parameter position valid (1 to parameter count)
- Value type supported

---

## Postconditions

**Success:**
- Value bound to parameter position
- Persists until statement reset

**Failure:**
- `sqlite-error` or `type-error` signaled
- Parameter binding unchanged

---

## Implementation

**Location:** `sqlite.lisp:248-270`

**Key Code:**
```common-lisp
(defun bind-parameter (stmt position value)
  (let ((error-code
         (etypecase value
           (null
            (sqlite-ffi:sqlite3-bind-null (stmt stmt) position))
           (integer
            (sqlite-ffi:sqlite3-bind-int64 (stmt stmt) position value))
           (float
            (sqlite-ffi:sqlite3-bind-double (stmt stmt) position value))
           (string
            (sqlite-ffi:sqlite3-bind-text (stmt stmt) position value))
           ((simple-array (unsigned-byte 8) (*))
            (sqlite-ffi:sqlite3-bind-blob (stmt stmt) position value)))))
    (unless (eq error-code :ok)
      (sqlite-error error-code (stmt stmt)
                    "Could not bind parameter ~D" position))))
```

**Mechanism:**
- Dispatches on value type via `etypecase`
- Calls appropriate SQLite FFI binding function
- Checks error code, signals error if needed

---

## Usage Examples

### Single Parameter
```common-lisp
(let ((stmt (prepare-statement db "SELECT * FROM users WHERE id = ?")))
  (bind-parameter stmt 1 42)
  (when (step-statement stmt)
    (format t "Found: ~A~%" (statement-column-value stmt 0))))
```

---

### Multiple Parameters
```common-lisp
(let ((stmt (prepare-statement db "INSERT INTO logs VALUES (?, ?, ?)")))
  (bind-parameter stmt 1 1000)           ; timestamp
  (bind-parameter stmt 2 "Event A")      ; message
  (bind-parameter stmt 3 "INFO")         ; level
  (step-statement stmt))
```

---

### With NULL
```common-lisp
(let ((stmt (prepare-statement db "INSERT INTO users (name, email) VALUES (?, ?)")))
  (bind-parameter stmt 1 "Alice")
  (bind-parameter stmt 2 :null)          ; email is NULL
  (step-statement stmt))
```

---

### With BLOB
```common-lisp
(let ((stmt (prepare-statement db "INSERT INTO files VALUES (?, ?)")))
  (bind-parameter stmt 1 "image.png")
  (bind-parameter stmt 2 (read-file-to-bytes "image.png"))
  (step-statement stmt))
```

---

### Loop with Reset
```common-lisp
(let ((stmt (prepare-statement db "INSERT INTO logs VALUES (?, ?)")))
  (dotimes (i 100)
    (bind-parameter stmt 1 (+ 1000 i))
    (bind-parameter stmt 2 (format nil "Event ~D" i))
    (step-statement stmt)
    (reset-statement stmt))
  (finalize-statement stmt))
```

---

## Related Contracts

- `prepare-statement` - Creates statement with placeholders
- `clear-bindings` - Clears all parameter bindings
- `reset-statement` - Resets statement (after this, rebind before stepping)
- `step-statement` - Executes with bound parameters

---

## Patterns

**Recommended:** Use high-level execute functions (automatic binding)
```common-lisp
(execute-non-query db "INSERT INTO logs VALUES (?, ?)" ts msg)
;; Binding automatic
```

**Manual binding:** For repeated execution
```common-lisp
(let ((stmt (prepare-statement db "INSERT INTO logs VALUES (?, ?)")))
  (unwind-protect
      (dotimes (i 1000)
        (bind-parameter stmt 1 (+ 1000 i))
        (bind-parameter stmt 2 (format nil "Event ~D" i))
        (step-statement stmt)
        (reset-statement stmt))
    (finalize-statement stmt)))
```

---

## Performance Notes

- **Binding overhead:** Negligible (simple copy)
- **Parameter count:** No performance penalty
- **Reuse:** Reset and rebind faster than reprepare

---

## SQLite Semantics

**Index base (RULE-004):**
- Parameters are 1-indexed (not 0-indexed)
- Matches SQLite C API convention
- First placeholder: position 1

**Type affinity:**
- SQLite applies type affinity to bound values
- INTEGER column with string value: converted to integer
- TEXT column with integer value: converted to string

---

## Error Handling

### Out-of-Range Index
```common-lisp
(let ((stmt (prepare-statement db "SELECT * FROM users WHERE id = ?")))
  (bind-parameter stmt 2 42))  ; Only 1 placeholder
;; => sqlite-error :RANGE (parameter index out of range)
```

---

### Type Mismatch (Not an error at binding time)
```common-lisp
(let ((stmt (prepare-statement db "INSERT INTO age VALUES (?)")))
  (bind-parameter stmt 1 "not a number")
  (step-statement stmt))
;; Succeeds at binding
;; SQLite's type affinity handles conversion
```

---

## Validation

**Tests:**
- `test-prepare-statement` (sqlite-tests.lisp:20): Basic binding
- `test-multi-insert` (sqlite-tests.lisp:40): Repeated binding and reset

**Documentation:**
- README.md (examples with binding)
- REFERENCE.md:121-138
- CL-SQLITE.agent.md:886

**Verified Properties:**
- Parameters bound correctly
- Multiple parameters supported
- Type conversion works

---

**Document Status:** Canonical
**Last Verified:** 2026-01-20 (Canon Initiation Pass 2)
