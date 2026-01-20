# Contract: with-open-database

**Signature:** `(with-open-database (db database-path &key busy-timeout) &body body) => result`
**Confidence:** 0.95
**Status:** Stable
**Form:** Macro

---

## Parameters

### db
**Type:** Symbol (variable name)
**Required:** Yes
**Description:** Variable name to bind database handle to
**Scope:** Lexical, bound within `body`

### database-path
**Type:** `(or string pathname)`
**Required:** Yes
**Description:** Path to SQLite database file or `:memory:`
**Constraints:** Same as `connect` function

### busy-timeout
**Type:** `(or null integer)`
**Required:** No
**Default:** `nil`
**Description:** Busy timeout in milliseconds
**Constraints:** Same as `connect` function

### body
**Type:** Forms (implicit progn)
**Required:** No (can be empty)
**Description:** Code to execute with database connection

---

## Returns

**Type:** `T` (any type)
**Description:** Value of last form in `body`

**Behavior:**
- If `body` completes normally: returns last form's value
- If `body` signals error: error propagates after cleanup
- If non-local exit (return-from, throw): cleanup occurs, control transfers

---

## Behavior

**Expansion:**
```common-lisp
(let ((db nil))
  (unwind-protect
      (progn
        (setf db (connect database-path :busy-timeout busy-timeout))
        ,@body)
    (when db (disconnect db))))
```

**Guarantee:** `disconnect` called even if:
- Body signals error
- Non-local exit occurs (return-from, throw, go)
- Normal completion

---

## Side Effects

- Opens database connection (via `connect`)
- Executes `body` forms (arbitrary side effects)
- Closes connection (via `disconnect`)
- Finalizes cached statements on exit

---

## Errors

### From connect
- `sqlite-error` if database cannot be opened
- Connection never established, no cleanup needed

### From body
- Any error from user code
- Connection cleanup guaranteed before error propagates

### From disconnect
- `sqlite-error` if unfinalised statements exist (rare)
- Error propagates after attempted cleanup

---

## Preconditions

- Same as `connect` function
- Parent directory exists (for file-based)
- Adequate permissions

---

## Postconditions

**Success:**
- Connection established
- `body` executed
- Connection closed
- Return value from `body`

**Failure (during connect):**
- `sqlite-error` signaled
- No connection established
- No cleanup needed

**Failure (during body):**
- Error propagated
- Connection cleanup attempted
- Resources released

---

## Implementation

**Location:** `sqlite.lisp:137-143`

**Key Code:**
```common-lisp
(defmacro with-open-database ((db database-path &key busy-timeout) &body body)
  `(let ((,db nil))
     (unwind-protect
          (progn
            (setf ,db (connect ,database-path :busy-timeout ,busy-timeout))
            ,@body)
       (when ,db (disconnect ,db)))))
```

**Design Pattern:** Resource Acquisition Is Initialization (RAII) via `unwind-protect`

---

## Usage Examples

### Basic Usage
```common-lisp
(with-open-database (db ":memory:")
  (execute-non-query db "CREATE TABLE users (id INTEGER, name TEXT)")
  (execute-non-query db "INSERT INTO users VALUES (1, 'Alice')")
  (execute-to-list db "SELECT * FROM users"))
;; Returns: ((1 "Alice"))
;; Connection automatically closed
```

### With Busy Timeout
```common-lisp
(with-open-database (db "/shared/db.sqlite" :busy-timeout 5000)
  (insert db :logs '(:message "Event occurred")))
```

### Error Handling
```common-lisp
(handler-case
    (with-open-database (db "db.sqlite")
      (error "Something went wrong"))
  (error (e)
    ;; Connection already closed by unwind-protect
    (format t "Error: ~A~%" e)))
```

### Nested Databases
```common-lisp
(with-open-database (db1 "source.sqlite")
  (with-open-database (db2 "target.sqlite")
    (let ((data (select db1 :users)))
      (dolist (row data)
        (insert db2 :users row)))))
;; Both connections closed in reverse order
```

### Early Exit
```common-lisp
(with-open-database (db "db.sqlite")
  (when (some-condition)
    (return-from function-name result))
  ;; Connection still closed even with early return
  (do-more-work db))
```

---

## Related Contracts

- `connect` - Underlying connection function
- `disconnect` - Underlying cleanup function

---

## Patterns

**Recommended:** Use for all scoped database operations
```common-lisp
;; DO: Automatic cleanup
(with-open-database (db "db.sqlite")
  (do-work db))

;; DON'T: Manual management (unless connection spans scopes)
(let ((db (connect "db.sqlite")))
  (do-work db)
  (disconnect db))
```

**Long-lived connections:** Don't use macro
```common-lisp
;; Application-level connection (outlives function scope)
(defvar *app-db* (connect "/var/app/database.sqlite"))

;; Shutdown hook
(defun shutdown ()
  (disconnect *app-db*))
```

**Why use macro:**
- Automatic cleanup (no forgotten `disconnect`)
- Exception-safe (cleanup even on error)
- Prevents resource leaks
- Enforces scoped connection pattern

---

## Design Rationale

**Why macro (not function):**
- Needs to bind variable in caller's scope
- `unwind-protect` requires compile-time structure

**Why unwind-protect:**
- Guarantees cleanup even on non-local exits
- Matches RAII pattern from other languages

**Why check `(when ,db ...)` in cleanup:**
- If `connect` fails, `db` is `nil`
- Prevents calling `disconnect` on `nil`
- Graceful degradation

---

## Validation

**Tests:**
- All tests implicitly verify (most tests use this macro)
- Error safety verified via exception tests

**Documentation:**
- README.md:98-112 (primary example pattern)
- REFERENCE.md:39-44
- CL-SQLITE.agent.md:300-307 (PATTERN-001)

**Verified Properties:**
- Resource cleanup (ARCH-002, ARCH-004)
- Exception safety via unwind-protect

---

**Document Status:** Canonical
**Last Verified:** 2026-01-20 (Canon Initiation Pass 2)
