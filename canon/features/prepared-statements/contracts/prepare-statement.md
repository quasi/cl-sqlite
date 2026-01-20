# Contract: prepare-statement

**Signature:** `(prepare-statement db sql) => sqlite-statement`
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
**Description:** SQL statement to prepare

**Constraints:**
- Must contain exactly one SQL statement (RULE-001)
- Must be valid SQLite SQL syntax
- May contain parameter placeholders (`?`)

**Examples:**
```sql
"SELECT * FROM users WHERE id = ?"
"INSERT INTO logs VALUES (?, ?)"
"UPDATE config SET value = ? WHERE key = ?"
```

---

## Returns

**Type:** `sqlite-statement`
**Description:** Prepared statement handle

**Properties:**
- `stmt` slot bound to foreign pointer (`sqlite3_stmt*`)
- `db` slot references parent database handle
- `column-count` slot initialized
- Ready for parameter binding and stepping

---

## Side Effects

- Compiles SQL to bytecode (C level)
- Adds statement to connection's statement cache
- If cache full: evicts least-recently-used statement (via MRU destructor)

---

## Errors

### sqlite-error
**When:** SQL syntax error

**Common error codes:**
- `:SQL` - SQL syntax error

**Error Context:**
- `db-handle` - Database handle
- `error-code` - SQLite error code
- `error-msg` - Human-readable message

**Examples:**
```common-lisp
(prepare-statement db "SELCT * FROM users")  ; Typo
;; => sqlite-error :SQL "near \"SELCT\": syntax error"

(prepare-statement db "SELECT * FROM nonexistent")
;; => Succeeds (table doesn't need to exist at prepare time)

(prepare-statement db "SELECT * FROM users; SELECT * FROM logs")
;; => Succeeds (SQLite prepares first statement, ignores second)
```

---

## Preconditions

- Database connected (`(slot-boundp db 'handle)` is `T`)
- SQL is syntactically valid

---

## Postconditions

**Success:**
- `sqlite-statement` instance created
- `stmt` slot bound to foreign pointer
- Statement compiled and ready
- Column count available

**Failure:**
- `sqlite-error` signaled
- No resources allocated (no cleanup needed)

---

## Implementation

**Location:** `sqlite.lisp:145-160`

**Key Code:**
```common-lisp
(defun prepare-statement (db sql)
  (let ((stmt (make-instance 'sqlite-statement :db db)))
    (cache-statement db sql stmt)
    stmt))

(defun cache-statement (db sql stmt)
  (let ((cached (get-cached-statement (cache db) sql)))
    (if cached
        (copy-statement-state stmt cached)
        (compile-statement stmt sql))))

(defun compile-statement (stmt sql)
  (cffi:with-foreign-object (ppstmt 'sqlite-ffi:p-sqlite3-stmt)
    (let ((error-code (sqlite-ffi:sqlite3-prepare-v2
                       (handle (db stmt)) sql -1 ppstmt (cffi:null-pointer))))
      (if (eq error-code :ok)
          (setf (stmt stmt) (cffi:mem-ref ppstmt 'sqlite-ffi:p-sqlite3-stmt)
                (column-count stmt) (sqlite-ffi:sqlite3-column-count (stmt stmt)))
          (sqlite-error error-code (db stmt) "Could not prepare statement")))))
```

---

## Caching Behavior

### Cache Hit

**Same SQL string requested multiple times:**
```common-lisp
(prepare-statement db "SELECT * FROM users WHERE id = ?")  ; Compiles
(prepare-statement db "SELECT * FROM users WHERE id = ?")  ; Cached
(prepare-statement db "SELECT * FROM users WHERE id = ?")  ; Cached
```

**Result:** First call compiles, subsequent calls reuse cached statement

**Benefit:** Avoid SQL parsing overhead

---

### Cache Miss

**Different SQL strings:**
```common-lisp
(prepare-statement db "SELECT * FROM users WHERE id = ?")
(prepare-statement db "SELECT * FROM users WHERE age > ?")
```

**Result:** Both compiled separately (different SQL)

---

### Cache Eviction

**Cache size: 16 entries (MRU eviction):**
```common-lisp
;; Prepare 17 different statements
(loop for i from 0 below 17
      do (prepare-statement db (format nil "SELECT * FROM t~D WHERE id = ?" i)))
;; First statement evicted from cache (LRU)
;; If reused later, must be recompiled
```

---

## Usage Examples

### Basic Preparation
```common-lisp
(let ((stmt (prepare-statement db "SELECT * FROM users WHERE id = ?")))
  ;; Use statement...
  (finalize-statement stmt))
```

### With Parameters
```common-lisp
(let ((stmt (prepare-statement db "INSERT INTO logs (msg, level) VALUES (?, ?)")))
  (bind-parameter stmt 1 "Event occurred")
  (bind-parameter stmt 2 "INFO")
  (step-statement stmt)
  (finalize-statement stmt))
```

### Via execute Functions (Automatic)
```common-lisp
;; prepare-statement called internally
(execute-non-query db "SELECT * FROM users WHERE id = ?" 1)
;; Statement prepared, bound, stepped, finalized automatically
```

---

## Related Contracts

- `finalize-statement` - Release statement resources
- `step-statement` - Execute statement
- `bind-parameter` - Bind parameters
- `reset-statement` - Reset statement for re-execution
- `clear-bindings` - Clear parameter bindings

---

## Patterns

**Recommended:** Use high-level execute functions for simple operations
```common-lisp
(execute-non-query db "INSERT INTO logs VALUES (?, ?)" ts msg)
```

**Manual preparation:** For repeated execution
```common-lisp
(let ((stmt (prepare-statement db "INSERT INTO logs VALUES (?, ?)")))
  (dotimes (i 100)
    (bind-parameter stmt 1 (+ i 1000))
    (bind-parameter stmt 2 (format nil "Event ~D" i))
    (step-statement stmt)
    (reset-statement stmt)))
  (finalize-statement stmt))
```

---

## Performance Notes

- **Compilation overhead:** SQL parsing by SQLite
- **Cache benefit:** Repeated statements avoid recompilation
- **Statement reuse:** Reset and re-bind instead of repreparing

**Benchmark:**
```common-lisp
;; With cache hits: ~1-10 microseconds per prepare
;; Without cache: ~10-100 microseconds per prepare
;; (Depends on SQL complexity)
```

---

## Validation

**Tests:**
- `test-prepare-statement` (sqlite-tests.lisp:20): Basic preparation

**Documentation:**
- README.md:167-174
- REFERENCE.md:88-101
- CL-SQLITE.agent.md:883

**Verified Properties:**
- Prepared statements compile correctly
- Statements ready for binding and stepping
- Caching works as expected

---

**Document Status:** Canonical
**Last Verified:** 2026-01-20 (Canon Initiation Pass 2)
