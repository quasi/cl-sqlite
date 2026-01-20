# Contract: last-insert-rowid

**Signature:** `(last-insert-rowid db) => integer`
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

## Returns

**Type:** `integer`
**Description:** The rowid of the most recent successful INSERT into a rowid table

**Behavior:**
- Returns rowid of last INSERT on this connection
- If no INSERT yet: returns 0
- Only tracks INSERT operations
- Only tracks rowid tables (not WITHOUT ROWID tables)
- Connection-specific (not affected by other connections)

**Rowid tables:**
- Tables without `WITHOUT ROWID` clause
- Have implicit `rowid` column (unless aliased by `INTEGER PRIMARY KEY`)

---

## Side Effects

None (read-only query)

---

## Errors

None documented. `sqlite3_last_insert_rowid` always succeeds.

---

## Preconditions

- Database connected (`(slot-boundp db 'handle)` is `T`)

---

## Postconditions

**Success:**
- Returns integer rowid
- Database state unchanged

---

## Implementation

**Location:** `sqlite.lisp:271-280`

**Key Code:**
```common-lisp
(defun last-insert-rowid (db)
  (sqlite-ffi:sqlite3-last-insert-rowid (handle db)))
```

**Direct FFI Call:** Thin wrapper over `sqlite3_last_insert_rowid`

**SQLite C API:**
```c
sqlite3_int64 sqlite3_last_insert_rowid(sqlite3*);
```

---

## Usage Examples

### Auto-Generated Primary Key
```common-lisp
(execute-non-query db
  "CREATE TABLE users (
     id INTEGER PRIMARY KEY AUTOINCREMENT,
     name TEXT)")

(execute-non-query db "INSERT INTO users (name) VALUES (?)" "Alice")
(let ((user-id (last-insert-rowid db)))
  (format t "Inserted user with ID: ~D~%" user-id))
;; => "Inserted user with ID: 1"
```

---

### Inserting Related Records
```common-lisp
;; Insert parent
(execute-non-query db "INSERT INTO orders (customer) VALUES (?)" "Alice")
(let ((order-id (last-insert-rowid db)))
  ;; Insert children with foreign key
  (execute-non-query db "INSERT INTO order_items (order_id, product) VALUES (?, ?)"
                     order-id "Widget")
  (execute-non-query db "INSERT INTO order_items (order_id, product) VALUES (?, ?)"
                     order-id "Gadget"))
```

---

### Batch Insert with IDs
```common-lisp
(let ((inserted-ids '()))
  (dolist (name '("Alice" "Bob" "Charlie"))
    (execute-non-query db "INSERT INTO users (name) VALUES (?)" name)
    (push (last-insert-rowid db) inserted-ids))
  (nreverse inserted-ids))
;; => (1 2 3)
```

---

### No Prior INSERT
```common-lisp
(let ((db (connect ":memory:")))
  (last-insert-rowid db))
;; => 0
```

---

### After Non-INSERT Operation
```common-lisp
(execute-non-query db "INSERT INTO users (name) VALUES (?)" "Alice")
(last-insert-rowid db)  ; => 1

(execute-non-query db "UPDATE users SET name = ? WHERE id = ?" "Alice Smith" 1)
(last-insert-rowid db)  ; => 1 (unchanged, UPDATE doesn't affect it)

(execute-non-query db "DELETE FROM users WHERE id = ?" 1)
(last-insert-rowid db)  ; => 1 (unchanged, DELETE doesn't affect it)
```

---

## Related Contracts

- `execute-non-query` - Performs INSERT that generates rowid
- `execute-single` - Alternative: `SELECT last_insert_rowid()`

---

## Patterns

**Recommended:** Use after INSERT to get auto-generated key
```common-lisp
(defun create-user (db name)
  (execute-non-query db "INSERT INTO users (name) VALUES (?)" name)
  (last-insert-rowid db))
```

**Pattern: Insert and retrieve**
```common-lisp
(defun insert-and-fetch (db name)
  (execute-non-query db "INSERT INTO users (name) VALUES (?)" name)
  (let ((id (last-insert-rowid db)))
    (execute-one-row-m-v db "SELECT id, name FROM users WHERE id = ?" id)))
```

**Pattern: Batch insert with tracking**
```common-lisp
(defun insert-users (db names)
  (mapcar (lambda (name)
            (execute-non-query db "INSERT INTO users (name) VALUES (?)" name)
            (cons name (last-insert-rowid db)))
          names))
;; => (("Alice" . 1) ("Bob" . 2) ("Charlie" . 3))
```

---

## Anti-patterns

❌ **Assuming sequential IDs:**
```common-lisp
;; WRONG - IDs may have gaps (deletes, failed inserts)
(execute-non-query db "INSERT INTO users (name) VALUES (?)" "Alice")
(let ((id (last-insert-rowid db)))
  ;; Assuming next ID will be (1+ id) - WRONG!
  ...)

;; RIGHT - Always use last-insert-rowid after INSERT
(execute-non-query db "INSERT INTO users (name) VALUES (?)" "Bob")
(let ((bob-id (last-insert-rowid db)))  ; Get actual ID
  ...)
```

❌ **Using across connections:**
```common-lisp
;; WRONG - last-insert-rowid is connection-specific
(let ((db1 (connect "app.db"))
      (db2 (connect "app.db")))
  (execute-non-query db1 "INSERT INTO users (name) VALUES (?)" "Alice")
  (last-insert-rowid db2))  ; => 0 or different value (WRONG!)

;; RIGHT - Same connection
(let ((db (connect "app.db")))
  (execute-non-query db "INSERT INTO users (name) VALUES (?)" "Alice")
  (last-insert-rowid db))  ; Correct
```

❌ **For WITHOUT ROWID tables:**
```common-lisp
(execute-non-query db
  "CREATE TABLE config (key TEXT PRIMARY KEY, value TEXT) WITHOUT ROWID")
(execute-non-query db "INSERT INTO config VALUES (?, ?)" "timeout" "30")
(last-insert-rowid db)  ; => 0 or undefined (no rowid table)
```

---

## Edge Cases

### Multiple INSERTs
```common-lisp
(execute-non-query db "INSERT INTO users (name) VALUES (?)" "Alice")
(last-insert-rowid db)  ; => 1

(execute-non-query db "INSERT INTO users (name) VALUES (?)" "Bob")
(last-insert-rowid db)  ; => 2 (updated)
```

### INSERT with Explicit Rowid
```common-lisp
(execute-non-query db "INSERT INTO users (id, name) VALUES (?, ?)" 100 "Alice")
(last-insert-rowid db)  ; => 100 (explicit rowid)
```

### Failed INSERT
```common-lisp
(execute-non-query db "INSERT INTO users (name) VALUES (?)" "Alice")
(last-insert-rowid db)  ; => 1

(handler-case
    (execute-non-query db "INSERT INTO users (id, name) VALUES (?, ?)" 1 "Bob")
  (sqlite-error (e)
    ;; Constraint violation (duplicate id)
    ))

(last-insert-rowid db)  ; => 1 (unchanged, failed INSERT doesn't update)
```

### Concurrent Connections
```common-lisp
;; Connection A
(execute-non-query db-a "INSERT INTO users (name) VALUES (?)" "Alice")
(last-insert-rowid db-a)  ; => 1

;; Connection B (different connection, same database)
(execute-non-query db-b "INSERT INTO users (name) VALUES (?)" "Bob")
(last-insert-rowid db-b)  ; => 2
(last-insert-rowid db-a)  ; => 1 (unchanged, connection-specific)
```

---

## Performance Notes

- Very fast (single FFI call, no SQL parsing)
- No database I/O
- Preferred over `SELECT last_insert_rowid()` (no query overhead)

**Comparison:**
```common-lisp
;; Fast (direct FFI)
(last-insert-rowid db)

;; Slower (SQL parsing + execution)
(execute-single db "SELECT last_insert_rowid()")
```

---

## SQLite Internals

**What SQLite tracks:**
- Each connection has `lastRowid` field
- Updated after successful INSERT into rowid table
- Reset to 0 on connection open

**Not tracked:**
- INSERT into WITHOUT ROWID tables
- INSERT into views (instead of tables)
- INSERT OR IGNORE that doesn't insert (constraint violation)

---

## Validation

**Tests:**
- `test-last-insert-rowid` (sqlite-tests.lisp:30): Basic usage
- Various tests use last-insert-rowid for foreign key insertion

**Documentation:**
- README.md:157-164
- REFERENCE.md:82-87
- CL-SQLITE.agent.md:882

**Verified Properties:**
- Returns correct rowid after INSERT
- Returns 0 if no INSERT yet
- Connection-specific behavior

---

## References

**SQLite Documentation:**
- https://www.sqlite.org/c3ref/last_insert_rowid.html
- https://www.sqlite.org/lang_createtable.html#rowid

**Related Concepts:**
- Rowid vs. WITHOUT ROWID tables
- INTEGER PRIMARY KEY aliasing
- AUTOINCREMENT constraint

---

**Document Status:** Canonical
**Last Verified:** 2026-01-20 (Canon Initiation Pass 2)
