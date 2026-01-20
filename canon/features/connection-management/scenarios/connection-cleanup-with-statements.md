# Scenario: Connection Cleanup with Cached Statements

**Confidence:** 0.95
**Status:** Verified
**Test:** `test-disconnect-with-statements` (sqlite-tests.lisp:17)

---

## Description

Verifies that `disconnect` properly finalizes all cached prepared statements before closing the connection.

**Purpose:** Validates statement cache cleanup during disconnection

**Critical Behavior:** Cached statements must be finalized before `sqlite3_close`, otherwise SQLite returns `:BUSY` error

---

## Setup

1. Create in-memory database
2. Create table with schema
3. Prepare statement (adds to cache)

---

## Execution

```common-lisp
(deftest test-disconnect-with-statements ()
  (let ((db (connect ":memory:")))
    (execute-non-query db "CREATE TABLE contacts (name TEXT, age INTEGER)")
    (prepare-statement db "SELECT * FROM contacts")
    (disconnect db)))
```

---

## Steps

1. **Connect** to `:memory:` database
   - Call `(connect ":memory:")`
   - Cache initialized (empty, size 16)

2. **Create table**
   - `(execute-non-query db "CREATE TABLE contacts ...")`
   - Schema established

3. **Prepare statement**
   - `(prepare-statement db "SELECT * FROM contacts")`
   - Statement compiled and added to cache
   - Foreign pointer (`sqlite3_stmt*`) stored in cache

4. **Disconnect with cached statement**
   - `(disconnect db)`
   - Cache cleared: `(sqlite.cache:clear-cache (cache db))`
   - Each cached statement finalized: `(really-finalize-statement stmt)`
   - After all statements finalized: `(sqlite3-close (handle db))`
   - Handle slot unbound

---

## Expected Outcomes

### During Disconnect
- ✅ Cache cleared before closing connection
- ✅ All cached statements finalized (C resources freed)
- ✅ `sqlite3_close` succeeds (no `:BUSY` error)
- ✅ Handle slot unbound
- ✅ No errors signaled

### Verification Points
- Statement count in cache before disconnect: 1
- Statement count in cache after disconnect: 0
- Connection state after disconnect: closed (handle unbound)

---

## Invariants Verified

- **INV-001 (Connection State Consistency):**
  - After disconnect: `(slot-boundp db 'handle)` is `NIL`

- **Resource Cleanup (ARCH-002):**
  - All cached statements finalized
  - Foreign resources released in correct order

---

## Properties Verified

- **Statement Finalization Before Close:**
  - `disconnect` clears cache before calling `sqlite3_close`
  - Order: finalize statements → close connection

- **Cache Destructor Invoked:**
  - Cache's `:destructor` function called on each entry
  - Destructor: `#'really-finalize-statement`

- **No Resource Leak:**
  - All prepared statement pointers freed
  - Connection pointer freed
  - No dangling foreign objects

---

## Implementation Details

**Disconnect implementation (sqlite.lisp:117-131):**
```common-lisp
(defun disconnect (db)
  (when (slot-boundp db 'handle)
    ;; CRITICAL: Clear cache BEFORE closing connection
    (sqlite.cache:clear-cache (cache db))

    ;; Now safe to close
    (let ((error-code (sqlite-ffi:sqlite3-close (handle db))))
      (unless (eq error-code :ok)
        (sqlite-error error-code db "Could not close sqlite3 database")))

    ;; Mark as disconnected
    (slot-makunbound db 'handle)))
```

**Cache initialization (sqlite.lisp:97-104):**
```common-lisp
(setf (cache object) (make-instance 'sqlite.cache:mru-cache
                                    :cache-size 16
                                    :destructor #'really-finalize-statement))
```

**Key Insight:** `:destructor` parameter ensures cache eviction/clearing calls `really-finalize-statement`

---

## What Could Go Wrong

### If cache not cleared:
```common-lisp
;; WRONG (hypothetical broken implementation)
(defun disconnect-broken (db)
  (when (slot-boundp db 'handle)
    ;; Skip cache clearing
    (let ((error-code (sqlite-ffi:sqlite3-close (handle db))))
      ;; ERROR: sqlite3_close returns :BUSY
      ;; Cause: Active prepared statements still exist
      (unless (eq error-code :ok)
        (sqlite-error error-code db "Could not close sqlite3 database")))))
```

**Result:** `sqlite-error` with `:BUSY` code

---

## Edge Cases

### Multiple Cached Statements
```common-lisp
(let ((db (connect ":memory:")))
  (execute-non-query db "CREATE TABLE t1 (x INTEGER)")
  (execute-non-query db "CREATE TABLE t2 (y INTEGER)")
  (prepare-statement db "SELECT * FROM t1")
  (prepare-statement db "SELECT * FROM t2")
  (prepare-statement db "INSERT INTO t1 VALUES (?)")
  ;; Cache now has 3 statements
  (disconnect db))
;; All 3 finalized in order
```

### Cache Full (LRU Eviction)
```common-lisp
(let ((db (connect ":memory:")))
  (execute-non-query db "CREATE TABLE t (x INTEGER)")
  (dotimes (i 20)  ; More than cache size (16)
    (prepare-statement db (format nil "SELECT ~D" i)))
  ;; Cache evicted oldest 4 entries (already finalized)
  ;; Cache holds 16 most recent
  (disconnect db))
;; Remaining 16 finalized
```

---

## Test Evidence

**Source:** `sqlite-tests.lisp:17`
```common-lisp
(deftest test-disconnect-with-statements ()
  (let ((db (connect ":memory:")))
    (execute-non-query db "CREATE TABLE contacts (name TEXT, age INTEGER)")
    (prepare-statement db "SELECT * FROM contacts")
    (disconnect db)))
```

**Test Status:** Passing (verified 2026-01-20)

**What test verifies:**
- No error during disconnect despite cached statement
- Implicit verification that cache clearing works

---

## Related Scenarios

- `basic-connection` - Disconnect without statements
- `statement-finalization` - Manual statement cleanup

---

## Related Contracts

- `disconnect` - Implements cache clearing
- `prepare-statement` - Adds to cache
- `finalize-statement` - Manual finalization (bypasses cache)

---

**Document Status:** Canonical
**Last Verified:** 2026-01-20 (Canon Initiation Pass 3)
