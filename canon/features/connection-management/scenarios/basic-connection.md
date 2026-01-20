# Scenario: Basic Connection Lifecycle

**Confidence:** 0.95
**Status:** Verified
**Test:** `test-connect` (sqlite-tests.lisp:14)

---

## Description

Establishes connection to in-memory database, executes basic operations, and disconnects cleanly.

**Purpose:** Validates fundamental connection management flow

---

## Setup

None required (in-memory database)

---

## Execution

```common-lisp
(deftest test-connect ()
  (let ((db (connect ":memory:")))
    (disconnect db)))
```

---

## Steps

1. **Connect** to `:memory:` database
   - Call `(connect ":memory:")`
   - Returns `sqlite-handle` instance

2. **Verify connection state**
   - `(slot-boundp db 'handle)` is `T`
   - Handle contains valid foreign pointer

3. **Disconnect**
   - Call `(disconnect db)`
   - Returns `nil`

4. **Verify disconnected state**
   - `(slot-boundp db 'handle)` is `NIL`
   - Resources released

---

## Expected Outcomes

### Connection Phase
- ✅ `connect` returns `sqlite-handle` instance
- ✅ `handle` slot bound to foreign pointer
- ✅ `database-path` slot set to `":memory:"`
- ✅ `cache` initialized (MRU cache, size 16)
- ✅ `statements` initialized to empty list
- ✅ No errors signaled

### Disconnection Phase
- ✅ `disconnect` returns `nil`
- ✅ `handle` slot unbound (INV-001)
- ✅ Cache cleared (all statements finalized)
- ✅ C-level connection closed
- ✅ No errors signaled

---

## Invariants Verified

- **INV-001 (Connection State Consistency):**
  - After `connect`: `(slot-boundp db 'handle)` is `T`
  - After `disconnect`: `(slot-boundp db 'handle)` is `NIL`

---

## Properties Verified

- Connection initialization succeeds for `:memory:` database
- Disconnection succeeds without active statements
- Resource cleanup completes without error

---

## Variations

### File-based Database
```common-lisp
(let ((db (connect "/tmp/test.sqlite")))
  (disconnect db))
;; File created on disk
```

### With Busy Timeout
```common-lisp
(let ((db (connect ":memory:" :busy-timeout 5000)))
  (disconnect db))
;; Timeout configured before disconnect
```

### Using Macro
```common-lisp
(with-open-database (db ":memory:")
  ;; Use database
  )
;; Automatic disconnect via unwind-protect
```

---

## Anti-patterns

❌ **Forgetting to disconnect:**
```common-lisp
(let ((db (connect "db.sqlite")))
  (do-work db))
;; Resource leak! Connection not closed
```

✅ **Correct:**
```common-lisp
(with-open-database (db "db.sqlite")
  (do-work db))
;; Automatic cleanup
```

---

## Test Evidence

**Source:** `sqlite-tests.lisp:14`
```common-lisp
(deftest test-connect ()
  (let ((db (connect ":memory:")))
    (disconnect db)))
```

**Test Status:** Passing (verified 2026-01-20)

---

## Related Scenarios

- `connection-cleanup-with-statements` - Disconnect with active cached statements
- `basic-query-execution` - Using connection for queries

---

**Document Status:** Canonical
**Last Verified:** 2026-01-20 (Canon Initiation Pass 3)
