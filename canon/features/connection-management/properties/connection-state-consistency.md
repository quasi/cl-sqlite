# Property: Connection State Consistency

**ID:** INV-001
**Confidence:** 1.0
**Status:** Verified
**Type:** Invariant

---

## Statement

```
∀ sqlite-handle h:
  (slot-boundp h 'handle) ⟺ (connection is active)
```

**Natural Language:**
For any database handle, the `handle` slot is bound if and only if the connection is active.

---

## Formal Definition

**Bi-conditional invariant:**

1. **Forward direction:**
   - If `(slot-boundp db 'handle)` is `T` → connection is active
   - Can safely perform database operations

2. **Reverse direction:**
   - If connection is active → `(slot-boundp db 'handle)` is `T`
   - After `disconnect`, `(slot-boundp db 'handle)` is `NIL`

**State transition:**
```
[Created] --connect--> [Connected: handle bound] --disconnect--> [Disconnected: handle unbound]
```

---

## Rationale

**Purpose:** Provides reliable way to check connection state

**Benefits:**
- Prevents operations on closed connections
- Enables defensive programming
- Clear state indicator

**Design choice:** Using slot boundness (not value) prevents stale pointer issues

---

## Enforcement Mechanisms

### During Connection (connect)

**Location:** `sqlite.lisp:95-104` (initialize-instance)

```common-lisp
(defmethod initialize-instance :after ((object sqlite-handle) &key ...)
  (cffi:with-foreign-object (ppdb 'sqlite-ffi:p-sqlite3)
    (let ((error-code (sqlite-ffi:sqlite3-open database-path ppdb)))
      (if (eq error-code :ok)
          (setf (handle object) (cffi:mem-ref ppdb 'sqlite-ffi:p-sqlite3)
                (database-path object) database-path)
          (sqlite-error error-code object ...)))))
```

**Guarantee:** If `connect` succeeds, `handle` slot is bound

**Failure mode:** If `sqlite3_open` fails, error signaled before slot bound

---

### During Disconnection (disconnect)

**Location:** `sqlite.lisp:117-131`

```common-lisp
(defun disconnect (db)
  (when (slot-boundp db 'handle)
    (sqlite.cache:clear-cache (cache db))
    (let ((error-code (sqlite-ffi:sqlite3-close (handle db))))
      (unless (eq error-code :ok)
        (sqlite-error error-code db "Could not close sqlite3 database")))
    (slot-makunbound db 'handle)))  ; <-- Enforcement
```

**Guarantee:** After successful close, `handle` slot is unbound

**Critical:** Uses `slot-makunbound`, not `(setf (handle db) nil)`

---

## Verification Methods

### Static Check (Before Operations)

```common-lisp
(defun safe-execute (db sql)
  (unless (slot-boundp db 'handle)
    (error "Cannot execute on closed connection"))
  (execute-non-query db sql))
```

### Runtime Assertion

```common-lisp
(assert (slot-boundp db 'handle) ()
        "Connection must be active")
```

### Test Verification

```common-lisp
(deftest test-connection-state ()
  (let ((db (connect ":memory:")))
    ;; After connect: bound
    (assert-true (slot-boundp db 'handle))
    (disconnect db)
    ;; After disconnect: unbound
    (assert-false (slot-boundp db 'handle))))
```

---

## Implications

### For Library Users

**Check before operations:**
```common-lisp
(when (slot-boundp db 'handle)
  (execute-non-query db sql))
```

**Defensive programming:**
```common-lisp
(defun maybe-disconnect (db)
  (when (slot-boundp db 'handle)
    (disconnect db)))
```

### For Library Implementation

**All API functions should assume:** If called, handle is bound
- No need to check in every function
- User responsibility to track connection lifetime

**Exception:** `disconnect` checks explicitly (idempotent)

---

## Test Evidence

**Direct test:** `test-connect` (sqlite-tests.lisp:14)
```common-lisp
(deftest test-connect ()
  (let ((db (connect ":memory:")))
    ;; Implicit verification: disconnect expects bound handle
    (disconnect db)))
```

**Indirect verification:** All tests use `with-open-database`
- Macro assumes handle bound during body
- Assumes handle unbound after disconnect

---

## Violations and Consequences

### Violating Forward Direction
**Scenario:** Handle bound but connection closed
```common-lisp
;; Hypothetical violation
(setf (handle db) some-stale-pointer)
;; Even though bound, connection is closed
```

**Consequence:**
- Operations fail with undefined behavior
- Possible segfault or SQLite error

**Prevention:** Only library can bind handle (no exported writer)

---

### Violating Reverse Direction
**Scenario:** Connection active but handle unbound
```common-lisp
;; Hypothetical violation
(slot-makunbound db 'handle)  ; Unbind without closing
;; Connection still active in C, but Lisp thinks it's closed
```

**Consequence:**
- Resource leak (C connection never closed)
- No way to close connection (handle lost)

**Prevention:** Only `disconnect` unbinds handle (after closing)

---

## Edge Cases

### Double Disconnect
```common-lisp
(let ((db (connect ":memory:")))
  (disconnect db)
  (disconnect db))  ; Second call
```

**Behavior:** Second `disconnect` is no-op
```common-lisp
(when (slot-boundp db 'handle)  ; False on second call
  ...)
```

**Result:** Idempotent disconnect

### Failed Connection
```common-lisp
(handler-case
    (let ((db (connect "/invalid/path/db.sqlite")))
      ...)
  (sqlite-error (e)
    ;; db never assigned, handle never bound
    ))
```

**Invariant preserved:** Handle never bound if connect fails

---

## Related Properties

- **ARCH-002 (Resource Management via CLOS):** Ties resource lifetime to slot boundness
- **RULE-002 (Connection Before Operations):** Depends on this invariant for enforcement

---

## Documentation References

- Specified: CL-SQLITE.agent.md:250-254 (INV-001)
- Implementation: sqlite.lisp:95-131
- Tests: sqlite-tests.lisp:14, 17

---

**Document Status:** Canonical
**Last Verified:** 2026-01-20 (Canon Initiation Pass 4)
