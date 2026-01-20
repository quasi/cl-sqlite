# Contract: disconnect

**Signature:** `(disconnect db) => nil`
**Confidence:** 0.95
**Status:** Stable

---

## Parameters

### db
**Type:** `sqlite-handle`
**Required:** Yes
**Description:** Connected database handle to close

**Constraints:**
- Must be valid `sqlite-handle` instance
- Connection must be active (slot-boundp check)

---

## Returns

**Type:** `nil`
**Description:** No meaningful return value

**Side Effects:**
- Closes SQLite connection (C-level resource freed)
- Finalizes all cached prepared statements
- Unbinds `handle` slot (marks connection as closed)
- Clears statement cache

---

## Side Effects

- Calls `sqlite3-close` on foreign pointer
- Finalizes all statements in cache
- Unbinds `handle` slot via `slot-makunbound`
- Releases C-level resources

---

## Errors

### sqlite-error
**When:** Cannot close database (e.g., unfinalised statements outside cache)
**Error Code:** `:BUSY` (most common)
**Message:** `"Could not close sqlite3 database"`
**Context:** Includes `db-handle`, `error-code`, `error-msg`

**Common Causes:**
- Active statements not finalized (outside cache)
- Transaction in progress
- Database locked by another thread

---

## Preconditions

- Database must be connected (`(slot-boundp db 'handle)` is `T`)
- All user-managed statements should be finalized

---

## Postconditions

**Success:**
- `(slot-boundp db 'handle)` is `NIL`
- All cached statements finalized
- C-level connection closed
- Resources released

**Failure:**
- `sqlite-error` signaled
- Connection may remain open (partial cleanup)

---

## Implementation

**Location:** `sqlite.lisp:117-131`

**Key Code:**
```common-lisp
(defun disconnect (db)
  (when (slot-boundp db 'handle)
    ;; Finalize all cached statements
    (sqlite.cache:clear-cache (cache db))

    ;; Close connection
    (let ((error-code (sqlite-ffi:sqlite3-close (handle db))))
      (unless (eq error-code :ok)
        (sqlite-error error-code db "Could not close sqlite3 database")))

    ;; Mark as disconnected
    (slot-makunbound db 'handle)))
```

**Key Mechanism:** Uses `clear-cache` to finalize all cached statements before closing

---

## Usage Examples

### Basic Disconnect
```common-lisp
(defvar *db* (connect "/path/to/db.sqlite"))
;; ... use database ...
(disconnect *db*)
;; *db* handle now unbound
```

### With Error Handling
```common-lisp
(let ((db (connect ":memory:")))
  (unwind-protect
      (progn
        ;; ... use database ...
        )
    (disconnect db)))  ; Ensure cleanup
```

### Preferred Pattern
```common-lisp
(with-open-database (db ":memory:")
  ;; ... use database ...
  )  ; Automatically disconnected
```

---

## Related Contracts

- `connect` - Establishes connection
- `with-open-database` - Automatic connection management
- `finalize-statement` - Manual statement cleanup

---

## Patterns

**Anti-pattern:** Manual disconnect for scoped connections
```common-lisp
;; DON'T
(let ((db (connect "db.sqlite")))
  (do-work db)
  (disconnect db))

;; DO
(with-open-database (db "db.sqlite")
  (do-work db))
```

**Correct:** Manual disconnect only for long-lived connections
```common-lisp
;; Application startup
(defvar *app-db* (connect "/var/app/database.sqlite"))

;; Application shutdown
(defun shutdown ()
  (disconnect *app-db*))
```

---

## Validation

**Tests:**
- `test-disconnect-with-statements` (sqlite-tests.lisp:17): Verifies statement finalization

**Documentation:**
- README.md:98-112
- REFERENCE.md:18-21
- CL-SQLITE.agent.md:872

**Verified Properties:**
- Connection state consistency (INV-001)
- Resource cleanup (ARCH-002)

---

**Document Status:** Canonical
**Last Verified:** 2026-01-20 (Canon Initiation Pass 2)
