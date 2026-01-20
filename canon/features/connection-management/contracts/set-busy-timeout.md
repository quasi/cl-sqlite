# Contract: set-busy-timeout

**Signature:** `(set-busy-timeout db milliseconds) => nil`
**Confidence:** 0.90
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

### milliseconds
**Type:** `integer`
**Required:** Yes
**Description:** Timeout duration in milliseconds

**Constraints:**
- Non-negative integer
- `0` means no waiting (fail immediately on lock)
- Positive values enable retry behavior

**Recommended:** 5000-10000 for concurrent access scenarios (RULE-009)

---

## Returns

**Type:** `nil`
**Description:** No meaningful return value

**Side Effects:**
- Configures SQLite connection's busy timeout
- Affects all subsequent operations on this connection

---

## Behavior

**When database is locked:**
- `milliseconds = 0`: Operation fails immediately with `:BUSY` error
- `milliseconds > 0`: SQLite retries for up to N milliseconds before returning `:BUSY`

**Mechanism:** Calls `sqlite3_busy_timeout` C function

---

## Side Effects

- Modifies SQLite connection's internal busy handler
- Persists for lifetime of connection
- Affects all statements prepared on this connection

---

## Errors

None documented. SQLite's `sqlite3_busy_timeout` always returns `SQLITE_OK`.

---

## Preconditions

- Database must be connected (`(slot-boundp db 'handle)` is `T`)

---

## Postconditions

**Success:**
- Busy timeout configured
- Future lock conflicts will wait up to `milliseconds` before failing

---

## Implementation

**Location:** `sqlite.lisp:133-135`

**Key Code:**
```common-lisp
(defun set-busy-timeout (db milliseconds)
  (sqlite-ffi:sqlite3-busy-timeout (handle db) milliseconds))
```

**Direct FFI Call:** Thin wrapper over C function

---

## Usage Examples

### Configure After Connection
```common-lisp
(defvar *db* (connect "/shared/db.sqlite"))
(set-busy-timeout *db* 5000)  ; Wait up to 5 seconds
```

### Configure During Connection
```common-lisp
(defvar *db* (connect "/shared/db.sqlite" :busy-timeout 5000))
;; connect calls set-busy-timeout internally
```

### Disable Timeout
```common-lisp
(set-busy-timeout *db* 0)  ; Fail immediately on lock
```

### Recommended for Concurrent Access
```common-lisp
;; Multiple processes accessing same database
(with-open-database (db "/shared/db.sqlite" :busy-timeout 10000)
  ;; Operations will retry for up to 10 seconds if locked
  (insert db :logs '(:message "Event occurred")))
```

---

## Related Contracts

- `connect` - Can specify `:busy-timeout` during connection
- All query/execution functions - Affected by timeout setting

---

## Patterns

**When to use:**
- Concurrent access scenarios (multiple processes/threads)
- Write-heavy workloads with potential contention
- Shared databases

**When to skip:**
- Single-threaded applications
- Read-only databases
- In-memory databases (no file locking)

**Recommended values:**
- Development: 5000 (5 seconds)
- Production: 10000-30000 (10-30 seconds)
- Batch operations: 60000+ (1+ minute)

---

## Design Rationale

**Why configurable:**
- Different applications have different timeout tolerance
- Some prefer fail-fast (0ms), others prefer eventual success (long timeout)

**Why not enabled by default:**
- SQLite defaults to fail-immediately behavior
- cl-sqlite preserves this default (principle of least surprise)

**Why separate function:**
- Allows reconfiguration without reconnection
- Explicit opt-in (RULE-009)

---

## Validation

**Tests:**
- No dedicated test (simple FFI wrapper)
- Tested indirectly via concurrent access examples

**Documentation:**
- README.md:105-106
- REFERENCE.md:34-37
- CL-SQLITE.agent.md:874

**Verified Properties:**
- Busy timeout configuration (property documented)

---

**Document Status:** Canonical
**Last Verified:** 2026-01-20 (Canon Initiation Pass 2)
