# Property: Busy Timeout Configuration

**ID:** RULE-009
**Confidence:** 0.90
**Status:** Documented
**Type:** Normative Rule

---

## Statement

```
For concurrent database access, busy-timeout SHOULD be set to a non-nil value
to enable retry behavior on database locks instead of immediate failure.
```

**Keywords:**
- **SHOULD:** Strong recommendation, may be ignored with justification

---

## Rationale

**SQLite's default behavior:**
- When database is locked: return `:BUSY` error immediately
- No retry logic

**Problem for concurrent access:**
- Writes serialize (only one writer at a time)
- Without timeout: operations fail frequently
- Application must implement retry logic

**Solution:** `busy-timeout` enables SQLite-level retry
- SQLite waits up to N milliseconds before returning `:BUSY`
- Automatic retry with exponential backoff (SQLite internal)
- Simpler application code

---

## Configuration Methods

### Method 1: During Connection

```common-lisp
(connect "/path/to/db.sqlite" :busy-timeout 5000)
```

**Mechanism:** `connect` calls `set-busy-timeout` internally

**Location:** `sqlite.lisp:110-117`
```common-lisp
(defun connect (database-path &key busy-timeout)
  (let ((db (make-instance 'sqlite-handle
                           :database-path ...)))
    (when busy-timeout
      (set-busy-timeout db busy-timeout))
    db))
```

---

### Method 2: After Connection

```common-lisp
(let ((db (connect "/path/to/db.sqlite")))
  (set-busy-timeout db 5000)
  ...)
```

**Use case:** Change timeout dynamically

---

### Method 3: With Macro

```common-lisp
(with-open-database (db "/path/to/db.sqlite" :busy-timeout 5000)
  ...)
```

**Preferred:** Combines connection + configuration + cleanup

---

## Recommended Values

### Development
```common-lisp
:busy-timeout 5000  ; 5 seconds
```
**Rationale:** Fast feedback during development

---

### Production (Light Concurrency)
```common-lisp
:busy-timeout 10000  ; 10 seconds
```
**Rationale:** Tolerate occasional lock contention

---

### Production (Heavy Concurrency)
```common-lisp
:busy-timeout 30000  ; 30 seconds
```
**Rationale:** More retry attempts, higher success rate

---

### Batch Operations
```common-lisp
:busy-timeout 60000  ; 60 seconds or more
```
**Rationale:** Can afford to wait, eventual success preferred

---

### When to Use 0 (Disable)
```common-lisp
:busy-timeout 0  ; or omit (default is nil)
```

**Use cases:**
- Single-threaded application (no concurrency)
- In-memory database (no file locking)
- Read-only database (no write contention)
- Fail-fast preferred (application handles retry)

---

## Behavior

### With busy-timeout = nil (default)

```common-lisp
(with-open-database (db "/shared/db.sqlite")
  ;; Another process has write lock
  (insert db :logs '(:message "Event")))
;; ERROR: sqlite-error :BUSY "database is locked"
```

**Application must handle:**
```common-lisp
(loop repeat 10
      do (handler-case
             (insert db :logs '(:message "Event"))
           (sqlite-error (e)
             (if (eq (error-code e) :busy)
                 (sleep 0.1)
                 (error e)))))
```

---

### With busy-timeout = 5000

```common-lisp
(with-open-database (db "/shared/db.sqlite" :busy-timeout 5000)
  ;; Another process has write lock
  (insert db :logs '(:message "Event")))
;; SQLite retries internally for up to 5 seconds
;; Either: succeeds when lock released
;; Or: ERROR after 5 seconds if lock still held
```

**Application code simpler:**
```common-lisp
;; Just handle genuine errors
(handler-case
    (insert db :logs '(:message "Event"))
  (sqlite-error (e)
    (log-error e)))
```

---

## Implementation

**FFI Binding:** `sqlite.lisp:133-135`
```common-lisp
(defun set-busy-timeout (db milliseconds)
  (sqlite-ffi:sqlite3-busy-timeout (handle db) milliseconds))
```

**SQLite C API:**
```c
int sqlite3_busy_timeout(sqlite3 *db, int ms);
```

**Internal Mechanism:**
- Sets callback that sleeps and retries
- Retry interval increases (exponential backoff)
- Total time capped at `ms` milliseconds

---

## Verification Methods

### Test: Timeout Configured
```common-lisp
(deftest test-busy-timeout ()
  (let ((db (connect ":memory:" :busy-timeout 5000)))
    ;; No direct way to verify timeout value set
    ;; Trust FFI call succeeded
    (disconnect db)))
```

**Limitation:** No getter for current timeout value in SQLite API

---

### Integration Test: Concurrent Access
```common-lisp
(deftest test-concurrent-writes ()
  (let ((db1 (connect "shared.db" :busy-timeout 5000))
        (db2 (connect "shared.db" :busy-timeout 5000)))
    ;; db1 starts transaction
    (execute-non-query db1 "BEGIN EXCLUSIVE")
    ;; db2 tries to write (should wait, not fail immediately)
    (bt:make-thread
      (lambda ()
        (insert db2 :logs '(:message "From thread"))))
    ;; Release lock
    (execute-non-query db1 "COMMIT")
    ;; Thread's insert should succeed
    ...))
```

---

## Common Mistakes

### Mistake 1: Forgetting Timeout for Shared Databases

```common-lisp
;; BAD: Shared database without timeout
(with-open-database (db "/shared/app.db")
  (insert db :users '(:name "Alice")))
;; Likely to fail with :BUSY in production
```

```common-lisp
;; GOOD: Shared database with timeout
(with-open-database (db "/shared/app.db" :busy-timeout 10000)
  (insert db :users '(:name "Alice")))
```

---

### Mistake 2: Timeout Too Short

```common-lisp
;; BAD: 100ms timeout (very short)
(with-open-database (db "/shared/app.db" :busy-timeout 100)
  (insert db :users '(:name "Alice")))
;; Still likely to fail under moderate load
```

**Guideline:** Use at least 1000ms (1 second) for shared databases

---

### Mistake 3: Timeout for In-Memory Databases

```common-lisp
;; UNNECESSARY: In-memory has no file locking
(with-open-database (db ":memory:" :busy-timeout 5000)
  ...)
;; Harmless but pointless (no concurrency possible)
```

---

## Exceptions to Rule

**When NOT to use busy-timeout:**

1. **Single-user applications:** No concurrency, timeout unnecessary
2. **Read-only databases:** No write locks, timeout irrelevant
3. **In-memory databases:** No file system, no locking
4. **Explicit retry logic:** Application implements own retry mechanism

**Example (explicit retry):**
```common-lisp
(defun insert-with-retry (db table data &key (max-retries 10))
  (loop repeat max-retries
        do (handler-case
               (return (insert db table data))
             (sqlite-error (e)
               (unless (eq (error-code e) :busy)
                 (error e))
               (sleep (random 1.0))))))
```

---

## Related Rules

- **RULE-002 (Connection Before Operations):** Timeout configured on connection
- **INV-001 (Connection State Consistency):** Timeout persists while connected

---

## Documentation References

- Specification: CL-SQLITE.agent.md:237-242 (RULE-009)
- Implementation: sqlite.lisp:110-117, 133-135
- SQLite docs: https://www.sqlite.org/c3ref/busy_timeout.html

---

**Document Status:** Canonical
**Last Verified:** 2026-01-20 (Canon Initiation Pass 4)
