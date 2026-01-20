# Contract: connect

**Signature:** `(connect database-path &key busy-timeout) => sqlite-handle`
**Confidence:** 0.95
**Status:** Stable

---

## Parameters

### database-path
**Type:** `(or string pathname)`
**Required:** Yes
**Description:** Path to SQLite database file or `:memory:` for in-memory database

**Constraints:**
- If pathname, normalized to string via `namestring`
- Special value `":memory:"` creates temporary in-memory database
- File created if doesn't exist (for file-based databases)

**Examples:**
```common-lisp
"/path/to/database.sqlite"   ; File database
(pathname "/path/to/db.sqlite")  ; Pathname (converted to string)
":memory:"                    ; In-memory database
```

### busy-timeout
**Type:** `(or null integer)`
**Required:** No
**Default:** `nil`
**Description:** Maximum milliseconds to wait for locked database

**Behavior:**
- If `nil`: Operations fail immediately when database locked (`:BUSY` error)
- If integer: Wait up to N milliseconds before returning `:BUSY` error
- Calls `sqlite3-busy-timeout` after connection established

**Recommended:** Set to non-nil value for concurrent access (RULE-009)

---

## Returns

**Type:** `sqlite-handle`
**Description:** Connected database handle

**Properties:**
- `handle` slot bound to foreign pointer (`sqlite3*`)
- `database-path` slot set to normalized path
- `cache` slot initialized with MRU cache (size 16)
- `statements` slot initialized to empty list

**Invariant:** `(slot-boundp handle 'handle)` is `T` (INV-001)

---

## Side Effects

- Allocates SQLite database connection (C-level resource)
- Creates file if file-based and doesn't exist
- Initializes statement cache (16 entries, MRU eviction)

---

## Errors

### sqlite-error
**When:** Cannot open database
**Error Code:** Varies (`:CANTOPEN`, `:PERM`, etc.)
**Message:** `"Could not open sqlite3 database ~A"` (with path)
**Context:** Includes `db-handle`, `error-code`, `error-msg`

**Common Causes:**
- File permissions (can't read/write)
- Directory doesn't exist
- Invalid path syntax
- Disk full (for file-based)

---

## Preconditions

- Parent directory exists (for file-based databases)
- Adequate permissions (read/write for file path)
- SQLite library available (libsqlite3.so, libsqlite3.dylib, sqlite3.dll)

---

## Postconditions

**Success:**
- `sqlite-handle` instance created
- `(slot-boundp handle 'handle)` is `T`
- Connection is active and usable
- Cache initialized and empty

**Failure:**
- `sqlite-error` signaled
- No resources allocated (no cleanup needed)

---

## Implementation

**Location:** `sqlite.lisp:106-115`

**Key Code:**
```common-lisp
(defun connect (database-path &key busy-timeout)
  (let ((db (make-instance 'sqlite-handle
                           :database-path (etypecase database-path
                                            (string database-path)
                                            (pathname (namestring database-path))))))
    (when busy-timeout
      (set-busy-timeout db busy-timeout))
    db))
```

**Initialization (via `initialize-instance`):**
```common-lisp
(defmethod initialize-instance :after ((object sqlite-handle) &key ...)
  (cffi:with-foreign-object (ppdb 'sqlite-ffi:p-sqlite3)
    (let ((error-code (sqlite-ffi:sqlite3-open database-path ppdb)))
      (if (eq error-code :ok)
          (setf (handle object) (cffi:mem-ref ppdb 'sqlite-ffi:p-sqlite3)
                (database-path object) database-path)
          (sqlite-error error-code ...))))
  (setf (cache object) (make-instance 'sqlite.cache:mru-cache
                                      :cache-size 16
                                      :destructor #'really-finalize-statement)))
```

---

## Usage Examples

### Basic File Database
```common-lisp
(defvar *db* (connect "/path/to/app.sqlite"))
;; ... use database ...
(disconnect *db*)
```

### In-Memory Database
```common-lisp
(defvar *mem-db* (connect ":memory:"))
;; ... use database (lost on disconnect) ...
(disconnect *mem-db*)
```

### With Busy Timeout
```common-lisp
(defvar *db* (connect "/shared/db.sqlite" :busy-timeout 5000))
;; Waits up to 5 seconds for locks
```

### With Pathname
```common-lisp
(defvar *db* (connect (pathname "~/databases/app.sqlite")))
;; Pathname normalized to string
```

---

## Related Contracts

- `disconnect` - Closes connection
- `with-open-database` - Scoped connection management
- `set-busy-timeout` - Configure timeout after connection

---

## Patterns

**Recommended:** Use `with-open-database` for automatic cleanup
```common-lisp
(with-open-database (db "/path/to/db.sqlite" :busy-timeout 5000)
  ;; ... use db ...
  )  ; Automatically disconnected
```

**Manual Management:** Only if connection lifetime spans scope
```common-lisp
(defvar *global-db* (connect ...))
;; ... use across multiple functions ...
(disconnect *global-db*)  ; When done
```

---

## Validation

**Tests:**
- `test-connect` (sqlite-tests.lisp:14): Basic connection succeeds
- `test-disconnect-with-statements` (sqlite-tests.lisp:17): Connection cleanup

**Documentation:**
- README.md:98-112
- REFERENCE.md:11-17
- CL-SQLITE.agent.md:872

**Verified Properties:**
- Connection state consistency (INV-001)
- Resource management (ARCH-002)

---

**Document Status:** Canonical
**Last Verified:** 2026-01-20 (Canon Initiation Pass 2)
