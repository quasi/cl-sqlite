# Architecture Guide: Understanding CL-SQLite

This guide explains the design principles and architecture of CL-SQLite.

## Design Philosophy

CL-SQLite follows these core principles:

1. **Layered APIs:** Users choose their abstraction level
2. **Lisp First:** Syntax and idioms feel native to Lisp
3. **Type Safety:** Explicit type mappings at boundaries
4. **Transparency:** Users understand what the library does
5. **Performance:** Smart caching and statement reuse by default

## API Layers

CL-SQLite provides four API layers. Each builds on the previous, and you can mix them:

```
┌─────────────────────────────────────────────┐
│  Simplified API (High-level)                │  ← Choose this for
│  create-table, insert, select, etc.         │     simple CRUD
├─────────────────────────────────────────────┤
│  Standard API (Mid-level)                   │  ← Choose this for
│  execute-to-list, execute-single, etc.      │     complex queries
├─────────────────────────────────────────────┤
│  Prepared Statement API (Low-level)         │  ← Choose this for
│  prepare-statement, step-statement, etc.    │     performance
├─────────────────────────────────────────────┤
│  FFI Layer (Raw bindings)                   │  ← Choose this for
│  sqlite3-prepare-v2, sqlite3-step, etc.    │     maximum control
└─────────────────────────────────────────────┘
         SQLite C Library (System)
```

### When to Use Each Layer

| Task | API | Reason |
|------|-----|--------|
| Create table | Simplified | Concise, no SQL |
| Insert rows | Simplified | Safe (no SQL injection), composable |
| Simple WHERE | Simplified | Clear, Lispy syntax |
| Complex query (JOIN) | Standard | Simplified doesn't support |
| Repeated query | Prepared | Better performance (parse once) |
| Bulk operations | Prepared | Highest performance (streaming) |
| Special cases | FFI | Maximum control when needed |

### Mixing Layers

You can mix layers in the same connection:

```common-lisp
(with-open-database (db "/tmp/app.db")
  ;; High-level: Create table
  (create-table db :users '((:id :integer :primary-key)
                            (:name :text)))

  ;; Mid-level: Complex query
  (let ((result (execute-to-list db
                  "SELECT u.id FROM users u WHERE LENGTH(u.name) > ?"
                  5)))
    ...)

  ;; Low-level: Streaming large result
  (let ((stmt (prepare-statement db "SELECT * FROM users")))
    (loop while (step-statement stmt)
          collect (statement-column-value stmt 0))
    (finalize-statement stmt)))
```

## Connection Management

Connections are represented by `sqlite-handle` objects:

```common-lisp
(sqlite:with-open-database (db "/tmp/data.db")
  ;; db is active during this block
  (insert db :users '(:name "Alice"))
  ;; db is automatically closed when block exits
)
;; db is now closed and unusable
```

**Why `with-open-database`?**
- Ensures database is closed even if error occurs
- Prevents resource leaks
- Exception-safe (uses `unwind-protect`)

## Statement Lifecycle

Understanding how statements are managed helps you use the library correctly:

```
PREPARE
   ↓ (creates or retrieves from cache)
STATEMENT OBJECT
   ├─ If cached: cache lookup (O(1))
   └─ If new: parse SQL (O(n))

BIND PARAMETERS
   ↓
STEP (iterate results)
   ├─ Each step returns one row
   └─ Returns NIL when done

FINALIZE
   ├─ For high-level APIs: automatic
   └─ For low-level API: manual
```

## Statement Caching

CL-SQLite automatically caches prepared statements. This is transparent but important:

```common-lisp
;; First execution: Parse SQL (expensive)
(execute-to-list db "SELECT * FROM users WHERE id = ?" 1)

;; Second execution: Cache hit (cheap), reuses statement
(execute-to-list db "SELECT * FROM users WHERE id = ?" 2)

;; Different SQL: Cache miss, new parse
(execute-to-list db "SELECT * FROM users WHERE name = ?" "Alice")
```

**Cache size:** Fixed at 16 statements
**Cache key:** Exact SQL text string
**Cache eviction:** MRU (Most Recently Used) when full

**Implication:** Repeated queries with same SQL benefit from caching automatically

## Type Mapping

CL-SQLite maps between Lisp types and SQLite types automatically:

### Lisp → SQLite

```
NIL              → NULL
42               → INTEGER
3.14             → REAL
"hello"          → TEXT
#(1 2 3)         → BLOB
```

### SQLite → Lisp

```
NULL             → NIL
INTEGER          → integer
REAL             → double-float
TEXT             → string
BLOB             → (simple-array (unsigned-byte 8))
```

**Key point:** Type conversion happens at the boundary (when binding or extracting), not in Lisp code.

## Transaction Model

Transactions ensure atomicity: either all operations succeed, or none do:

```common-lisp
(with-transaction (db)
  (insert db :accounts '(:name "Alice" :balance 100))
  (insert db :accounts '(:name "Bob" :balance 100))
  (execute-non-query db "UPDATE accounts SET balance = balance - 10
                         WHERE name = 'Alice'")
  (execute-non-query db "UPDATE accounts SET balance = balance + 10
                         WHERE name = 'Bob'")
  ;; If any operation fails: entire transaction rolls back
  ;; If all succeed: automatically commits
)
```

**Important:** SQLite doesn't support nested transactions
- Solution: Use savepoints (manual) or restructure code

## Performance Considerations

### Statement Preparation is Expensive

```common-lisp
;; SLOW: Parses statement each time
(dotimes (i 1000)
  (execute-to-list db "SELECT * FROM users WHERE id = ?" i))

;; FASTER: Automatic cache (still re-parses first 16)
;; Already used by all APIs

;; FASTEST: Manual prepared statement + reuse
(let ((stmt (prepare-statement db "SELECT * FROM users WHERE id = ?")))
  (dotimes (i 1000)
    (bind-parameter stmt 1 i)
    (step-statement stmt)
    (statement-column-value stmt 0))
  (finalize-statement stmt))
```

### Data Type Conversions

Type conversions are necessary but not expensive. Avoid unnecessary conversions:

```common-lisp
;; Unnecessary: Convert string to integer, then query
(let ((user-input "123"))
  (parse-integer user-input)
  (execute-single db "SELECT * FROM users WHERE id = ?" 123))

;; Better: Bind as-is, let SQLite handle conversion
(execute-single db "SELECT * FROM users WHERE id = ?" user-input)
```

## Error Handling

Errors are signaled as Lisp conditions, not return codes:

```common-lisp
(handler-case
    (insert db :users '(:id 1 :name "Alice"))  ; Constraint violation
  (sqlite:sqlite-constraint-error (e)
    ;; Access error details
    (format t "Constraint violation: ~a~%" (sqlite:error-msg e)))
  (sqlite:sqlite-error (e)
    ;; Catch all other SQLite errors
    (format t "Database error: ~a~%" (sqlite:error-msg e))))
```

**Error hierarchy:**
```
sqlite-error (all SQLite errors inherit from this)
  └─ sqlite-constraint-error (constraint violations)
```

## Where Clause Compilation

The simplified API compiles s-expression WHERE clauses to SQL:

```common-lisp
;; Input (s-expression)
'(:and (:> :age 18) (:= :status "active"))

;; Compiled (SQL + parameters)
;; WHERE (age > ? AND status = ?)
;; Parameters: (18 "active")
```

**Compilation is pure:** Same input → Same SQL (deterministic)

**Implications:**
- WHERE clauses can be composed programmatically
- SQL generation is testable
- Safe from SQL injection (values are parameters)

## Extension Support

CL-SQLite supports SQLite extensions (like sqlite-vec for vector search):

```common-lisp
(with-open-database (db "/tmp/vectors.db")
  ;; Enable extension loading
  (enable-load-extension db)

  ;; Load extension
  (load-extension db "./vectors0")

  ;; Use extension (vector operations)
  (create-vector-table db :embeddings :dim 384)
  ...)
```

**Important:** `enable-load-extension` must precede `load-extension`

## Code Organization

```
sqlite-ffi.lisp          ← FFI bindings (raw C API)
    ↓ wraps
sqlite.lisp              ← Core (connections, statements)
    ├→ cache.lisp        ← Statement cache implementation
    ├→ simple.lisp       ← Simplified API (CRUD)
    └→ sqlite-vec.lisp   ← Vector extension (optional)
```

**Design principle:** Each layer wraps lower layers, no circular dependencies

## Resource Management

CL-SQLite uses Lisp's standard resource management patterns:

### Automatic (Macros)

```common-lisp
(with-open-database (db "/tmp/data.db")
  ;; Automatically closes db
  )

(with-transaction (db)
  ;; Automatically commits or rolls back
  )
```

### Manual (Functions)

```common-lisp
;; For maximum control
(let ((stmt (prepare-statement db "SELECT * FROM users")))
  ;; Use stmt...
  (finalize-statement stmt))  ; Explicit cleanup
```

**Pattern:** All resources follow `unwind-protect` model
- Exception-safe
- Guaranteed cleanup
- No resource leaks

## Testing Strategy

CL-SQLite tests follow these patterns:

1. **Unit tests:** Individual functions (FFI, cache, type conversion)
2. **Integration tests:** Full workflows (CRUD, transactions)
3. **Scenario tests:** Complex multi-step operations

## Known Limitations

### Cache Size Not Configurable

Fixed at 16 entries. If your application uses >16 different queries, oldest queries get re-parsed.

**Workaround:** Use prepared statements for frequently used queries

### No Nested Transactions

SQLite limitation, not CL-SQLite:

```common-lisp
(with-transaction (db)
  ;; This will fail:
  (with-transaction (db)
    ...))

;; Workaround: Use savepoints manually or restructure
```

### No JOINs in Simplified API

The simplified API doesn't support JOINs. Use standard API:

```common-lisp
;; Not supported:
;; (select db :users :join :posts ...)

;; Use standard API:
(execute-to-list db
  "SELECT u.name, p.title FROM users u JOIN posts p ON u.id = p.user_id")
```

## Architecture Decisions

See `canon/core/decisions/` for detailed rationale on:
- **ADR-0001:** Statement Caching (why 16? why MRU?)
- **ADR-0002:** Simplified Lispy API (why s-expressions?)
- **ADR-0003:** Vector Extension Support (how extensions work)
- **ADR-0004:** Agent Specification (how agents use this library)

---

**Status:** Complete
**Last Updated:** 2026-01-21
**Related:** [API Reference](../reference/), [Performance Guide](../advanced.md)
