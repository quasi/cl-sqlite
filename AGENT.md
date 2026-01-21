# AGENT.md: CL-SQLite Project Specification

## Project Context

**Name:** cl-sqlite
**Language:** Common Lisp
**Build System:** ASDF
**Test Runner:** Prove (or similar)
**Target:** SQLite 3.x
**Status:** Production-ready

## Build Commands

```bash
# Load system
(asdf:load-system :sqlite)

# Run tests
(asdf:test-system :sqlite)

# Load with tests
(asdf:load-system :sqlite :force t)
```

## Code Organization

| Module | Purpose | Location |
|--------|---------|----------|
| **sqlite-ffi** | C API bindings | `sqlite-ffi.lisp` |
| **sqlite** | Core connection & statement APIs | `sqlite.lisp` |
| **sqlite.cache** | Prepared statement cache | `cache.lisp` |
| **sqlite.simple** | High-level CRUD API | `simple.lisp` |
| **sqlite.vec** | Vector extension API | `sqlite-vec.lisp` |

## Core Terminology

See `canon/core/foundation/vocabulary.md` for complete definitions.

| Term | Definition | Status |
|------|-----------|--------|
| **connection** | Active database session, type `sqlite-handle` | Stable |
| **statement** | Prepared SQL, type `sqlite-statement` | Stable |
| **binding** | Parameter-to-value association | Stable |
| **finalization** | Return statement to cache (NOT destruction) | Stable |
| **stepping** | Advance to next result row | Stable |
| **transaction** | Atomic BEGIN...COMMIT/ROLLBACK sequence | Stable |
| **cache** | MRU cache for statements (size: 16) | Stable |

## Type Mapping (RULE-007)

### Lisp → SQLite

| Lisp Type | SQLite Type | Notes |
|-----------|-------------|-------|
| `NIL` | `NULL` | Bidirectional |
| `INTEGER` | `INTEGER` | 64-bit signed |
| `DOUBLE-FLOAT` | `REAL` | IEEE 754 |
| `REAL` (other) | `REAL` | Coerced to double |
| `STRING` | `TEXT` | UTF-8 |
| `(VECTOR (UNSIGNED-BYTE 8))` | `BLOB` | Raw bytes |
| `(SIMPLE-ARRAY SINGLE-FLOAT)` | `BLOB` | IEEE 754 32-bit |

### SQLite → Lisp

| SQLite Type | Lisp Type | Function |
|-------------|-----------|----------|
| `NULL` | `NIL` | — |
| `INTEGER` | `INTEGER` | `sqlite3-column-int64` |
| `REAL` | `DOUBLE-FLOAT` | `sqlite3-column-double` |
| `TEXT` | `STRING` | `sqlite3-column-text` |
| `BLOB` | `(SIMPLE-ARRAY (UNSIGNED-BYTE 8))` | `sqlite3-column-blob` |

## Architecture Rules

### RULE-001: Single Statement Per Prepare

**Rule:** `prepare-statement` MUST accept only single SQL statements
**Applies to:** `prepare-statement` function
**Rationale:** SQLite API supports one statement per prepared object
**Violation consequence:** Error (multiple statements detected)
**Agent action:** Auto-fix forbidden; flag as error

### RULE-002: Column Index Base (0-based)

**Rule:** Column indices MUST be 0-based in results
**Applies to:** `statement-column-value`, `statement-column-name`
**Rationale:** SQLite C API uses 0-based indexing
**Violation consequence:** Index out of bounds error
**Agent action:** Flag if 1-based indices detected in user code

### RULE-003: Parameter Index Base (1-based)

**Rule:** Parameter indices MUST be 1-based when binding
**Applies to:** `bind-parameter`, `sqlite3-bind-*`
**Rationale:** SQLite C API uses 1-based parameter indexing
**Violation consequence:** Binding to wrong parameter
**Agent action:** Auto-fix forbidden; document clearly

### RULE-004: Cache Keying by SQL Text

**Rule:** Statement cache key MUST be exact SQL text string
**Applies to:** `prepare-statement` cache lookup
**Rationale:** Different SQL → different cache entries
**Invariant:** `(prepare-statement db sql₁) = (prepare-statement db sql₂) ⟺ sql₁ = sql₂` (INV-003)
**Agent action:** Enforce in implementation

### RULE-005: No Transaction Nesting

**Rule:** `with-transaction` MUST NOT nest
**Applies to:** `with-transaction` macro
**Rationale:** SQLite does not support nested transactions
**Violation consequence:** SQLite error: "cannot start transaction"
**Agent action:** Flag as error if attempted

### RULE-006: Extension Loading Sequence

**Rule:** `enable-load-extension` MUST precede `load-extension`
**Applies to:** Vector API and other extensions
**Sequence:** `enable-load-extension` → `load-extension` → use extension
**Violation consequence:** "not authorized" error from SQLite
**Agent action:** Flag if sequence violated

### RULE-007: Type Conversion at Boundaries

**Rule:** Type conversion MUST occur at SQLite boundary, not in Lisp code
**Applies to:** All binding and result extraction
**Rationale:** Centralized type mapping prevents inconsistencies
**Functions:** `bind-parameter`, `statement-column-value`, result extraction
**Agent action:** Ensure conversion functions used

### RULE-008: Statement Finalization Returns to Cache

**Rule:** `finalize-statement` MUST return statement to cache, NOT destroy
**Applies to:** `finalize-statement` function
**Rationale:** Enables statement reuse for performance
**Violation consequence:** Cache miss, re-parsing cost
**Agent action:** Use `really-finalize-statement` only for true destruction
**Important:** This violates principle of least surprise; document clearly

### RULE-009: WHERE Clause Operators Exhaustive

**Rule:** `compile-where` MUST validate operator is in known set
**Applies to:** Simplified API WHERE compilation
**Known operators:** `:=`, `:<`, `:>`, `:<=`, `:>=`, `:<>`, `:like`, `:in`, `:is-null`, `:is-not-null`, `:and`, `:or`, `:not`
**Violation consequence:** Error with unknown operator name
**Agent action:** Auto-fix forbidden; flag as error

### RULE-010: Name Normalization (Lowercase)

**Rule:** Table and column names MUST be normalized to lowercase
**Applies to:** Simplified API functions, `normalize-name`
**Rationale:** SQLite case-insensitive, lowercase is canonical
**Function:** `(string-downcase (string name))`
**Agent action:** Apply normalization in all simplified API functions

## State Machines

### Connection Lifecycle

```
disconnected --[connect]--> connected --[disconnect]--> closed
                                ↓ ↑
                         (execute/prepare)
```

**Invariant (INV-001):** `(slot-boundp handle 'handle) ⟺ (connected)`

**Transitions:**
- `disconnected → connected`: `connect` succeeds, slot bound
- `connected → connected`: Operations (execute, prepare)
- `connected → closed`: `disconnect` called, slot unbound
- `closed → (error)`: Any operation on closed connection

**Agent rule:** Check connection state before operations; signal error if unbound

### Statement Lifecycle

```
unprepared --[prepare-statement]--> prepared --[bind-parameter]--> bound
                   ↑                                                    ↓
                [cache hit]                                      [step-statement→T]
                   ↑                                                    ↓
                cached <--[finalize-statement]-- done <--[step→NIL]-- stepping
                                                   ↑
                                          [reset-statement]
```

**Invariants:**
- **INV-005:** DONE state is sticky (stepping after NIL continues NIL)
- **INV-004:** Rebinding overwrites previous binding

**Agent rule:** Verify statement transitions follow this machine

## Invariants

### INV-001: Connection State Consistency

**Statement:** Connection is active ⟺ handle slot is bound

**Check:** `(slot-boundp handle 'handle) ⟺ (can-use handle)`

**Consequence:** Any operation on unbound handle signals error

### INV-002: Transaction Atomicity

**Statement:** Transaction body completes → COMMIT; body signals error → ROLLBACK

**Check:** Success flag set after body, cleanup uses flag

**Mechanism:** `unwind-protect` with success flag

### INV-003: Statement Cache Purity

**Statement:** Same SQL → same cached statement

**Formula:** `prepare-statement(db, sql₁) = prepare-statement(db, sql₂) ⟺ sql₁ = sql₂`

**Check:** Cache keyed by exact SQL text string

### INV-004: Parameter Binding Idempotence

**Statement:** Rebinding same parameter overwrites (no append)

**Check:** Second `bind-parameter` replaces first value

### INV-005: DONE State Stickiness

**Statement:** Once `step-statement` returns NIL (DONE), subsequent steps continue returning NIL

**Check:** sqlite3 state machine, not Lisp logic

### INV-006: Type Round-Trip Preservation

**Statement:** Values survive round-trip: Lisp → SQLite → Lisp with type preserved

**Check:** Type mapping tables (RULE-007)

**Special cases:**
- `REAL` (non-double) → coerced to `double-float`
- Float vectors → encoded as 32-bit IEEE 754

## Error Handling

### Error Hierarchy

```
simple-error
  └── sqlite-error
        └── sqlite-constraint-error
```

**sqlite-error slots:** `handle`, `error-code`, `error-msg`, `statement`, `sql`

**Agent rule:** All SQLite errors MUST include error context

### Common Error Codes

| Code | Name | Meaning | Recovery |
|------|------|---------|----------|
| 1 | SQLITE_ERROR | Generic error | Check error message |
| 2 | SQLITE_INTERNAL | Internal error | Report bug |
| 3 | SQLITE_PERM | Permission denied | Check file permissions |
| 4 | SQLITE_ABORT | Aborted | Retry transaction |
| 5 | SQLITE_BUSY | Database locked | Retry or increase timeout |
| 6 | SQLITE_LOCKED | Table locked | Wait and retry |
| 19 | SQLITE_CONSTRAINT | Constraint violation | Check data |

## Cache Behavior

**Type:** MRU (Most Recently Used)
**Size:** 16 entries (fixed)
**Key:** Exact SQL text string
**Destructor:** `really-finalize-statement`

**Eviction:** When full, least recently used entry removed and destructed

**Agent behavior:**
- MUST treat cache as transparent
- MUST NOT assume cache size
- MUST document statement reuse for performance

## Performance Properties

### Statement Preparation

**Cost:** O(1) cache lookup + O(n) SQL parsing (first time)

**Cached:** O(1) lookup

**Optimization:** Cache hit rate determines overall performance

### Parameter Binding

**Cost:** O(1) per parameter

**Bulk binding:** Consider `bind-parameter` in loop

### Result Iteration

**Cost:** O(n) rows × O(m) columns

**Streaming:** Use prepared API for large result sets

## Extension API: Vector Search

### Prerequisite

- Extension loading must follow RULE-006
- Float vectors encoded as 32-bit IEEE 754 (4 bytes per float)

### Functions

```common-lisp
(create-vector-table db table-name :dim 384 :distance :cosine)
(vector-search db table-name query-vector :k 10)
(float-vector-to-blob vector) ; Lisp → SQLite
(blob-to-float-vector blob)   ; SQLite → Lisp
```

**Distance metrics:** `:l2` (Euclidean), `:cosine` (Cosine similarity)

## Code Style

### Naming

- Functions: kebab-case, verb-first (`prepare-statement`, `bind-parameter`)
- Variables: descriptive, lowercase (`stmt`, `bindings`, `col-index`)
- Constants: UPPERCASE_WITH_UNDERSCORES
- Types: UpperCamelCase (`SQLiteHandle`, `SQLiteStatement`)

### Structure

- FFI layer: Raw bindings, minimal Lisp logic
- Core layer: Connection, statement, transaction management
- API layers: High-level interfaces (simple, standard, prepared)
- Extensions: Orthogonal to core (vec-api, etc.)

### Comments

- FFI functions: Document C API equivalents and parameters
- State machine transitions: Document invariants
- Type conversions: Document rationale (RULE-007)

## File Locations

| Type | Location |
|------|----------|
| FFI bindings | `sqlite-ffi.lisp` |
| Core API | `sqlite.lisp` |
| Cache implementation | `cache.lisp` |
| Simplified API | `simple.lisp` |
| Vector extension | `sqlite-vec.lisp` |
| Tests | `t/` directory |
| Documentation | `docs/` and `canon/` |

## Testing

### Test Levels

- **Unit:** Individual functions (FFI, cache, type conversion)
- **Integration:** Full workflows (CRUD, transactions, extensions)
- **Scenario:** Complex multi-step operations

### Test Patterns

```common-lisp
;; FFI: Direct C binding verification
(= (sqlite3-libversion-number) expected-version)

;; Core: Connection state transitions
(with-open-database (db ":memory:")
  (is (connected-p db) "connection active")
  (disconnect db)
  (signals error (execute-single db "SELECT 1")))

;; Simplified API: CRUD operations
(with-open-database (db ":memory:")
  (create-table db :users '(...))
  (insert db :users '(:name "Alice"))
  (is (length (select db :users)) 1 "insert succeeded"))
```

## Known Limitations

### Cache Size Not Configurable

**Issue:** Fixed 16-entry cache
**Workaround:** Close and reopen connection to clear
**Mitigation:** 16 sufficient for most applications

### No Nested Transactions

**Issue:** SQLite doesn't support nesting
**Workaround:** Use savepoints manually or restructure code
**Mitigation:** Document clearly (RULE-005)

### No JOINs in Simplified API

**Issue:** Simplified API has no join syntax
**Workaround:** Use standard API for complex queries
**Mitigation:** Layered design allows mixing APIs

## References

**Canon specifications:**
- `canon/core/foundation/vocabulary.md` - Core terms
- `canon/core/foundation/ontology.md` - State machines & relationships
- `canon/core/decisions/` - Architecture decisions

**Code:**
- `sqlite.lisp` - Core implementation
- `simple.lisp` - Simplified API
- `cache.lisp` - Statement cache

**Documentation:**
- `docs/README.md` - User entry point
- `docs/quickstart.md` - Getting started
- `docs/reference/` - API reference

---

**Document Status:** Canonical
**Last Updated:** 2026-01-21
**Confidence:** 0.95 (extracted from Canon + code verification)
