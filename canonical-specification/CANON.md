# AGENT.md: Inquisitio Project Specification

## Project Context

**Name:** inquisitio (formerly cl-sqlite)
**Language:** Common Lisp
**Build System:** ASDF
**Test Runner:** FiveAM
**Target:** SQLite 3.x
**License:** MIT
**Status:** Production-ready

## Build Commands

```lisp
;; Load system
(asdf:load-system :inquisitio)

;; Run tests
(asdf:test-system :inquisitio)

;; Backward compat (still works)
(asdf:load-system :sqlite)
```

## Code Organization

| Module | Purpose | Location |
|--------|---------|----------|
| **inquisitio.ffi** | C API bindings | `ffi.lisp` |
| **inquisitio** | Core connection & statement APIs | `core.lisp` |
| **inquisitio.cache** | Prepared statement cache | `cache.lisp` |
| **inquisitio (simplified)** | High-level CRUD API | `simplified.lisp` |
| **inquisitio (vec)** | Vector extension API | `vec.lisp` |
| **telos features** | Intent tracking hierarchy | `features.lisp` |

### Package Nicknames (Backward Compatibility)

| New Package | Nickname |
|-------------|----------|
| `:inquisitio` | `:sqlite` |
| `:inquisitio.ffi` | `:sqlite-ffi` |
| `:inquisitio.cache` | `:sqlite.cache` |
| `:inquisitio-tests` | `:sqlite-tests` |

## Core Terminology

| Term | Definition | Status |
|------|-----------|--------|
| **connection** | Active database session, type `sqlite-handle` | Stable |
| **statement** | Prepared SQL, type `sqlite-statement` | Stable |
| **binding** | Parameter-to-value association | Stable |
| **finalization** | Return statement to cache (NOT destruction) | Stable |
| **stepping** | Advance to next result row | Stable |
| **transaction** | Atomic BEGIN...COMMIT/ROLLBACK sequence | Stable |
| **cache** | MRU cache for statements (size: 16) | Stable |

## Type Mapping

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
`prepare-statement` MUST accept only single SQL statements.

### RULE-002: Column Index Base (0-based)
Column indices MUST be 0-based in results.

### RULE-003: Parameter Index Base (1-based)
Parameter indices MUST be 1-based when binding.

### RULE-004: Cache Keying by SQL Text
Statement cache key MUST be exact SQL text string.

### RULE-005: No Transaction Nesting
`with-transaction` MUST NOT nest.

### RULE-006: Extension Loading Sequence
`enable-load-extension` MUST precede `load-extension`.

### RULE-007: Type Conversion at Boundaries
Type conversion MUST occur at SQLite boundary, not in Lisp code.

### RULE-008: Statement Finalization Returns to Cache
`finalize-statement` returns to cache, NOT destroys.

### RULE-009: WHERE Clause Operators Exhaustive
`compile-where` validates operator is in known set: `:=`, `:<`, `:>`, `:<=`, `:>=`, `:<>`, `:like`, `:in`, `:is-null`, `:is-not-null`, `:and`, `:or`, `:not`.

### RULE-010: Name Normalization (Lowercase)
Table and column names MUST be normalized to lowercase via `normalize-name`.

## Error Handling

### Error Hierarchy

```
simple-error
  └── sqlite-error
        └── sqlite-constraint-error
```

**sqlite-error slots:** `handle`, `error-code`, `error-msg`, `statement`, `sql`

### Restarts

All execute functions provide `retry-query` and `skip-query` restarts.

## File Locations

| Type | Location |
|------|----------|
| Telos features | `features.lisp` |
| FFI bindings | `ffi.lisp` |
| Cache implementation | `cache.lisp` |
| Core API | `core.lisp` |
| Simplified API | `simplified.lisp` |
| Vector extension | `vec.lisp` |
| Tests | `tests/` directory |
| Example | `examples/vec-example.lisp` |
| Documentation | `docs/` |

## Testing

```lisp
;; Run all tests
(asdf:test-system :inquisitio)

;; Or directly
(5am:run! 'inquisitio-tests::inquisitio-suite)
```

Test files:
- `tests/core-tests.lisp` — Connection, statements, queries, iterate
- `tests/transaction-tests.lisp` — Transaction commit/rollback, blobs
- `tests/simplified-tests.lisp` — CRUD, WHERE compilation, input validation
- `tests/vec-tests.lisp` — Vector table creation, search, scalar functions

## Telos Integration

Features are defined in `features.lisp` and intent is tracked on key functions/classes/conditions:

```lisp
(telos:list-features)              ;; Show all features
(telos:feature-members 'inquisitio-core)  ;; Show members
(telos:intent-chain 'inquisitio:connect)  ;; Trace intent hierarchy
```

---

**Document Status:** Canonical
**Last Updated:** 2026-02-09
