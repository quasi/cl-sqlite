# Core Vocabulary

**Confidence:** 0.95
**Source:** Multi-source triangulation (docs, code, tests)
**Status:** Stable

This document defines the core terminology used throughout the cl-sqlite specification.

---

## Primary Entities

### connection
**Type:** `sqlite-handle`
**Definition:** An instance representing an active database session.
**Lifecycle:** Created by `connect`, destroyed by `disconnect`
**Invariant:** `(slot-boundp handle 'handle) ⟺ (connection is active)` (INV-001)
**Source:** CL-SQLITE.agent.md:23

**Properties:**
- Has associated cache (size 16)
- Has list of active statements
- Encapsulates foreign pointer to sqlite3 database

### statement
**Type:** `sqlite-statement`
**Definition:** An instance representing prepared SQL.
**Lifecycle:** Created by `prepare-statement`, returned to cache by `finalize-statement`
**Source:** CL-SQLITE.agent.md:24

**Properties:**
- May be cached (keyed by SQL text)
- Has column metadata (names, count)
- Has parameter metadata (names, count)
- Encapsulates foreign pointer to sqlite3_stmt

### parameter
**Type:** `(or integer string)`
**Definition:** Placeholder in SQL for runtime values.
**Formats:**
- Positional: `?`
- Named: `:name`, `@name`, `$name`
**Source:** CL-SQLITE.agent.md:25

**Properties:**
- Parameters are 1-indexed (RULE-004)
- Can be bound by index or name
- Supports types: NULL, INTEGER, REAL, TEXT, BLOB

### binding
**Definition:** Association of parameter with value.
**Source:** CL-SQLITE.agent.md:26

**Properties:**
- Idempotent (rebinding overwrites, INV-004)
- Type conversion at boundary (RULE-007)
- Persists across `reset-statement`, cleared by `clear-statement-bindings`

### finalization
**Definition:** Returning statement to cache (NOT destruction).
**Source:** CL-SQLITE.agent.md:27
**Important:** Differs from typical resource finalization semantics.

**Properties:**
- `finalize-statement` → returns to cache
- `really-finalize-statement` → actual destruction
- Enables statement reuse (performance optimization)

### stepping
**Definition:** Advancing to next row in result set.
**Source:** CL-SQLITE.agent.md:28

**Properties:**
- Returns `T` if row available
- Returns `NIL` if no more rows (DONE state)
- DONE state is sticky (INV-005)

### transaction
**Definition:** Atomic BEGIN...COMMIT/ROLLBACK sequence.
**Source:** CL-SQLITE.agent.md:29

**Properties:**
- Managed by `with-transaction` macro
- Automatic commit on success, rollback on error (INV-002)
- Cannot be nested (RULE-005)

---

## API Layers

### simple-api
**Definition:** High-level functions using s-expressions and plists.
**Functions:** `insert`, `select`, `update-table`, `delete-from`, `create-table`, `drop-table`
**Source:** CL-SQLITE.agent.md:30

**Properties:**
- No raw SQL strings (SQL injection prevention)
- S-expression WHERE clauses
- Name normalization (lowercase)

### standard-api
**Definition:** Mid-level functions for SQL execution.
**Functions:** `execute-to-list`, `execute-single`, `execute-one-row-m-v`, `execute-non-query`, etc.
**Source:** CL-SQLITE.agent.md:31

**Properties:**
- Accepts SQL strings with parameters
- Both positional and named variants
- Automatic statement management

### prepared-api
**Definition:** Low-level statement management.
**Functions:** `prepare-statement`, `step-statement`, `bind-parameter`, `finalize-statement`, etc.
**Source:** CL-SQLITE.agent.md:32

**Properties:**
- Full control over statement lifecycle
- Manual binding and stepping
- Maximum performance (statement reuse)

### vec-api
**Definition:** Vector search extension functions.
**Functions:** `create-vector-table`, `vector-search`, `vec-distance-L2`, `vec-add`, etc.
**Source:** CL-SQLITE.agent.md:33

**Properties:**
- Requires sqlite-vec extension loaded
- Operates on float32 vectors
- K-nearest neighbor search

---

## Data Types

### Type Mapping (RULE-007)

**Lisp to SQLite:**

| Lisp Type | SQLite Type | Notes |
|-----------|-------------|-------|
| `NIL` | `NULL` | Bidirectional |
| `INTEGER` | `INTEGER` | 64-bit signed |
| `DOUBLE-FLOAT` | `REAL` | IEEE 754 double |
| `REAL` (other) | `REAL` | Coerced to double-float |
| `STRING` | `TEXT` | UTF-8 encoded |
| `(VECTOR (UNSIGNED-BYTE 8))` | `BLOB` | Raw bytes |
| `VECTOR` (generic) | `BLOB` | Elements coerced to 0-255 |

**SQLite to Lisp:**

| SQLite Type | Lisp Type | Function |
|-------------|-----------|----------|
| `NULL` | `NIL` | - |
| `INTEGER` | `INTEGER` | `sqlite3-column-int64` |
| `REAL` | `DOUBLE-FLOAT` | `sqlite3-column-double` |
| `TEXT` | `STRING` | `sqlite3-column-text` |
| `BLOB` | `(SIMPLE-ARRAY (UNSIGNED-BYTE 8))` | `sqlite3-column-blob` |

---

## State Concepts

### cache
**Definition:** MRU cache for prepared statements.
**Properties:**
- Fixed size: 16 entries
- Keyed by SQL text string (INV-003)
- Destructor: `really-finalize-statement`
**Source:** Inferred from code

### column-index
**Definition:** 0-based index into result set columns.
**Constraint:** `column-index ∈ [0, column-count - 1]` (RULE-004)
**Source:** SQLite C API convention

### parameter-index
**Definition:** 1-based index into statement parameters.
**Constraint:** `parameter-index ∈ [1, parameter-count]` (RULE-004)
**Source:** SQLite C API convention

---

## Error Types

### sqlite-error
**Parent:** `simple-error`
**Definition:** Base condition for all SQLite errors.
**Slots:** `handle`, `error-code`, `error-msg`, `statement`, `sql`
**Source:** sqlite.lisp:43

### sqlite-constraint-error
**Parent:** `sqlite-error`
**Definition:** Constraint violation (e.g., UNIQUE, NOT NULL, FOREIGN KEY).
**Triggered when:** `error-code = :CONSTRAINT`
**Source:** sqlite.lisp:55

---

## Operational Concepts

### normalize-name
**Definition:** Convert symbol/keyword/string to lowercase string.
**Function:** `(string-downcase (string name))`
**Used by:** Simple API (tables, columns)
**Source:** simple.lisp:8

### normalize-type
**Definition:** Convert symbol/keyword to uppercase string.
**Function:** `(string-upcase (string type))`
**Used by:** Simple API (column types)
**Source:** simple.lisp:5

### compile-where
**Definition:** Compile s-expression to SQL WHERE clause.
**Returns:** `(values sql-string param-list)`
**Operators:** `:and`, `:or`, `:not`, `:=`, `:<`, `:>`, `:<=`, `:>=`, `:<>`, `:like`, `:in`, `:is-null`, `:is-not-null`
**Source:** simple.lisp:38

---

## Vector Extension Concepts

### float-vector-to-blob
**Definition:** Convert Lisp float vector to SQLite blob (32-bit little-endian).
**Signature:** `(float-vector-to-blob (simple-array single-float (*))) => (vector (unsigned-byte 8))`
**Encoding:** 4 bytes per float (IEEE 754 single-precision)
**Source:** sqlite.lisp:153

### blob-to-float-vector
**Definition:** Convert SQLite blob to Lisp float vector.
**Signature:** `(blob-to-float-vector (vector (unsigned-byte 8))) => (simple-array single-float (*))`
**Constraint:** Blob length must be multiple of 4
**Source:** sqlite.lisp:164

### vector-search
**Definition:** K-nearest neighbor search on vec0 virtual table.
**Returns:** List of rows with distance metric.
**Ordering:** Results ordered by distance (ascending)
**Source:** sqlite-vec.lisp:41

---

## Confidence Metadata

| Term | Confidence | Source | Status |
|------|------------|--------|--------|
| connection | 0.95 | Multi-source | Stable |
| statement | 0.95 | Multi-source | Stable |
| parameter | 0.95 | Multi-source | Stable |
| binding | 0.95 | Multi-source | Stable |
| finalization | 0.98 | Code + docs | Stable |
| stepping | 0.95 | Multi-source | Stable |
| transaction | 0.90 | Docs + code | Stable |
| simple-api | 0.95 | Multi-source | Stable |
| standard-api | 0.95 | Multi-source | Stable |
| prepared-api | 0.95 | Multi-source | Stable |
| vec-api | 0.90 | Recent addition | Stable |

---

**Document Status:** Canonical
**Last Updated:** 2026-01-20
**Validation:** All terms verified across documentation, code, and tests
