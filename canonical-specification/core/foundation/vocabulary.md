# Inquisitio Vocabulary

Canonical definitions of terms used throughout the specification.

## Core Terms

### Connection
Active database session, represented by `sqlite-handle` type. Maintains the link between the Common Lisp process and SQLite database file or in-memory database.

### Statement
Prepared SQL statement, represented by `sqlite-statement` type. Compiled SQL that can be executed multiple times with different parameters.

### Binding
Parameter-to-value association for prepared statements. Maps placeholders (positional `?` or named `:name`) to actual values.

### Finalization
The act of returning a prepared statement to the cache. NOT destruction - statements are reused from cache when possible.

### Stepping
Advancing to the next result row during query execution. Returns `T` if row available, `NIL` when no more rows.

### Transaction
Atomic BEGIN...COMMIT/ROLLBACK sequence ensuring all-or-nothing execution of a group of operations.

### Cache
Most-recently-used (MRU) cache for prepared statements with a fixed size of 16 entries. Avoids repeated compilation overhead for frequently used queries by caching and reusing prepared statements.

## Data Types

### NULL Representation
SQLite NULL bidirectionally mapped to Common Lisp `NIL`.

### INTEGER
64-bit signed integer, mapped between Common Lisp INTEGER and SQLite INTEGER types.

### REAL
IEEE 754 double-precision floating point, mapped to Common Lisp DOUBLE-FLOAT.

### TEXT
UTF-8 encoded string, mapped to Common Lisp STRING.

### BLOB
Raw byte sequence, mapped to `(VECTOR (UNSIGNED-BYTE 8))` in Common Lisp.

## Indexing Conventions

### Column Index Base
Column indices in result rows are 0-based (first column is index 0).

### Parameter Index Base
Parameter indices for binding are 1-based (first parameter is index 1).

## Processes

### Query Preparation
Process of compiling SQL text into a prepared statement via `prepare-statement`. Statement may be retrieved from cache if SQL text matches exactly.

### Parameter Binding
Process of associating values with statement parameters before execution. Supports positional (`?`) and named (`:name`, `@name`, `$name`) parameters.

### Extension Loading
Two-step process for loading SQLite extensions: first call `enable-load-extension` to allow loading, then call `load-extension` with path to shared library.

### Name Normalization
Process of converting table/column identifiers to lowercase via `normalize-name` for consistency and SQL injection prevention.
