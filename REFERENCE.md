# CL-SQLITE Reference Manual

This document provides a comprehensive reference for the `cl-sqlite` library. It covers the public API in the `sqlite` package and the low-level bindings in the `sqlite-ffi` package.

## Package `sqlite`

The `sqlite` package provides the high-level idiomatic Common Lisp interface to SQLite.

### Connection Management

#### `connect`
**Syntax:** `(connect database-path &key busy-timeout)`
**Returns:** `sqlite-handle`

Connects to the SQLite database at the given `database-path`.
- `database-path`: A string or pathname specifying the database file. Use `":memory:"` to create a private, temporary in-memory database.
- `busy-timeout`: (Optional) The maximum amount of time (in milliseconds) to wait for a locked database before returning an error. If `NIL`, operations on locked databases fail immediately.

#### `disconnect`
**Syntax:** `(disconnect handle)`
**Returns:** `nil`

Disconnects the given `handle` from the database. It ensures that all non-finalized statements associated with the handle are finalized before closing the connection. All further operations on the handle are invalid.

#### `with-open-database`
**Syntax:** `(with-open-database (db-var path &key busy-timeout) &body body)`

A macro that executes `body` with `db-var` bound to a new database connection to `path`. It ensures that the database is properly disconnected (via `disconnect`) after the body finishes or is interrupted.
- `db-var`: The variable name to bind the connection handle to.
- `path`: The path to the database.
- `busy-timeout`: Passed to `connect`.

#### `sqlite-handle`
**Class**

The class representing a connection to an SQLite database.
- **Accessors:**
  - `handle`: (Reader) Returns the underlying `sqlite-ffi:p-sqlite3` pointer.

#### `set-busy-timeout`
**Syntax:** `(set-busy-timeout db milliseconds)`
**Returns:** `error-code` (usually `:OK`)

Sets the maximum amount of time (in milliseconds) the library will wait for a locked database when executing a statement on the connection `db`.

---

### Query Execution (High-Level)

These functions allow executing SQL queries without manually managing prepared statement objects. They handle preparation, binding, execution, and finalization (returning to cache).

**Parameter Binding:**
All `execute-*` functions accept parameters to bind to the query placeholders.
- **Positional Parameters:** Use `?` in SQL. Arguments are bound in order.
- **Named Parameters:** Use `:name`, `@name`, or `$name` in SQL. Use the `/named` variants of functions and provide arguments as alternating name/value pairs (e.g., `":id" 1`).

**Supported Data Types:**
- `NULL`: Passed as `NIL`.
- `INTEGER`: Passed as 64-bit integer.
- `REAL/FLOAT`: Passed as double-float.
- `STRING`: Passed as text.
- `(VECTOR (UNSIGNED-BYTE 8))`: Passed as BLOB.

#### `execute-non-query`
**Syntax:** `(execute-non-query db sql &rest parameters)`
**Returns:** `nil`

Executes a statement that does not return rows (e.g., `INSERT`, `UPDATE`, `CREATE`).
- `db`: The database connection.
- `sql`: The SQL string.
- `parameters`: Values for positional placeholders.

#### `execute-non-query/named`
**Syntax:** `(execute-non-query/named db sql &rest parameters)`
**Returns:** `nil`

Same as `execute-non-query`, but parameters are alternating names and values.

#### `execute-single`
**Syntax:** `(execute-single db sql &rest parameters)`
**Returns:** `value` or `nil`

Executes a query and returns the value of the first column of the first row. Returns `nil` if no rows are returned or the value is NULL.

#### `execute-single/named`
**Syntax:** `(execute-single/named db sql &rest parameters)`
**Returns:** `value` or `nil`

Same as `execute-single`, but parameters are alternating names and values.

#### `execute-one-row-m-v`
**Syntax:** `(execute-one-row-m-v db sql &rest parameters)`
**Returns:** `(values col1 col2 ...)`

Executes a query and returns the values of the first row as multiple values. If no rows are returned, returns multiple `nil`s corresponding to the column count.

#### `execute-one-row-m-v/named`
**Syntax:** `(execute-one-row-m-v/named db sql &rest parameters)`
**Returns:** `(values col1 col2 ...)`

Same as `execute-one-row-m-v`, but parameters are alternating names and values.

#### `execute-to-list`
**Syntax:** `(execute-to-list db sql &rest parameters)`
**Returns:** `((col1 col2 ...) ...)`

Executes a query and returns the entire result set as a list of lists. Each inner list represents a row.

#### `execute-to-list/named`
**Syntax:** `(execute-to-list/named db sql &rest parameters)`
**Returns:** `((col1 col2 ...) ...)`

Same as `execute-to-list`, but parameters are alternating names and values.

#### `last-insert-rowid`
**Syntax:** `(last-insert-rowid db)`
**Returns:** `integer`

Returns the auto-generated ID of the last inserted row on the database connection `db`.

---

### Prepared Statements

For repeated execution of the same SQL or fine-grained control, use prepared statements.

#### `prepare-statement`
**Syntax:** `(prepare-statement db sql)`
**Returns:** `sqlite-statement`

Prepares an SQL statement. The `sql` string must contain exactly one statement. Statements are cached internally by the connection.

#### `sqlite-statement`
**Class**

Represents a prepared statement.
- **Accessors:**
  - `statement-column-names`: Returns a list of strings representing the column names in the result set.
  - `statement-bind-parameter-names`: Returns a list of strings representing the names of bind parameters (or `nil` for unnamed ones).

#### `finalize-statement`
**Syntax:** `(finalize-statement statement)`
**Returns:** `nil`

Finalizes the statement. In `cl-sqlite`, this resets the statement and returns it to the cache for reuse, rather than destroying it immediately.

#### `step-statement`
**Syntax:** `(step-statement statement)`
**Returns:** `boolean`

Steps to the next row of the result set.
- Returns `T` if a new row is available.
- Returns `NIL` if execution is complete (no more rows).

#### `reset-statement`
**Syntax:** `(reset-statement statement)`
**Returns:** `nil`

Resets the statement to its initial state, allowing it to be executed again. Does not clear bindings.

#### `clear-statement-bindings`
**Syntax:** `(clear-statement-bindings statement)`
**Returns:** `nil`

Sets all parameters in the statement to NULL.

#### `bind-parameter`
**Syntax:** `(bind-parameter statement parameter value)`
**Returns:** `nil`

Binds a value to a parameter in the prepared statement.
- `parameter`: Can be an integer index (1-based) or a string (parameter name, e.g., `":id"`).
- `value`: The value to bind (integer, float, string, byte-vector, or nil).

#### `statement-column-value`
**Syntax:** `(statement-column-value statement column-number)`
**Returns:** `value`

Retrieves the value of the column at `column-number` (0-based) for the current row.

#### `with-prepared-statement`
**Syntax:** `(with-prepared-statement statement-var (db sql parameters-var) &body body)`

A macro that handles preparation, binding, execution, and finalization of a statement.
- `statement-var`: Variable bound to the prepared statement.
- `db`: Database connection.
- `sql`: SQL string.
- `parameters-var`: A list of values for positional parameters.
- The statement is automatically finalized (returned to cache) after `body`.

#### `with-prepared-statement/named`
**Syntax:** `(with-prepared-statement/named statement-var (db sql parameters-var) &body body)`

Same as `with-prepared-statement`, but `parameters-var` must be a list of alternating parameter names and values.

---

### Transactions

#### `with-transaction`
**Syntax:** `(with-transaction db &body body)`

Wraps `body` inside a transaction (`BEGIN TRANSACTION` ... `COMMIT`/`ROLLBACK`).
- If `body` completes successfully, the transaction is committed.
- If `body` signals an error or is interrupted, the transaction is rolled back.

---

### Iterate Integration

`cl-sqlite` integrates with the `iterate` library to allow iterating over query results.

#### `in-sqlite-query`
**Usage:** `(iter (for (var1 var2 ...) in-sqlite-query sql-string on-database db [with-parameters params]))`

Iterates over the rows returned by `sql-string`.
- `var1, var2...`: Variables to bind to column values.
- `params`: A list of values for positional parameters.

#### `in-sqlite-query/named`
**Usage:** `(iter (for (var1 var2 ...) in-sqlite-query/named sql-string on-database db [with-parameters params]))`

Same as `in-sqlite-query`, but `params` is a list of alternating names and values.

#### `on-sqlite-statement`
**Usage:** `(iter (for (var1 var2 ...) on-sqlite-statement statement))`

Iterates over the rows of an already prepared (and bound) `statement`.

---

### Error Handling

#### `sqlite-error`
**Condition**

The base condition for all SQLite errors.

**Readers:**
- `sqlite-error-code`: Returns the SQLite error code keyword (e.g., `:ERROR`, `:CONSTRAINT`).
- `sqlite-error-message`: Returns the error message string from SQLite.
- `sqlite-error-db-handle`: Returns the database handle involved.
- `sqlite-error-sql`: Returns the SQL that caused the error (if available).
- `sqlite-error-statement`: Returns the statement object (if available).

#### `sqlite-constraint-error`
**Condition**

A subclass of `sqlite-error` specifically for constraint violations (error code `:CONSTRAINT`).

---

## Package `sqlite-ffi`

The `sqlite-ffi` package contains the direct CFFI bindings to the SQLite C API. These symbols are direct mappings to C functions.

**Note:** For detailed documentation on these functions, refer to the [SQLite C Interface Documentation](https://www.sqlite.org/c3ref/intro.html).

### Types
- `p-sqlite3`
- `p-sqlite3-stmt`
- `p-sqlite3-context`
- `p-sqlite3-value`
- `error-code` (Enum: `:OK`, `:ROW`, `:DONE`, etc.)

### Core Functions
- `sqlite3-open`, `sqlite3-open-v2`, `sqlite3-close`
- `sqlite3-errmsg`
- `sqlite3-busy-timeout`
- `sqlite3-prepare`
- `sqlite3-step`, `sqlite3-reset`, `sqlite3-finalize`
- `sqlite3-exec`, `sqlite3-get-table`, `sqlite3-free-table`

### Columns & Values
- `sqlite3-column-count`
- `sqlite3-column-type`
- `sqlite3-column-text`, `sqlite3-column-int`, `sqlite3-column-int64`, `sqlite3-column-double`, `sqlite3-column-blob`
- `sqlite3-column-bytes`, `sqlite3-column-name`
- `sqlite3-column-value`
- `sqlite3-value-*` functions (blob, bytes, double, int, int64, text, type, numeric-type)

### Binding
- `sqlite3-bind-parameter-count`, `sqlite3-bind-parameter-index`, `sqlite3-bind-parameter-name`
- `sqlite3-bind-text`, `sqlite3-bind-int64`, `sqlite3-bind-double`, `sqlite3-bind-blob`, `sqlite3-bind-null`
- `sqlite3-bind-zeroblob`, `sqlite3-bind-value`
- `sqlite3-clear-bindings`

### Custom Functions & Extensions
- `sqlite3-create-function`, `sqlite3-create-function-v2`
- `sqlite3-create-collation`
- `sqlite3-result-*` functions (blob, double, error, int, int64, null, text, value, zeroblob)
- `sqlite3-user-data`, `sqlite3-aggregate-context`

### Miscellaneous
- `sqlite3-last-insert-rowid`
- `sqlite3-limit`
