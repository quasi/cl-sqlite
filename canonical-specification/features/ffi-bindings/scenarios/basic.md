---
type: scenario
name: ffi-bindings-basic
feature: ffi-bindings
covers:
  - core
---

# ffi-bindings Basic Scenario

## Given
- A SQLite library loaded via FFI
- CFFI bindings defined for core SQLite functions
- Foreign type definitions for sqlite3 database and statement handles

## When
- Opening a database connection using sqlite3-open
- Preparing SQL statements with bind parameters
- Binding parameters using positional (?) or named ($var) placeholders
- Stepping through query results
- Retrieving column values and metadata
- Finalizing statements and closing connections

## Then
- Database handles are properly managed as foreign pointers
- Statements can be prepared, bound, executed, and finalized
- Parameter binding works with both positional and named parameters
- Column values are correctly retrieved with proper type conversion
- Statement metadata (column names, bind parameter names) is accessible
- Resources are properly cleaned up to avoid memory leaks
