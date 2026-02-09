---
type: contract
name: core-operations-core
version: 1.0.0
feature: core-operations
---

# Core Operations Contract

## Purpose
Provides essential database operations: connecting to databases, preparing SQL statements, binding parameters, executing queries, and iterating over results.

## API Functions

### Connection Management

#### `connect`
Opens a connection to a SQLite database.

**Parameters:**
```json
{
  "type": "object",
  "properties": {
    "database-path": {
      "type": "string",
      "description": "Path to database file or ':memory:' for in-memory database"
    }
  },
  "required": ["database-path"]
}
```

**Returns:** `sqlite-handle` - Active database connection

**Errors:**
- `sqlite-error` when database cannot be opened (invalid path, permissions)

#### `disconnect`
Closes a database connection and releases resources.

**Parameters:**
```json
{
  "type": "object",
  "properties": {
    "db": {
      "type": "object",
      "description": "sqlite-handle to close"
    }
  },
  "required": ["db"]
}
```

**Returns:** NIL

#### `with-open-database`
Macro providing automatic connection cleanup.

**Parameters:**
```json
{
  "type": "object",
  "properties": {
    "db-var": {
      "type": "symbol",
      "description": "Variable name for database handle"
    },
    "path": {
      "type": "string",
      "description": "Database file path"
    },
    "busy-timeout": {
      "type": "integer",
      "description": "Milliseconds to wait when database is locked"
    }
  },
  "required": ["db-var", "path"]
}
```

### Statement Lifecycle

#### `prepare-statement`
Compiles SQL text into a prepared statement.

**Parameters:**
```json
{
  "type": "object",
  "properties": {
    "db": {
      "type": "object",
      "description": "sqlite-handle"
    },
    "sql": {
      "type": "string",
      "description": "SQL statement text (single statement only)"
    }
  },
  "required": ["db", "sql"]
}
```

**Returns:** `sqlite-statement` - Prepared statement (may be from cache)

**Errors:**
- `sqlite-error` when SQL syntax is invalid

#### `step-statement`
Advances to the next result row.

**Returns:**
- `T` if row is available
- `NIL` if no more rows

#### `reset-statement`
Resets statement to initial state, preserving bindings.

#### `finalize-statement`
Returns statement to cache (does NOT destroy it).

### Parameter Binding

#### `bind-parameter`
Binds a value to a statement parameter.

**Parameters:**
```json
{
  "type": "object",
  "properties": {
    "statement": {
      "type": "object",
      "description": "sqlite-statement"
    },
    "parameter": {
      "oneOf": [
        {"type": "integer", "minimum": 1, "description": "1-based parameter index"},
        {"type": "string", "description": "Named parameter (e.g., ':name')"}
      ]
    },
    "value": {
      "description": "Value to bind (NIL, INTEGER, DOUBLE-FLOAT, STRING, or byte vector)"
    }
  },
  "required": ["statement", "parameter", "value"]
}
```

**Errors:**
- `sqlite-error` when parameter index/name is invalid

### Query Execution

#### `execute-non-query`
Executes SQL that returns no results (INSERT, UPDATE, DELETE).

**Parameters:**
```json
{
  "type": "object",
  "properties": {
    "db": {
      "type": "object",
      "description": "sqlite-handle"
    },
    "sql": {
      "type": "string"
    },
    "parameters": {
      "type": "array",
      "description": "Positional parameter values"
    }
  },
  "required": ["db", "sql"]
}
```

**Returns:** NIL

#### `execute-to-list`
Executes query and returns all rows as a list.

**Returns:** List of lists (each inner list is a row)

#### `execute-single`
Executes query expecting exactly one scalar result.

**Returns:** Single value

**Errors:**
- `sqlite-error` when query returns 0 or >1 rows

### Restarts

All execution functions provide:
- `retry-query` - Re-execute the query
- `skip-query` - Skip execution and return NIL

## Invariants

1. **RULE-001:** `prepare-statement` accepts only single SQL statements
2. **RULE-002:** Column indices are 0-based
3. **RULE-003:** Parameter indices are 1-based
4. **RULE-008:** `finalize-statement` returns to cache, does NOT destroy
