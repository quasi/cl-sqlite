---
type: contract
name: ffi-bindings-core
version: 1.0.0
feature: ffi-bindings
---

# FFI Bindings Contract

## Purpose
Provides low-level CFFI bindings to SQLite C API. These are the raw FFI function wrappers used by higher-level abstractions.

## API Functions

### Database Connection

#### `sqlite3-open`
Opens a SQLite database connection.

**Parameters:**
```json
{
  "type": "object",
  "properties": {
    "filename": {
      "type": "string",
      "description": "Path to database file or ':memory:'"
    },
    "db": {
      "type": "pointer",
      "description": "Pointer to p-sqlite3 pointer (output parameter)"
    }
  },
  "required": ["filename", "db"]
}
```

**Returns:** `error-code` enum value (`:OK` on success)

**Errors:**
- `:CANTOPEN` when file cannot be opened
- `:NOMEM` when out of memory
- `:PERM` when permission denied

#### `sqlite3-close`
Closes a database connection.

**Parameters:**
```json
{
  "type": "object",
  "properties": {
    "db": {
      "type": "pointer",
      "description": "p-sqlite3 database handle"
    }
  },
  "required": ["db"]
}
```

**Returns:** `error-code` enum value

**Errors:**
- `:BUSY` when unfinalized statements exist

### Statement Lifecycle

#### `sqlite3-prepare`
Compiles SQL into a prepared statement (using v2 API).

**Parameters:**
```json
{
  "type": "object",
  "properties": {
    "db": {
      "type": "pointer",
      "description": "p-sqlite3 database handle"
    },
    "sql": {
      "type": "string",
      "description": "SQL text to compile"
    },
    "sql-length-bytes": {
      "type": "integer",
      "description": "Length of SQL in bytes (-1 for null-terminated)"
    },
    "stmt": {
      "type": "pointer",
      "description": "Output pointer for p-sqlite3-stmt"
    },
    "tail": {
      "type": "pointer",
      "description": "Output pointer to remaining SQL text"
    }
  },
  "required": ["db", "sql", "sql-length-bytes", "stmt", "tail"]
}
```

**Returns:** `error-code` enum value

**Errors:**
- `:ERROR` when SQL syntax is invalid
- `:NOMEM` when out of memory

#### `sqlite3-step`
Advances statement to next row or executes non-query.

**Parameters:**
```json
{
  "type": "object",
  "properties": {
    "statement": {
      "type": "pointer",
      "description": "p-sqlite3-stmt handle"
    }
  },
  "required": ["statement"]
}
```

**Returns:**
- `:ROW` when row is available
- `:DONE` when complete
- Other error codes on failure

**Errors:**
- `:BUSY` when database is locked
- `:CONSTRAINT` when constraint violation occurs
- `:MISMATCH` when type mismatch occurs

### Parameter Binding

#### `sqlite3-bind-text`
Binds a text value to a parameter.

**Parameters:**
```json
{
  "type": "object",
  "properties": {
    "statement": {
      "type": "pointer",
      "description": "p-sqlite3-stmt handle"
    },
    "parameter-index": {
      "type": "integer",
      "description": "1-based parameter index"
    },
    "value": {
      "type": "string",
      "description": "Text value to bind"
    },
    "octets-count": {
      "type": "integer",
      "description": "Byte length (-1 for null-terminated)"
    },
    "destructor": {
      "type": "pointer",
      "description": "SQLITE_TRANSIENT or SQLITE_STATIC"
    }
  },
  "required": ["statement", "parameter-index", "value", "octets-count", "destructor"]
}
```

**Returns:** `error-code` enum value

**Errors:**
- `:RANGE` when parameter index is out of range
- `:NOMEM` when out of memory

## Invariants

1. **RULE-FFI-001:** All FFI functions return `error-code` values (except void functions)
2. **RULE-FFI-002:** Parameter indices are 1-based, column indices are 0-based
3. **RULE-FFI-003:** Destructors must be `destructor-transient` or `destructor-static`
4. **RULE-FFI-004:** Database handles must be closed to prevent resource leaks
