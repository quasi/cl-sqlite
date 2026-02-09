---
type: contract
name: conditions-core
version: 1.0.0
feature: conditions
---

# Conditions Contract

## Purpose
Defines condition types for SQLite errors and provides structured error signaling with database context, error codes, and SQL text.

## Condition Types

### `sqlite-error`
Primary condition for all SQLite errors.

**Slots Schema:**
```json
{
  "type": "object",
  "properties": {
    "handle": {
      "type": "object",
      "description": "sqlite-handle that experienced the error (may be NIL)",
      "reader": "sqlite-error-db-handle"
    },
    "error-code": {
      "type": "keyword",
      "description": "SQLite error code enum (:ERROR, :BUSY, :CONSTRAINT, etc.)",
      "reader": "sqlite-error-code"
    },
    "error-msg": {
      "type": "string",
      "description": "Error message from sqlite3_errmsg",
      "reader": "sqlite-error-message"
    },
    "statement": {
      "type": "object",
      "description": "sqlite-statement that caused the error (may be NIL)",
      "reader": "sqlite-error-statement"
    },
    "sql": {
      "type": "string",
      "description": "SQL text that caused the error (may be NIL)",
      "reader": "sqlite-error-sql"
    }
  }
}
```

**Parent:** `simple-error`

### `sqlite-constraint-error`
Specialized condition for constraint violations (UNIQUE, NOT NULL, FOREIGN KEY, CHECK).

**Slots Schema:**
```json
{
  "type": "object",
  "properties": {},
  "description": "Inherits all slots from sqlite-error"
}
```

**Parent:** `sqlite-error`

## Error Signaling

#### `sqlite-error` (function)
Constructs and signals an error condition.

**Parameters:**
```json
{
  "type": "object",
  "properties": {
    "error-code": {
      "type": "keyword",
      "description": "SQLite error code (:ERROR, :CONSTRAINT, :BUSY, etc.)"
    },
    "message": {
      "oneOf": [
        {"type": "string"},
        {"type": "array", "description": "Format control string and arguments"}
      ]
    },
    "statement": {
      "type": "object",
      "description": "sqlite-statement (optional)"
    },
    "db-handle": {
      "type": "object",
      "description": "sqlite-handle (derived from statement if not provided)"
    },
    "sql-text": {
      "type": "string",
      "description": "SQL text (derived from statement if not provided)"
    }
  },
  "required": ["error-code", "message"]
}
```

**Behavior:**
- Signals `sqlite-constraint-error` when error-code is `:CONSTRAINT`
- Signals `sqlite-error` for all other error codes
- Automatically retrieves error message from database via `sqlite3-errmsg`

**Errors:**
This function does not return; it always signals a condition.

## Print Representation

When printed, `sqlite-error` objects display:
- Error code and message
- Database path (if available)
- SQL text that caused the error (if available)

Example output:
```
Code CONSTRAINT: UNIQUE constraint failed: users.email.
Database: /path/to/db.sqlite
SQL: INSERT INTO users (email) VALUES (?)
```

## Invariants

1. **RULE-COND-001:** `sqlite-constraint-error` is signaled only for `:CONSTRAINT` errors
2. **RULE-COND-002:** Error message is retrieved from SQLite via FFI when db-handle is available
3. **RULE-COND-003:** All slots except error-code may be NIL
4. **RULE-COND-004:** SQL text is automatically captured from statement when available
