---
type: contract
name: error-handling-core
version: 1.0.0
feature: error-handling
---

# Error Handling Contract

## Purpose
Provides structured error handling with restarts for query operations. All query execution functions establish restarts for retry and skip behaviors.

## Restart Protocol

### Standard Restarts

All query execution functions provide these restarts:

#### `retry-query`
Re-executes the query from the beginning.

**Behavior:**
- Recreates prepared statement
- Re-binds all parameters with original values
- Executes the query again

**Use Cases:**
- Database was locked (`:BUSY`), retry after delay
- Temporary I/O error resolved
- Manual intervention completed

#### `skip-query`
Skips query execution and returns NIL.

**Behavior:**
- Statement is finalized
- Function returns NIL immediately
- No database changes are made

**Use Cases:**
- Non-critical query that can be skipped
- Fallback to cached data
- Graceful degradation

## Query Function Error Behavior

#### `execute-non-query`
**Restarts Schema:**
```json
{
  "type": "object",
  "properties": {
    "retry-query": {
      "description": "Re-execute with same SQL and parameters",
      "returns": "Result of successful execution or NIL"
    },
    "skip-query": {
      "description": "Skip execution and return NIL",
      "returns": "NIL"
    }
  }
}
```

**Example:**
```lisp
(handler-bind ((sqlite-error
                 (lambda (c)
                   (when (eq (sqlite-error-code c) :busy)
                     (sleep 0.1)
                     (invoke-restart 'retry-query)))))
  (execute-non-query db "INSERT INTO ..."))
```

#### `execute-to-list`
**Restarts Schema:**
```json
{
  "type": "object",
  "properties": {
    "retry-query": {
      "description": "Re-execute and collect results",
      "returns": "List of rows"
    },
    "skip-query": {
      "description": "Skip and return NIL",
      "returns": "NIL (not empty list)"
    }
  }
}
```

#### `execute-single`
**Restarts Schema:**
```json
{
  "type": "object",
  "properties": {
    "retry-query": {
      "description": "Re-execute and return scalar value",
      "returns": "Single value or NIL"
    },
    "skip-query": {
      "description": "Skip and return NIL",
      "returns": "NIL"
    }
  }
}
```

#### `execute-one-row-m-v`
**Restarts Schema:**
```json
{
  "type": "object",
  "properties": {
    "retry-query": {
      "description": "Re-execute and return row as multiple values",
      "returns": "Multiple values (one per column)"
    },
    "skip-query": {
      "description": "Skip and return NIL",
      "returns": "NIL"
    }
  }
}
```

#### `connect`
**Restarts Schema:**
```json
{
  "type": "object",
  "properties": {
    "retry-query": {
      "description": "Retry connection with same parameters",
      "returns": "sqlite-handle"
    },
    "skip-query": {
      "description": "Skip connection and return NIL",
      "returns": "NIL"
    }
  }
}
```

## Error Context

All `sqlite-error` conditions provide context for handlers:

**Available Information:**
```json
{
  "type": "object",
  "properties": {
    "error-code": {
      "description": "SQLite error code keyword",
      "accessor": "sqlite-error-code"
    },
    "error-message": {
      "description": "Human-readable error from SQLite",
      "accessor": "sqlite-error-message"
    },
    "db-handle": {
      "description": "Database connection (may be NIL)",
      "accessor": "sqlite-error-db-handle"
    },
    "sql-text": {
      "description": "SQL that caused error (may be NIL)",
      "accessor": "sqlite-error-sql"
    },
    "statement": {
      "description": "Prepared statement (may be NIL)",
      "accessor": "sqlite-error-statement"
    }
  }
}
```

## Common Error Patterns

### Busy Handler Pattern
```lisp
(handler-bind ((sqlite-error
                 (lambda (c)
                   (when (eq (sqlite-error-code c) :busy)
                     (sleep 0.1)
                     (invoke-restart 'retry-query)))))
  ;; query operations
  )
```

### Constraint Violation Handler
```lisp
(handler-case
    (insert db 'users :email "duplicate@example.com")
  (sqlite-constraint-error (c)
    (format t "Constraint violated: ~A" (sqlite-error-message c))
    nil))
```

### Skip Non-Critical Queries
```lisp
(handler-bind ((sqlite-error
                 (lambda (c)
                   (invoke-restart 'skip-query))))
  (execute-non-query db "UPDATE analytics SET ..."))
```

## Invariants

1. **RULE-ERR-001:** All query functions establish `retry-query` and `skip-query` restarts
2. **RULE-ERR-002:** `retry-query` re-executes with original parameters (side-effect: statement lifecycle repeats)
3. **RULE-ERR-003:** `skip-query` always returns NIL (never an empty list or other false value)
4. **RULE-ERR-004:** Restarts are established in the dynamic scope of the query function
5. **RULE-ERR-005:** Constraint errors signal `sqlite-constraint-error`, all others signal `sqlite-error`
6. **RULE-ERR-006:** Error conditions always contain error-code; other slots may be NIL
