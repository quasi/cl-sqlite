---
type: contract
name: simplified-interface-core
version: 1.0.0
feature: simplified-interface
---

# Simplified Interface Contract

## Purpose
Provides high-level, SQL-injection-safe CRUD operations using s-expressions and plists. Automatically generates parameterized SQL from Lisp data structures.

## Name Normalization

#### `normalize-name`
Converts Lisp names to safe SQL identifiers.

**Parameters:**
```json
{
  "type": "object",
  "properties": {
    "name": {
      "oneOf": [
        {"type": "symbol"},
        {"type": "keyword"},
        {"type": "string"}
      ],
      "description": "Lisp identifier to convert"
    }
  },
  "required": ["name"]
}
```

**Returns:** String (lowercase, hyphens converted to underscores)

**Validation:**
- Must contain only alphanumeric characters and underscores
- Must not start with a digit
- Must not be empty

**Errors:**
- `sqlite-error` when identifier is invalid (prevents SQL injection)

## Table Operations

#### `create-table`
Creates a table from s-expression column definitions.

**Parameters:**
```json
{
  "type": "object",
  "properties": {
    "db": {
      "type": "object",
      "description": "sqlite-handle"
    },
    "name": {
      "type": "symbol",
      "description": "Table name"
    },
    "columns": {
      "type": "array",
      "description": "List of column definitions",
      "items": {
        "type": "array",
        "description": "(name type [:primary-key] [:autoincrement] [:not-null] [:unique])"
      }
    },
    "if-not-exists": {
      "type": "boolean",
      "default": false
    }
  },
  "required": ["db", "name", "columns"]
}
```

**Example:**
```lisp
(create-table db 'users
  '((id :integer :primary-key :autoincrement)
    (email :text :not-null :unique)
    (created-at :text)))
```

**Errors:**
- `sqlite-error` when table already exists (without `:if-not-exists`)
- `sqlite-error` when column definition is invalid

#### `drop-table`
Drops a table.

**Parameters:**
```json
{
  "type": "object",
  "properties": {
    "db": {
      "type": "object",
      "description": "sqlite-handle"
    },
    "name": {
      "type": "symbol",
      "description": "Table name"
    },
    "if-exists": {
      "type": "boolean",
      "default": false
    }
  },
  "required": ["db", "name"]
}
```

**Errors:**
- `sqlite-error` when table does not exist (without `:if-exists`)

## CRUD Operations

#### `insert`
Inserts a row from a plist.

**Parameters:**
```json
{
  "type": "object",
  "properties": {
    "db": {
      "type": "object",
      "description": "sqlite-handle"
    },
    "table": {
      "type": "symbol",
      "description": "Table name"
    },
    "data": {
      "type": "array",
      "description": "Plist of column names and values",
      "example": "(:email \"user@example.com\" :name \"Alice\")"
    }
  },
  "required": ["db", "table", "data"]
}
```

**Returns:** NIL

**Errors:**
- `sqlite-constraint-error` when constraints are violated
- `sqlite-error` when column names are invalid

#### `select`
Selects rows using s-expression WHERE clauses.

**Parameters:**
```json
{
  "type": "object",
  "properties": {
    "db": {
      "type": "object",
      "description": "sqlite-handle"
    },
    "table": {
      "type": "symbol",
      "description": "Table name"
    },
    "columns": {
      "type": "array",
      "description": "List of column names or '(*)",
      "default": "(*)"
    },
    "where": {
      "type": "array",
      "description": "S-expression WHERE clause",
      "example": "(:and (:= :status \"active\") (:> :age 18))"
    },
    "order-by": {
      "oneOf": [
        {"type": "keyword", "description": "Single column"},
        {"type": "array", "description": "(:column :asc/:desc)"},
        {"type": "array", "items": {"type": "array"}, "description": "List of order clauses"}
      ]
    },
    "limit": {
      "type": "integer",
      "minimum": 0
    },
    "offset": {
      "type": "integer",
      "minimum": 0
    }
  },
  "required": ["db", "table"]
}
```

**Returns:** List of lists (rows)

**WHERE Clause Operators:**
- `:=`, `:<`, `:>`, `:<=`, `:>=`, `:<>`, `:like` - Comparison operators
- `:and`, `:or`, `:not` - Logical operators
- `:in` - List membership
- `:is-null`, `:is-not-null` - NULL checks

**Errors:**
- `sqlite-error` when LIMIT/OFFSET are not non-negative integers
- `sqlite-error` when ORDER BY direction is not :asc or :desc
- `sqlite-error` when WHERE clause has unknown operator

#### `update-table`
Updates rows from a plist with WHERE clause.

**Parameters:**
```json
{
  "type": "object",
  "properties": {
    "db": {
      "type": "object",
      "description": "sqlite-handle"
    },
    "table": {
      "type": "symbol",
      "description": "Table name"
    },
    "data": {
      "type": "array",
      "description": "Plist of columns to update",
      "example": "(:status \"inactive\" :updated-at \"2026-02-09\")"
    },
    "where": {
      "type": "array",
      "description": "S-expression WHERE clause"
    }
  },
  "required": ["db", "table", "data"]
}
```

**Returns:** NIL

**Errors:**
- `sqlite-constraint-error` when constraints are violated

#### `delete-from`
Deletes rows matching WHERE clause.

**Parameters:**
```json
{
  "type": "object",
  "properties": {
    "db": {
      "type": "object",
      "description": "sqlite-handle"
    },
    "table": {
      "type": "symbol",
      "description": "Table name"
    },
    "where": {
      "type": "array",
      "description": "S-expression WHERE clause (optional but recommended)"
    }
  },
  "required": ["db", "table"]
}
```

**Returns:** NIL

**Warning:** Without WHERE clause, deletes ALL rows.

## Invariants

1. **RULE-SIMP-001:** All identifiers are validated to prevent SQL injection
2. **RULE-SIMP-002:** All values are passed via parameters (never interpolated)
3. **RULE-SIMP-003:** Hyphens in Lisp names become underscores in SQL
4. **RULE-SIMP-004:** ORDER BY direction must be :asc or :desc (validated at runtime)
5. **RULE-SIMP-005:** LIMIT and OFFSET must be non-negative integers
