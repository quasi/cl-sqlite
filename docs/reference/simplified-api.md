# Simplified Interface Reference

The simplified interface provides high-level functions for CRUD operations using Lisp syntax instead of SQL strings.

## Overview

| Function | Purpose |
|----------|---------|
| `create-table` | Define a new table with columns and constraints |
| `insert` | Add rows to a table |
| `select` | Query rows with optional WHERE and ORDER BY |
| `update-table` | Modify existing rows |
| `delete-from` | Remove rows matching a condition |
| `compile-where` | Compile s-expression WHERE clause to SQL |

## create-table

Create a new table with schema definition.

### Signature

```common-lisp
(create-table db table-name columns)
```

### Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `db` | `sqlite-handle` | Open database connection |
| `table-name` | keyword/symbol | Table name (normalized to lowercase) |
| `columns` | list | Column definitions |

### Column Definition

Each column is a list: `(name type [constraints...])`

```common-lisp
'((:id :integer :primary-key)
  (:name :text :not-null)
  (:email :text :unique)
  (:age :integer :default 0))
```

**Supported Types:**
- `:integer` - 64-bit signed integer
- `:real` - Floating-point number
- `:text` - Unicode text
- `:blob` - Binary data

**Supported Constraints:**
- `:primary-key` - Primary key (usually combined with `:autoincrement`)
- `:autoincrement` - Auto-incrementing integer
- `:not-null` - Disallow NULL values
- `:unique` - Unique constraint
- `:default` - Default value
- `:check` - Check constraint (value)

### Examples

```common-lisp
;; Simple table
(create-table db :users
  '((:id :integer :primary-key :autoincrement)
    (:name :text :not-null)
    (:email :text :unique)))

;; Table with constraints
(create-table db :orders
  '((:id :integer :primary-key)
    (:user-id :integer :not-null)
    (:total :real :check 0.0)
    (:status :text :default "pending")))
```

## insert

Add one or more rows to a table.

### Signature

```common-lisp
(insert db table-name row-plist &key if-not-exists)
```

### Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `db` | `sqlite-handle` | Open database connection |
| `table-name` | keyword/symbol | Target table |
| `row-plist` | plist | Column values as plist |
| `:if-not-exists` | boolean | Use `INSERT OR IGNORE` (default: nil) |

### Examples

```common-lisp
;; Single insert
(insert db :users '(:name "Alice" :email "alice@example.com"))

;; Multiple inserts
(insert db :users '(:name "Bob" :email "bob@example.com"))
(insert db :users '(:name "Charlie" :email "charlie@example.com"))

;; Insert with if-not-exists
(insert db :users '(:id 1 :name "Alice") :if-not-exists t)
```

### Returns

The newly inserted row's `:id` (if table has auto-increment).

## select

Query rows from a table.

### Signature

```common-lisp
(select db table-name &key where order-by limit offset)
```

### Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `db` | `sqlite-handle` | Open database connection |
| `table-name` | keyword/symbol | Source table |
| `:where` | s-expression | WHERE clause (optional) |
| `:order-by` | keyword/list | ORDER BY clause (optional) |
| `:limit` | integer | LIMIT clause (optional) |
| `:offset` | integer | OFFSET clause (optional) |

### WHERE Clause Operators

The `:where` parameter accepts s-expressions using these operators:

| Operator | Meaning | Example |
|----------|---------|---------|
| `:=` | Equals | `(:= :age 30)` |
| `:<` | Less than | `(:< :age 18)` |
| `:>` | Greater than | `(:> :age 65)` |
| `:<=` | Less or equal | `(:>= :salary 50000)` |
| `:>=` | Greater or equal | `(:>= :salary 50000)` |
| `:<>` | Not equal | `(:<> :status "inactive")` |
| `:like` | Pattern match | `(:like :email "%@gmail.com")` |
| `:in` | In list | `(:in :status "active" "pending")` |
| `:is-null` | NULL check | `(:is-null :deleted-at)` |
| `:is-not-null` | NOT NULL | `(:is-not-null :email)` |
| `:and` | Logical AND | `(:and (:> :age 18) (:= :active 1))` |
| `:or` | Logical OR | `(:or (:= :status "admin") (:= :vip 1))` |
| `:not` | Logical NOT | `(:not (:= :deleted 1))` |

### ORDER BY Syntax

```common-lisp
;; Single column, ascending (default)
:order-by :name

;; Single column, descending
:order-by '(:name :desc)

;; Multiple columns
:order-by '((:age :desc) (:name :asc))
```

### Examples

```common-lisp
;; All rows
(select db :users)

;; With WHERE
(select db :users :where '(:= :active 1))

;; Complex WHERE
(select db :users
        :where '(:and (:> :age 18)
                      (:= :active 1)
                      (:is-not-null :email)))

;; With ORDER BY
(select db :users :order-by '(:age :desc))

;; With LIMIT
(select db :users :limit 10)

;; Combined
(select db :users
        :where '(:> :age 18)
        :order-by '(:name :asc)
        :limit 10
        :offset 20)
```

### Returns

List of rows. Each row is a list of column values in table order.

```common-lisp
((1 "Alice" "alice@example.com")
 (2 "Bob" "bob@example.com"))
```

## update-table

Modify existing rows.

### Signature

```common-lisp
(update-table db table-name values-plist &key where)
```

### Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `db` | `sqlite-handle` | Open database connection |
| `table-name` | keyword/symbol | Target table |
| `values-plist` | plist | New values as plist |
| `:where` | s-expression | WHERE clause (optional, omit to update all) |

### Examples

```common-lisp
;; Update single row
(update-table db :users '(:email "alice-new@example.com")
              :where '(:= :id 1))

;; Update multiple rows
(update-table db :users '(:active 0)
              :where '(:< :age 18))

;; Update all rows (careful!)
(update-table db :users '(:updated-at (current-timestamp)))
```

### Returns

Number of rows updated.

## delete-from

Remove rows from a table.

### Signature

```common-lisp
(delete-from db table-name &key where)
```

### Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `db` | `sqlite-handle` | Open database connection |
| `table-name` | keyword/symbol | Target table |
| `:where` | s-expression | WHERE clause (optional, omit to delete all) |

### Examples

```common-lisp
;; Delete single row
(delete-from db :users :where '(:= :id 1))

;; Delete with complex WHERE
(delete-from db :users
            :where '(:and (:< :age 18)
                          (:is-null :parent-id)))

;; Delete all rows (careful!)
(delete-from db :users)
```

### Returns

Number of rows deleted.

## compile-where

Compile an s-expression WHERE clause to SQL.

### Signature

```common-lisp
(compile-where where-clause)
```

### Returns

`(values sql-string param-list)`

### Example

```common-lisp
(multiple-value-bind (sql params)
    (compile-where '(:and (:> :age 18) (:= :active 1)))
  (format t "SQL: ~a~%" sql)
  (format t "Params: ~a~%" params))
```

**Output:**
```
SQL: (age > ? AND active = ?)
Params: (18 1)
```

## Error Handling

### SQL Injection Prevention

The simplified API prevents SQL injection by design:

```common-lisp
;; Safe: user-input is parameterized
(select db :users :where `(:= :email ,user-input))

;; User input never enters SQL string
```

### Invalid WHERE Operator

Using an unknown operator signals an error:

```common-lisp
(select db :users :where '(:invalid-op :name "test"))
;; Error: Unknown operator :INVALID-OP
```

## Common Patterns

### Check if record exists

```common-lisp
(let ((result (select db :users :where '(:= :id 1))))
  (if result "exists" "not found"))
```

### Count rows

```common-lisp
(length (select db :users :where '(:> :age 18)))
```

### Get first match

```common-lisp
(car (select db :users :where '(:= :email "alice@example.com")))
```

### Batch operations

```common-lisp
(with-transaction (db)
  (dolist (user user-list)
    (insert db :users user)))
```

---

**See also:** [Standard API Reference](./standard-api.md), [Full API](./full-api.md)
