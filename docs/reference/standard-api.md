# Standard Interface Reference

The standard interface accepts raw SQL strings with parameterized values. Use this for complex queries and when you need full SQL control.

## Overview

| Function | Purpose |
|----------|---------|
| `execute-to-list` | Execute query, return all rows |
| `execute-single` | Execute query, return single row |
| `execute-one-row-m-v` | Execute query, return multiple values |
| `execute-non-query` | Execute statement (INSERT/UPDATE/DELETE) |
| `last-insert-rowid` | Get auto-increment ID of last insert |

## execute-to-list

Execute a SELECT query and return all rows.

### Signature

```common-lisp
(execute-to-list db sql &rest params)
```

### Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `db` | `sqlite-handle` | Open database connection |
| `sql` | string | SQL query with `?` placeholders |
| `params...` | any | Parameter values (in order) |

### Examples

```common-lisp
;; Simple query
(execute-to-list db "SELECT * FROM users")

;; With parameters
(execute-to-list db "SELECT * FROM users WHERE age > ?" 18)

;; Multiple parameters
(execute-to-list db "SELECT * FROM users WHERE age > ? AND status = ?" 18 "active")

;; Complex query
(execute-to-list db
  "SELECT u.name, COUNT(p.id) as post_count FROM users u
   LEFT JOIN posts p ON u.id = p.user_id
   GROUP BY u.id ORDER BY post_count DESC LIMIT ?
  " 10)
```

### Returns

List of rows, where each row is a list of column values:

```common-lisp
((1 "Alice" 30) (2 "Bob" 25) (3 "Charlie" 35))
```

## execute-single

Execute a SELECT query and return a single row.

### Signature

```common-lisp
(execute-single db sql &rest params)
```

### Parameters

Same as `execute-to-list`

### Examples

```common-lisp
;; Get one user
(execute-single db "SELECT * FROM users WHERE id = ?" 1)

;; Get count
(execute-single db "SELECT COUNT(*) FROM users")

;; Get aggregate
(execute-single db "SELECT AVG(age) FROM users WHERE status = ?" "active")
```

### Returns

Single row as a list, or `NIL` if no match:

```common-lisp
(1 "Alice" 30)   ; Single row found
NIL              ; No match
```

### Common Pattern: Count

```common-lisp
(let ((result (execute-single db "SELECT COUNT(*) FROM users")))
  (if result
      (format t "Total users: ~a~%" (car result))
      (format t "No users~%")))
```

## execute-one-row-m-v

Execute a SELECT query and return column values as multiple values.

### Signature

```common-lisp
(execute-one-row-m-v db sql &rest params)
```

### Parameters

Same as `execute-to-list`

### Example

```common-lisp
(multiple-value-bind (id name age)
    (execute-one-row-m-v db "SELECT id, name, age FROM users WHERE id = ?" 1)
  (format t "User: ~a (~a years old)~%" name age))
```

### Returns

Column values as multiple return values (not a list)

## execute-non-query

Execute an INSERT, UPDATE, or DELETE statement.

### Signature

```common-lisp
(execute-non-query db sql &rest params)
```

### Parameters

Same as `execute-to-list`

### Examples

```common-lisp
;; Insert
(execute-non-query db "INSERT INTO users (name, age) VALUES (?, ?)" "Alice" 30)

;; Update
(execute-non-query db "UPDATE users SET age = ? WHERE id = ?" 31 1)

;; Delete
(execute-non-query db "DELETE FROM users WHERE age < ?" 18)

;; Batch insert (less efficient than transaction)
(dolist (user users)
  (execute-non-query db "INSERT INTO users (name, age) VALUES (?, ?)"
                      (car user) (cadr user)))
```

### Returns

The number of rows affected (inserted/updated/deleted)

## last-insert-rowid

Get the auto-increment ID from the most recent INSERT.

### Signature

```common-lisp
(last-insert-rowid db)
```

### Example

```common-lisp
(execute-non-query db "INSERT INTO users (name, age) VALUES (?, ?)" "Alice" 30)
(let ((id (last-insert-rowid db)))
  (format t "New user ID: ~a~%" id))
```

### Returns

Integer ID of last inserted row (or 0 if no insert)

## Advanced: Named Parameters

For complex queries, use named parameters:

```common-lisp
;; SQLite allows multiple parameter syntax:
;; ?     - Positional (1-based)
;; :name - Named
;; @name - Named (alternative)
;; $name - Named (alternative)

(execute-to-list db
  "SELECT * FROM users WHERE age > :min_age AND status = :status"
  :min-age 18 :status "active")
```

**Note:** Different SQL implementations may use different named parameter syntax.

## Performance: Prepared Statements

For repeated queries, use the prepared statement API for better performance:

```common-lisp
;; Standard API (re-parses each time)
(dolist (id (list 1 2 3 4 5))
  (execute-single db "SELECT * FROM users WHERE id = ?" id))

;; Prepared API (parse once)
(let ((stmt (prepare-statement db "SELECT * FROM users WHERE id = ?")))
  (dolist (id (list 1 2 3 4 5))
    (bind-parameter stmt 1 id)
    (step-statement stmt)
    (format t "User: ~a~%" (statement-column-value stmt 1)))
  (finalize-statement stmt))
```

**Note:** Statements are automatically cached, so repeated queries benefit from caching even with the standard API.

## Error Handling

### Parameter Mismatch

```common-lisp
;; Error: 2 placeholders but 1 parameter
(execute-to-list db "SELECT * FROM users WHERE age > ? AND id = ?" 18)

;; Correct: 2 parameters
(execute-to-list db "SELECT * FROM users WHERE age > ? AND id = ?" 18 1)
```

### Invalid SQL

```common-lisp
;; Error: SQL syntax error
(execute-to-list db "SELET * FROM users")  ; Typo: SELET

;; Correct
(execute-to-list db "SELECT * FROM users")
```

### Constraint Violation

```common-lisp
;; Error: UNIQUE constraint violation
(execute-non-query db "INSERT INTO users (email) VALUES (?)" "alice@example.com")
(execute-non-query db "INSERT INTO users (email) VALUES (?)" "alice@example.com")
;; sqlite-constraint-error signaled
```

### Catching Errors

```common-lisp
(handler-case
    (execute-non-query db "INSERT INTO users (email) VALUES (?)" "alice@example.com")
  (sqlite:sqlite-constraint-error (e)
    (format t "Duplicate email!~%"))
  (sqlite:sqlite-error (e)
    (format t "Database error: ~a~%" (sqlite:error-msg e))))
```

## Common Patterns

### COUNT with WHERE

```common-lisp
(let ((result (execute-single db
               "SELECT COUNT(*) FROM users WHERE age > ?"
               18)))
  (car result))  ; Extract count
```

### EXISTS

```common-lisp
(let ((exists (execute-single db
               "SELECT 1 FROM users WHERE email = ?" user-email)))
  (if exists "found" "not found"))
```

### MAX/MIN

```common-lisp
(execute-single db "SELECT MAX(age), MIN(age) FROM users")
```

### ORDER BY LIMIT

```common-lisp
(execute-to-list db
  "SELECT * FROM posts ORDER BY created_at DESC LIMIT ? OFFSET ?"
  10 20)  ; Get posts 20-30
```

### JOINs

```common-lisp
(execute-to-list db
  "SELECT u.name, COUNT(p.id) as posts
   FROM users u LEFT JOIN posts p ON u.id = p.user_id
   GROUP BY u.id ORDER BY posts DESC")
```

## See Also

- [Simplified Interface Reference](./simplified-api.md) - For simple CRUD
- [Prepared Statement Reference](./prepared-api.md) - For maximum performance
- [Full API Reference](./full-api.md) - All functions
