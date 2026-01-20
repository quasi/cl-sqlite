# Contract: statement-info (Column Metadata Functions)

**Signatures:**
- `(statement-column-name stmt column-index) => string`
- `(statement-column-origin-name stmt column-index) => string`
- `(statement-column-table-name stmt column-index) => string`
- `(statement-column-database-name stmt column-index) => string`

**Confidence:** 0.85
**Status:** Stable

---

## Overview

These functions retrieve metadata about columns in a prepared statement's result set.

**Use Cases:**
- Dynamically build column headers
- Map result columns to database schema
- Debug query results
- Determine which table/database column came from

---

## statement-column-name

### Signature
`(statement-column-name stmt column-index) => string`

### Description
Returns the column name or alias from the SELECT clause

### Returns
**Type:** `string`

**Behavior:**
- Returns name from SELECT clause
- If column has AS alias, returns alias
- If column is expression (e.g., `COUNT(*)`), returns expression string
- If column is unnamed, returns empty string

### Examples
```common-lisp
;; SELECT name, age, ...
(statement-column-name stmt 0)  ; => "name"
(statement-column-name stmt 1)  ; => "age"

;; SELECT COUNT(*) as cnt, ...
(statement-column-name stmt 0)  ; => "cnt"

;; SELECT id+1, ...
(statement-column-name stmt 0)  ; => "id+1"

;; SELECT * (all columns)
(statement-column-name stmt 0)  ; => "id"
(statement-column-name stmt 1)  ; => "name"
```

---

## statement-column-origin-name

### Signature
`(statement-column-origin-name stmt column-index) => string`

### Description
Returns the original column name (before aliasing)

### Returns
**Type:** `string`

**Behavior:**
- Returns true column name from table schema
- Ignores SELECT clause aliases
- Useful for mapping back to source column

### Examples
```common-lisp
;; SELECT name as user_name, age as user_age
(statement-column-name stmt 0)         ; => "user_name"
(statement-column-origin-name stmt 0)  ; => "name"

(statement-column-name stmt 1)         ; => "user_age"
(statement-column-origin-name stmt 1)  ; => "age"
```

---

## statement-column-table-name

### Signature
`(statement-column-table-name stmt column-index) => string`

### Description
Returns the table name that the column belongs to

### Returns
**Type:** `string`

**Behavior:**
- Returns source table name
- Useful for JOINs (which table did this column come from)
- Empty if column is computed expression

### Examples
```common-lisp
;; SELECT users.id, posts.id
(statement-column-table-name stmt 0)  ; => "users"
(statement-column-table-name stmt 1)  ; => "posts"

;; SELECT COUNT(*) as cnt
(statement-column-table-name stmt 0)  ; => ""
```

---

## statement-column-database-name

### Signature
`(statement-column-database-name stmt column-index) => string`

### Description
Returns the database name (main, temp, or attached database)

### Returns
**Type:** `string`

**Behavior:**
- Returns database: "main", "temp", or name of ATTACH DATABASE
- Empty for computed columns
- Usually "main" for typical queries

### Examples
```common-lisp
;; SELECT id FROM users
(statement-column-database-name stmt 0)  ; => "main"

;; SELECT id FROM temp.cache
(statement-column-database-name stmt 0)  ; => "temp"

;; SELECT id FROM attached.table
(statement-column-database-name stmt 0)  ; => "attached"
```

---

## Implementation

**Location:** `sqlite.lisp:324-345`

**Key Code:**
```common-lisp
(defun statement-column-name (stmt column-index)
  (sqlite-ffi:sqlite3-column-name (stmt stmt) column-index))

(defun statement-column-origin-name (stmt column-index)
  (sqlite-ffi:sqlite3-column-origin-name (stmt stmt) column-index))

(defun statement-column-table-name (stmt column-index)
  (sqlite-ffi:sqlite3-column-table-name (stmt stmt) column-index))

(defun statement-column-database-name (stmt column-index)
  (sqlite-ffi:sqlite3-column-database-name (stmt stmt) column-index))
```

**Direct FFI Calls:** Thin wrappers over SQLite column metadata functions

---

## Usage Examples

### Build Column Headers
```common-lisp
(defun get-column-headers (db sql)
  (let ((stmt (prepare-statement db sql)))
    (prog1
        (loop for i from 0 below (statement-column-count stmt)
              collect (statement-column-name stmt i))
      (finalize-statement stmt))))

(get-column-headers db "SELECT id, name, age FROM users")
;; => ("id" "name" "age")

(get-column-headers db "SELECT id as user_id, name FROM users")
;; => ("user_id" "name")
```

---

### Display Result with Headers
```common-lisp
(let ((stmt (prepare-statement db "SELECT id, name FROM users")))
  ;; Get headers
  (let ((headers (loop for i from 0 below (statement-column-count stmt)
                       collect (statement-column-name stmt i))))
    (format t "~A ~A~%" (first headers) (second headers))
    ;; Get rows
    (loop while (step-statement stmt)
          do (format t "~D ~A~%"
                     (statement-column-value stmt 0)
                     (statement-column-value stmt 1))))
  (finalize-statement stmt))
```

---

### Debug Query Results
```common-lisp
(defun describe-columns (db sql)
  (let ((stmt (prepare-statement db sql)))
    (loop for i from 0 below (statement-column-count stmt)
          do (format t "Column ~D:~%" i)
             (format t "  Name: ~A~%" (statement-column-name stmt i))
             (format t "  Origin: ~A~%" (statement-column-origin-name stmt i))
             (format t "  Table: ~A~%" (statement-column-table-name stmt i))
             (format t "  Database: ~A~%" (statement-column-database-name stmt i)))
    (finalize-statement stmt)))

(describe-columns db "SELECT id, name FROM users")
;; Outputs metadata for each column
```

---

### MAP Results to Objects
```common-lisp
(defun fetch-as-alist (db sql)
  (let ((stmt (prepare-statement db sql)))
    (prog1
        (let ((headers (loop for i from 0 below (statement-column-count stmt)
                             collect (statement-column-name stmt i))))
          (loop while (step-statement stmt)
                collect (loop for i from 0 below (statement-column-count stmt)
                              for name in headers
                              collect (cons (intern name :keyword)
                                          (statement-column-value stmt i)))))
      (finalize-statement stmt))))

(fetch-as-alist db "SELECT id, name FROM users")
;; => ((:ID 1 :NAME "Alice") (:ID 2 :NAME "Bob"))
```

---

## Related Contracts

- `prepare-statement` - Creates statement (metadata available after prepare)
- `statement-column-count` - Gets column count (for loop bounds)
- `statement-column-value` - Reads column data

---

## Patterns

**Recommended:** Use for dynamic column mapping
```common-lisp
(defun query-to-alist (db sql &rest params)
  (let ((stmt (prepare-statement db sql)))
    (loop for i from 1 for p in params
          do (bind-parameter stmt i p))
    (prog1
        (let ((headers (loop for i from 0 below (statement-column-count stmt)
                             collect (statement-column-name stmt i))))
          (loop while (step-statement stmt)
                collect (loop for i from 0 below (statement-column-count stmt)
                              for name in headers
                              collect (cons (intern name :keyword)
                                          (statement-column-value stmt i)))))
      (finalize-statement stmt))))
```

---

## Performance Notes

- **Metadata access:** O(1) per column
- **Timing:** Available after prepare (before stepping)
- **Caching:** Consider caching headers if needed multiple times

---

## Preconditions

- Statement prepared (compiled)
- Column index valid (0 to column-count-1)

---

## Postconditions

**Success:**
- Returns metadata string
- Database state unchanged

---

## Edge Cases

### Computed Columns
```common-lisp
(statement-column-name (prepare-statement db "SELECT COUNT(*) as cnt") 0)
;; => "cnt"

(statement-column-table-name (prepare-statement db "SELECT COUNT(*)") 0)
;; => ""
```

---

### Aliased Columns
```common-lisp
;; SELECT users.name as user_name
(statement-column-name stmt 0)          ; => "user_name"
(statement-column-origin-name stmt 0)   ; => "name"
(statement-column-table-name stmt 0)    ; => "users"
```

---

## Validation

**Tests:**
- Limited testing (metadata queries)
- Used in result mapping scenarios

**Documentation:**
- README.md (not mentioned)
- REFERENCE.md (not documented)
- CL-SQLITE.agent.md:890

**Verified Properties:**
- Column names retrieved correctly
- Metadata consistent with schema

---

**Document Status:** Canonical
**Last Verified:** 2026-01-20 (Canon Initiation Pass 2)
