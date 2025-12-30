# CL-SQLITE

**CL-SQLITE** is a Common Lisp interface to the [SQLite](https://sqlite.org/) embedded relational database engine. It provides a clean and idiomatic Lisp API for interacting with SQLite databases, supporting both high-level convenience functions and low-level prepared statement operations.

The library is available under the Public Domain.

## Features

*   **Idiomatic API**: simple functions for common tasks (`execute-non-query`, `execute-single`, `execute-to-list`).
*   **Parameter Binding**: Support for both positional (`?`) and named (`:name`, `@name`, `$name`) parameters to prevent SQL injection.
*   **Automatic Type Conversion**: Seamless conversion between SQLite types and Lisp types (Integers, Floats, Strings, BLOBs, NULLs).
*   **Transaction Support**: `with-transaction` macro for atomic operations.
*   **Iterate Integration**: Custom drivers for the [Iterate](https://common-lisp.net/project/iterate/) library for efficient looping over query results.
*   **Prepared Statements**: Full control over statement lifecycle for performance-critical code.
*   **In-Memory Databases**: Easy creation of in-memory databases for testing or temporary storage.

## Installation

You can install `cl-sqlite` via [Quicklisp](https://www.quicklisp.org/beta/):

```lisp
(ql:quickload :sqlite)
```

**Prerequisites:**
You need `sqlite3` installed on your system.
*   **Ubuntu/Debian**: `sudo apt-get install libsqlite3-dev`
*   **macOS**: `brew install sqlite`
*   **Windows**: Ensure `sqlite3.dll` is in your PATH.

## Getting Started

### Quick Start (Simplified Interface)

For many applications, the simplified interface provides a convenient, "Lispy" way to interact with the database without writing raw SQL.

```lisp
(use-package :sqlite)

(defvar *db* (connect ":memory:"))

;; Create a table
;; The schema is defined as a list of column definitions: (name type &rest options)
(create-table *db* :users '((:id :integer :primary-key :autoincrement)
                            (:name :text)
                            (:age :integer)))

;; Insert data
(insert *db* :users '(:name "Alice" :age 30))
(insert *db* :users '(:name "Bob" :age 25))

;; Select data
(select *db* :users)
;; => ((1 "Alice" 30) (2 "Bob" 25))

;; Select with WHERE clause (s-expression)
(select *db* :users :where '(:> :age 28))
;; => ((1 "Alice" 30))

;; Select with ORDER BY
(select *db* :users :order-by '(:age :desc))
;; => ((1 "Alice" 30) (2 "Bob" 25))

;; Update data
(update-table *db* :users '(:age 31) :where '(:= :name "Alice"))

;; Disconnect
(disconnect *db*)
```

### Getting Started (Standard API)

If you prefer more control or need to execute raw SQL, use the standard API.

```lisp
(use-package :sqlite)

;; Connect to an in-memory database
(defvar *db* (connect ":memory:"))

;; Create a table
(execute-non-query *db* "CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT, age INTEGER)")

;; Insert some data
(execute-non-query *db* "INSERT INTO users (name, age) VALUES (?, ?)" "Alice" 30)
(execute-non-query *db* "INSERT INTO users (name, age) VALUES (?, ?)" "Bob" 25)

;; Query data
(execute-to-list *db* "SELECT * FROM users")
;; => ((1 "Alice" 30) (2 "Bob" 25))

;; Disconnect
(disconnect *db*)
```

## Cookbook / Examples

### connecting to a Database

You can connect to a file-based database or an in-memory database.

```lisp
;; Open a file database (creates it if it doesn't exist)
(defvar *db* (connect "/path/to/database.sqlite"))

;; Open a temporary in-memory database
(defvar *mem-db* (connect ":memory:"))

;; Automatically close the database with `with-open-database`
(with-open-database (db "/path/to/database.sqlite")
  (execute-non-query db "CREATE TABLE IF NOT EXISTS test (id INTEGER)"))
```

### Executing Non-Query Statements

Use `execute-non-query` for `INSERT`, `UPDATE`, `DELETE`, and `CREATE` statements.

```lisp
(execute-non-query *db* "UPDATE users SET age = ? WHERE name = ?" 31 "Alice")
```

### Fetching Data

#### Single Value
Use `execute-single` when you expect a single value (e.g., a count or a specific field).

```lisp
(execute-single *db* "SELECT count(*) FROM users")
;; => 2

(execute-single *db* "SELECT age FROM users WHERE name = ?" "Alice")
;; => 31
```

#### Single Row (Multiple Values)
Use `execute-one-row-m-v` to get a single row as multiple values.

```lisp
(multiple-value-bind (id name age)
    (execute-one-row-m-v *db* "SELECT * FROM users WHERE name = ?" "Alice")
  (format t "User: ~A, Age: ~A~%" name age))
```

#### List of Rows
Use `execute-to-list` to get all results as a list of lists.

```lisp
(execute-to-list *db* "SELECT name, age FROM users WHERE age > ?" 20)
;; => (("Alice" 31) ("Bob" 25))
```

### Named Parameters

`cl-sqlite` supports named parameters for better code readability. Use the `/named` variant of the functions.

```lisp
(execute-non-query/named *db* "INSERT INTO users (name, age) VALUES (:name, :age)"
                         ":name" "Charlie"
                         ":age" 40)

(execute-to-list/named *db* "SELECT * FROM users WHERE age > :min_age"
                       ":min_age" 35)
```

### Transactions

Wrap your operations in `with-transaction`. The transaction is automatically committed if the body completes successfully, or rolled back if an error occurs.

```lisp
(with-transaction *db*
  (execute-non-query *db* "UPDATE users SET age = age + 1")
  (execute-non-query *db* "INSERT INTO logs (message) VALUES (?)" "Ages updated"))
```

### Using Iterate

If you use the `iterate` library, `cl-sqlite` provides a driver for iterating over query results efficiently without loading everything into memory.

```lisp
(use-package :iter)

(iter (for (name age) in-sqlite-query "SELECT name, age FROM users" on-database *db*)
      (collect (cons name age)))
```

With parameters:

```lisp
(iter (for (name) in-sqlite-query "SELECT name FROM users WHERE age > ?"
           on-database *db*
           with-parameters (25))
      (collect name))
```

With named parameters:

```lisp
(iter (for (name) in-sqlite-query/named "SELECT name FROM users WHERE age > :age"
           on-database *db*
           with-parameters (":age" 25))
      (collect name))
```

### Prepared Statements (Low-Level API)

For tight loops or specialized usage, you can manage prepared statements manually.

```lisp
(let ((stmt (prepare-statement *db* "INSERT INTO users (name, age) VALUES (?, ?)")))
  (unwind-protect
       (loop for (name age) in '(("Dave" 20) ("Eve" 22))
             do (reset-statement stmt)
                (bind-parameter stmt 1 name)
                (bind-parameter stmt 2 age)
                (step-statement stmt))
    (finalize-statement stmt)))
```

### Handling BLOBs

Binary data (BLOBs) are handled as `(vector (unsigned-byte 8))`.

```lisp
(execute-non-query *db* "CREATE TABLE images (id INTEGER PRIMARY KEY, data BLOB)")

(let ((data (make-array 5 :element-type '(unsigned-byte 8) :initial-contents '(1 2 3 4 5))))
  (execute-non-query *db* "INSERT INTO images (data) VALUES (?)" data))

(let ((retrieved (execute-single *db* "SELECT data FROM images WHERE id = 1")))
  (print retrieved))
;; => #(1 2 3 4 5)
```

## API Reference

### Connection Management

*   **`connect`** `(path &key busy-timeout)`: Connects to the database at `path`. Use `":memory:"` for an in-memory DB.
*   **`disconnect`** `(handle)`: Closes the database connection.
*   **`with-open-database`** `((var path &key busy-timeout) &body body)`: Context manager for database connections.

### Query Execution

*   **`execute-non-query`** `(db sql &rest params)`: Executes a statement that returns no results (INSERT, UPDATE, etc).
*   **`execute-single`** `(db sql &rest params)`: Returns the first column of the first row.
*   **`execute-one-row-m-v`** `(db sql &rest params)`: Returns the first row as multiple values.
*   **`execute-to-list`** `(db sql &rest params)`: Returns the result set as a list of lists.
*   **`last-insert-rowid`** `(db)`: Returns the ID of the last inserted row.

*Named parameter variants:* `execute-non-query/named`, `execute-single/named`, `execute-one-row-m-v/named`, `execute-to-list/named`.

### Transactions

*   **`with-transaction`** `(db &body body)`: Executes body within a transaction. Commits on success, rolls back on error.

### Prepared Statements

*   **`prepare-statement`** `(db sql)`: Compiles a SQL statement.
*   **`bind-parameter`** `(statement index-or-name value)`: Binds a value to a parameter.
*   **`step-statement`** `(statement)`: Executes or advances the statement. Returns `T` if a row is available, `NIL` otherwise.
*   **`reset-statement`** `(statement)`: Resets the statement for re-execution.
*   **`finalize-statement`** `(statement)`: Frees the statement resources.
*   **`statement-column-value`** `(statement index)`: Gets the value of a column in the current row.

### Errors

*   **`sqlite-error`**: Base condition for SQLite errors.
*   **`sqlite-constraint-error`**: Signaled on constraint violations (e.g. unique key violation).

## Running Tests

To run the test suite, you need to load the `:sqlite-tests` system.

```lisp
(ql:quickload :sqlite-tests)
(sqlite-tests:run-all-sqlite-tests)
```

## License

Public Domain.
