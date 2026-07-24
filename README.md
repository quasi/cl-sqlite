# Inquisitio

**Inquisitio** is a Common Lisp interface to the [SQLite](https://sqlite.org/) embedded relational database engine. It provides a clean and idiomatic Lisp API for interacting with SQLite databases, supporting both high-level convenience functions and low-level prepared statement operations. Supports the **Vector extensions**.

> This Library is designed by me and  implemented by Claude with my inputs. This is designed for consumption by Agents as well as Humans.  If you have a problem with Agent written code then this library is *not* for you. You can try the original [cl-sqlite](https://github.com/TeMPOraL/cl-sqlite).

Your comments / feedback most welcome.

The library is available under the MIT license.

## Features

*   **Idiomatic API**: simple functions for common tasks (`execute-non-query`, `execute-single`, `execute-to-list`).
*   **Parameter Binding**: Support for both positional (`?`) and named (`:name`, `@name`, `$name`) parameters to prevent SQL injection.
*   **Automatic Type Conversion**: Seamless conversion between SQLite types and Lisp types (Integers, Floats, Strings, BLOBs, NULLs).
*   **Transaction Support**: `with-transaction` macro for atomic operations.
*   **Iterate Integration**: Custom drivers for the [Iterate](https://common-lisp.net/project/iterate/) library for efficient looping over query results.
*   **Prepared Statements**: Full control over statement lifecycle for performance-critical code.
*   **In-Memory Databases**: Easy creation of in-memory databases for testing or temporary storage.
*   **Thread Safe**: A connection handle may be shared between threads. See [Threading](#threading).

## Installation

You can load Inquisitio via ASDF (the `:sqlite` system name still works for backward compatibility):

```bash
cd ~/.quicklisp/local-projects/
git clone https://github.com/quasi/inquisitio.git
```

```lisp
(ql:quickload :inquisitio)
;; or
(asdf:load-system :inquisitio)
;; or for backward compatibility:
(asdf:load-system :sqlite)
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

Inquisitio supports named parameters for better code readability. Use the `/named` variant of the functions.

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

`with-transaction` nests. The outermost form uses `BEGIN`/`COMMIT`/`ROLLBACK`;
inner forms use `SAVEPOINT`/`RELEASE`/`ROLLBACK TO`, so an inner failure discards
only the inner scope:

```lisp
(with-transaction *db*
  (execute-non-query *db* "INSERT INTO logs (message) VALUES (?)" "starting")
  (ignore-errors
    (with-transaction *db*                      ; a nested savepoint
      (execute-non-query *db* "INSERT INTO logs (message) VALUES (?)" "attempt")
      (error "this scope fails")))              ; only "attempt" is rolled back
  (execute-non-query *db* "INSERT INTO logs (message) VALUES (?)" "done"))
;; "starting" and "done" are committed.
```

An error escaping the *outer* form still discards everything, released inner
savepoints included.

### Using Iterate

If you use the `iterate` library, Inquisitio provides a driver for iterating over query results efficiently without loading everything into memory.

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

## Threading

**A `sqlite-handle` may be shared between threads.** Inquisitio guards its own
state, so no individual call needs a lock of your own.

What that means precisely:

| Concern | Who handles it |
|---|---|
| The prepared-statement cache and the handle's statement list | Inquisitio, via a recursive per-handle lock |
| Two threads calling `execute-*` on one handle at the same time | SQLite — connections are opened with `SQLITE_OPEN_FULLMUTEX` |
| `BEGIN`/`COMMIT`/`ROLLBACK` interleaving between threads | `with-transaction` holds the per-handle lock for its whole body |
| A `sqlite-statement` object shared between threads | **You.** Statements are not safe to use from two threads at once |
| A *sequence* of calls that depends on connection-global state | **You**, with `with-database-lock` |

That last row is the sharp edge. `last-insert-rowid` reads connection-global
state, so on a shared handle another thread's `INSERT` can land between yours
and your read:

```lisp
;; WRONG on a shared handle — the id may belong to another thread's insert
(execute-non-query *db* "INSERT INTO users (name) VALUES (?)" "alice")
(last-insert-rowid *db*)

;; Right
(with-database-lock (*db*)
  (execute-non-query *db* "INSERT INTO users (name) VALUES (?)" "alice")
  (last-insert-rowid *db*))
```

The same applies to any pair of calls where the second reads what the first
established — including `enable-load-extension` / `load-extension`.

### Transactions serialize per handle

`BEGIN`, `COMMIT` and `ROLLBACK` are connection-global in SQLite: without
exclusion, one thread's `ROLLBACK` silently destroys another thread's committed
work. `with-transaction` therefore holds the handle's lock for the entire body,
so **only one transaction runs on a handle at a time, and every other thread's
operation on that handle waits for it.** That is a deliberate trade: correctness
over concurrency. If it is too coarse for your workload, give each thread its own
connection — separate handles do not contend on a Lisp lock, only on SQLite's own
file locking (see `*default-busy-timeout*` below).

Beware the usual hazard: if a transaction body blocks on something another thread
needs, you can deadlock. Keep transaction bodies short and free of foreign locks.

### Making several statements atomic without a transaction

```lisp
(with-database-lock (*db*)
  (let ((id (execute-single *db* "SELECT MAX(id) FROM t")))
    (execute-non-query *db* "INSERT INTO t (id) VALUES (?)" (1+ id))))
```

The lock is recursive, so any Inquisitio call — including `with-transaction` — may
appear inside the body.

### Checking the linked library

```lisp
(sqlite-threadsafe)  ; => 0 single-thread, 1 serialized, 2 multi-thread
```

This reports the `SQLITE_THREADSAFE` value the *linked* libsqlite3 was compiled
with, which is not something Inquisitio can choose for you. If it returns `0`,
SQLite's mutexes were compiled out and no `open_v2` flag can bring them back: on
such a build do not share a handle between threads, whatever the rest of this
section says. `1` and `2` are both fine — Inquisitio passes `SQLITE_OPEN_FULLMUTEX`
when opening, which selects serialized mode per connection in either case.

This matters in practice: the system libsqlite3 on macOS 15 reports `2`, so the
`FULLMUTEX` flag is doing real work there, not making a point.

## API Reference

### Connection Management

*   **`connect`** `(path &key busy-timeout)`: Connects to the database at `path`. Use `":memory:"` for an in-memory DB. `busy-timeout` is in milliseconds and defaults to `*default-busy-timeout*`; pass `nil` to fail immediately on a locked database.
*   **`disconnect`** `(handle)`: Closes the database connection.
*   **`with-open-database`** `((var path &key busy-timeout) &body body)`: Context manager for database connections.
*   **`*default-busy-timeout*`**: Milliseconds `connect` waits on a locked database when `:busy-timeout` is not supplied. Defaults to `5000`.

### Concurrency

*   **`with-database-lock`** `((db) &body body)`: Runs `body` holding `db`'s recursive handle lock, excluding other threads from the handle.
*   **`handle-lock`** `(db)`: The recursive lock itself, should you need to compose with it.
*   **`sqlite-threadsafe`** `()`: The `SQLITE_THREADSAFE` mode of the linked SQLite library (0, 1, or 2).

### Query Execution

*   **`execute-non-query`** `(db sql &rest params)`: Executes a statement that returns no results (INSERT, UPDATE, etc).
*   **`execute-single`** `(db sql &rest params)`: Returns the first column of the first row.
*   **`execute-one-row-m-v`** `(db sql &rest params)`: Returns the first row as multiple values.
*   **`execute-to-list`** `(db sql &rest params)`: Returns the result set as a list of lists.
*   **`last-insert-rowid`** `(db)`: Returns the ID of the last inserted row.

*Named parameter variants:* `execute-non-query/named`, `execute-single/named`, `execute-one-row-m-v/named`, `execute-to-list/named`.

### Transactions

*   **`with-transaction`** `(db &body body)`: Executes body within a transaction. Commits on success, rolls back on error. Nests via `SAVEPOINT`. Holds the handle lock for the whole body — see [Threading](#threading).

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

## Security

The simplified interface (`simple.lisp`) parameterizes all data values through `?` bindings and validates all structural SQL elements (table names, column names, sort direction, limits). See [SQL Injection Analysis](docs/sql-injection-analysis.md) for a detailed assessment against the OWASP SQL Injection Prevention Cheat Sheet.

## Running Tests

To run the test suite, you need to load the `:sqlite-tests` system.

```lisp
(ql:quickload :sqlite-tests)
(sqlite-tests:run-all-sqlite-tests)
```

## Changelog
- Jul 2026 2.2 Thread safety
  - A `sqlite-handle` may now be shared between threads; see [Threading](#threading)
  - Statement cache and statement list guarded by a recursive per-handle lock
  - `with-transaction` holds that lock for its body, and nests via `SAVEPOINT`
  - Connections open with `sqlite3_open_v2` + `SQLITE_OPEN_FULLMUTEX`
  - `connect` defaults to a 5-second busy timeout (`*default-busy-timeout*`); pass `:busy-timeout nil` for the old fail-fast behaviour
  - New: `with-database-lock`, `handle-lock`, `sqlite-threadsafe`
- Feb 2026 2.1 Input validation hardening for simplified interface ([details](docs/sql-injection-analysis.md))
  - `normalize-name` validates identifiers and converts hyphens to underscores
  - ORDER BY direction allowlisted to `:asc` / `:desc`
  - LIMIT and OFFSET type-checked as non-negative integers
- Jan 2026 2.0 New Fork

## Licence of this Fork

MIT

## History from the original repo

* 9 Feb 2026 - I changed the name from cl-sqlite -> Inquisitio as this fork and the original cl-sqlite had drifted apart a lot. There were some folks on Reddit who thought that my fork was the 'wrong' direction. The library was public domain. I have changed the licence to MIT and I have maintained the git history as well as the original changelog in this file. My sincere thanks go to the original developers for their efforts.

Original Repo: https://github.com/TeMPOraL/cl-sqlite

### Support
This package is written by Kalyanov Dmitry.
This project has a cl-sqlite-devel mailing list.


### Historic Changelog
- 23 Jan 2009 0.1 Initial version
- 03 Mar 2009 0.1.1 Fixed bug with access to recently freed memory during statement preparation
- 22 Mar 2009 0.1.2 disconnect function now ensures that all non-finalized statements are finalized before closing the database (otherwise errors are signaled when database is being closed).
- 28 Apr 2009 0.1.3 Added support for passing all values of type REAL (including RATIONAL) as query parameter. cl-sqlite is made available as git repository.
- 10 May 2009 0.1.4 Added test suite (based on FiveAM testing framework); changed foreign library definition to work on Mac OS X (thanks to Patrick Stein) and removed the dependency on sqlite3_next_stmt function that appeared only in sqlite 3.6.0 (making cl-sqlite work with older sqlite versions)
- 13 June 2009 0.1.5 Allow passing pathnames to CONNECT function.
- 24 Oct 2009 0.1.6 Add busy-timeout argument to CONNECT. Fix library defininitions for running on Microsoft Windows.
- 14 Nov 2010 0.2 Added support for named parameters. Made statement reset and connection close more safe by clearing statements' bindings and unbinding slot of connection object. Added error condition for SQLite errors. Changes are courtesy of Alexander Gavrilov.


## Original License

Public Domain.
