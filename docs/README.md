# CL-SQLite Documentation

Welcome to CL-SQLite — a Common Lisp interface to SQLite databases.

## What is CL-SQLite?

CL-SQLite provides a Lispy way to interact with SQLite databases. It offers multiple API layers, from simple high-level functions for common CRUD operations to low-level prepared statement access for maximum control.

## Quick Start

### Installation

```lisp
(asdf:load-system :sqlite)
```

### Your First Database

```common-lisp
(with-open-database (db "/tmp/example.db")
  ;; Create a table
  (create-table db :users
                '((:id :integer :primary-key)
                  (:name :text :not-null)
                  (:age :integer)))

  ;; Insert data
  (insert db :users '(:name "Alice" :age 30))
  (insert db :users '(:name "Bob" :age 25))

  ;; Query the data
  (select db :users :where '(:> :age 26)))
```

## Documentation Map

| Level | For | Guide |
|-------|-----|-------|
| **Beginner** | Learning the basics | [Quickstart →](./quickstart.md) |
| **Developer** | Building applications | [Tutorials →](./tutorials/) |
| **Reference** | API documentation | [Reference →](./reference/) |
| **Concepts** | Understanding design | [Concepts →](./concepts/) |
| **Advanced** | Performance & control | [Advanced →](./advanced.md) |

## The Three API Layers

### 1. Simplified Interface (Recommended for most use)

High-level Lispy functions for CRUD operations:

```common-lisp
(select db :users :where '(:and (:> :age 18) (:= :active 1)))
(insert db :users '(:name "Charlie" :age 35))
(update-table db :users '(:active 0) :where '(:< :age 18))
(delete-from db :users :where '(:is-null :email))
```

**Good for:** Basic CRUD, simple queries, SQL injection prevention

**See:** [Simplified Interface Reference](./reference/simplified-api.md)

### 2. Standard Interface (Most SQL queries)

Mid-level functions that accept SQL strings with parameters:

```common-lisp
(execute-to-list db "SELECT * FROM users WHERE age > ? AND active = ?" 18 1)
(execute-single db "SELECT count(*) FROM users")
(execute-non-query db "DELETE FROM logs WHERE created < ?")
```

**Good for:** Complex queries, migrations, full SQL control

**See:** [Standard Interface Reference](./reference/standard-api.md)

### 3. Prepared Statement API (Maximum control)

Low-level direct statement management:

```common-lisp
(let ((stmt (prepare-statement db "SELECT name, age FROM users WHERE age > ?")))
  (bind-parameter stmt 1 25)
  (loop while (step-statement stmt)
        collect (list (statement-column-value stmt 0)
                      (statement-column-value stmt 1)))
  (finalize-statement stmt))
```

**Good for:** Performance-critical code, statement reuse, streaming

**See:** [Prepared Statement Reference](./reference/prepared-api.md)

## Key Features

### SQL Injection Prevention

The simplified API never concatenates SQL:

```common-lisp
;; Safe: User input never enters SQL
(select db :users :where `(:= :email ,user-input))

;; All values are parameterized
```

### Statement Caching

Prepared statements are automatically cached (size: 16). Repeat queries reuse cached statements for better performance.

### Transaction Support

```common-lisp
(with-transaction (db)
  (insert db :users '(:name "Alice"))
  (insert db :users '(:name "Bob"))
  ;; Auto-commit on success, rollback on error
)
```

### Vector Extensions

For semantic search and embeddings:

```common-lisp
(create-vector-table db :embeddings :dim 384)
(insert db :embeddings
        '(:id 1 :vector ,(float-vector-to-blob embedding-data)))
(vector-search db :embeddings query-vector :k 10)
```

## Common Patterns

### Checking if a table exists

```common-lisp
(execute-single db "SELECT name FROM sqlite_master WHERE type='table' AND name=?" :table-name)
```

### Iterating over results

```common-lisp
(let ((rows (execute-to-list db "SELECT * FROM users")))
  (dolist (row rows)
    (format t "~a~%" row)))
```

### Getting the last inserted row ID

```common-lisp
(insert db :users '(:name "Alice"))
(execute-single db "SELECT last_insert_rowid()")
```

### Batch inserts with transactions

```common-lisp
(with-transaction (db)
  (dolist (user user-list)
    (insert db :users user)))
```

## Getting Help

- **Questions?** See [FAQ](./faq.md) or [Troubleshooting](./troubleshooting.md)
- **Bug reports:** [GitHub Issues](https://github.com/quasi/cl-sqlite/issues)
- **Source code:** [GitHub Repository](https://github.com/quasi/cl-sqlite)

## Next Steps

1. [Read the quickstart](./quickstart.md) (5 min)
2. [Try the tutorials](./tutorials/) (15 min each)
3. [Reference guide](./reference/) for specific tasks

---

**Version:** 1.0+
**License:** MIT
**Status:** Production-ready
