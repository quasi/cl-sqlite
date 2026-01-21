# Quickstart: Your First SQLite Database

Get a working SQLite application in under 5 minutes.

## Prerequisites

- SBCL, CCL, or another Common Lisp implementation
- Quicklisp or ASDF configured
- Basic Lisp knowledge

## Step 1: Load the Library

```common-lisp
(asdf:load-system :sqlite)
```

You should see no errors.

## Step 2: Create and Open a Database

```common-lisp
(sqlite:with-open-database (db "/tmp/demo.db")
  (format t "Database opened!~%"))
```

**Expected output:**
```
Database opened!
```

The `with-open-database` macro automatically closes the database when done.

## Step 3: Create a Table

```common-lisp
(sqlite:with-open-database (db "/tmp/demo.db")
  (sqlite:create-table db :users
    '((:id :integer :primary-key)
      (:name :text :not-null)
      (:email :text)))
  (format t "Table created!~%"))
```

**Expected output:**
```
Table created!
```

## Step 4: Insert Data

```common-lisp
(sqlite:with-open-database (db "/tmp/demo.db")
  (sqlite:insert db :users '(:name "Alice" :email "alice@example.com"))
  (sqlite:insert db :users '(:name "Bob" :email "bob@example.com"))
  (format t "Data inserted!~%"))
```

**Expected output:**
```
Data inserted!
```

## Step 5: Query the Data

```common-lisp
(sqlite:with-open-database (db "/tmp/demo.db")
  (let ((results (sqlite:select db :users)))
    (format t "Query results:~%")
    (dolist (row results)
      (format t "  ~a~%" row))))
```

**Expected output:**
```
Query results:
  (1 "Alice" "alice@example.com")
  (2 "Bob" "bob@example.com")
```

## Step 6: Query with WHERE

```common-lisp
(sqlite:with-open-database (db "/tmp/demo.db")
  (let ((results (sqlite:select db :users
                                :where '(:= :name "Alice"))))
    (dolist (row results)
      (format t "Found: ~a~%" row))))
```

**Expected output:**
```
Found: (1 "Alice" "alice@example.com")
```

## Verify It Works

Run this complete example:

```common-lisp
(asdf:load-system :sqlite)

(sqlite:with-open-database (db "/tmp/test.db")
  ;; Create
  (sqlite:create-table db :items
    '((:id :integer :primary-key)
      (:name :text)))

  ;; Insert
  (sqlite:insert db :items '(:name "Item 1"))
  (sqlite:insert db :items '(:name "Item 2"))

  ;; Query
  (let ((count (sqlite:execute-single db "SELECT count(*) FROM items")))
    (format t "Total items: ~a~%" (car count))))
```

**Expected output:**
```
Total items: 2
```

## What's Next?

- [Tutorial: CRUD Operations](./tutorials/01-crud.md)
- [Tutorial: Complex Queries](./tutorials/02-complex-queries.md)
- [Full API Reference](./reference/)

---

**Status:** ✓ Complete
**Time:** ~5 minutes
**Next Step:** [CRUD Tutorial](./tutorials/01-crud.md)
