# Tutorial: CRUD Operations

Learn the basics of Create, Read, Update, Delete operations with CL-SQLite.

## Prerequisites

- Completed [Quickstart](../quickstart.md)
- Basic understanding of Lisp lists and plists

## Create: Setting Up Your Data Model

### Define a Table Schema

```common-lisp
(asdf:load-system :sqlite)

(sqlite:with-open-database (db "/tmp/blog.db")
  (sqlite:create-table db :posts
    '((:id :integer :primary-key :autoincrement)
      (:title :text :not-null)
      (:content :text)
      (:author :text)
      (:published :integer :default 0)
      (:created-at :integer)))
  (format t "Table created!~%"))
```

**Output:** `Table created!`

**What happened:**
- `:id` - Auto-incrementing primary key
- `:title` - Required text field
- `:content` - Optional text field
- `:published` - Boolean (1/0), defaults to unpublished
- `:created-at` - Timestamp (Unix epoch)

## Read: Querying Your Data

### Insert Sample Data

```common-lisp
(sqlite:with-open-database (db "/tmp/blog.db")
  (sqlite:insert db :posts
                '(:title "Hello World"
                  :content "My first post"
                  :author "Alice"
                  :published 1
                  :created-at 1642000000))
  (sqlite:insert db :posts
                '(:title "Lisp Tips"
                  :content "Why I love Lisp"
                  :author "Bob"
                  :published 1
                  :created-at 1642100000))
  (sqlite:insert db :posts
                '(:title "Draft Post"
                  :content "Work in progress..."
                  :author "Alice"
                  :published 0
                  :created-at 1642200000))
  (format t "Data inserted!~%"))
```

**Output:** `Data inserted!`

### Select All Rows

```common-lisp
(sqlite:with-open-database (db "/tmp/blog.db")
  (let ((posts (sqlite:select db :posts)))
    (format t "All posts (~d):~%" (length posts))
    (dolist (post posts)
      (format t "  ~a~%" post))))
```

**Output:**
```
All posts (3):
  (1 "Hello World" "My first post" "Alice" 1 1642000000)
  (2 "Lisp Tips" "Why I love Lisp" "Bob" 1 1642100000)
  (3 "Draft Post" "Work in progress..." "Alice" 0 1642200000)
```

### Select with WHERE Clause

```common-lisp
(sqlite:with-open-database (db "/tmp/blog.db")
  ;; Get published posts
  (let ((published (sqlite:select db :posts
                                  :where '(:= :published 1))))
    (format t "Published posts: ~a~%" published))

  ;; Get posts by Alice
  (let ((alice-posts (sqlite:select db :posts
                                    :where '(:= :author "Alice"))))
    (format t "Alice's posts: ~a~%" alice-posts))

  ;; Get Alice's published posts
  (let ((results (sqlite:select db :posts
                                :where '(:and (:= :author "Alice")
                                             (:= :published 1)))))
    (format t "Alice's published: ~a~%" results)))
```

**Output:**
```
Published posts: ((1 "Hello World" "My first post" "Alice" 1 1642000000) (2 "Lisp Tips" "Why I love Lisp" "Bob" 1 1642100000))
Alice's posts: ((1 "Hello World" "My first post" "Alice" 1 1642000000) (3 "Draft Post" "Work in progress..." "Alice" 0 1642200000))
Alice's published: ((1 "Hello World" "My first post" "Alice" 1 1642000000))
```

### Select with ORDER BY

```common-lisp
(sqlite:with-open-database (db "/tmp/blog.db")
  ;; Newest first
  (let ((posts (sqlite:select db :posts
                              :order-by '(:created-at :desc))))
    (format t "Newest posts first:~%")
    (dolist (post posts)
      (format t "  ~a~%" (second post)))))
```

**Output:**
```
Newest posts first:
  Draft Post
  Lisp Tips
  Hello World
```

### Select with LIMIT

```common-lisp
(sqlite:with-open-database (db "/tmp/blog.db")
  ;; Get first 2 posts
  (let ((posts (sqlite:select db :posts :limit 2)))
    (format t "First 2 posts: ~d~%" (length posts))))
```

**Output:** `First 2 posts: 2`

## Update: Modifying Existing Data

### Update a Specific Post

```common-lisp
(sqlite:with-open-database (db "/tmp/blog.db")
  ;; Publish the draft post
  (sqlite:update-table db :posts
                       '(:published 1)
                       :where '(:= :id 3))
  (format t "Post published!~%"))
```

### Update Multiple Posts

```common-lisp
(sqlite:with-open-database (db "/tmp/blog.db")
  ;; Update all of Bob's posts
  (sqlite:update-table db :posts
                       '(:published 0)
                       :where '(:= :author "Bob"))
  (format t "Bob's posts unpublished!~%"))
```

### Verify Updates

```common-lisp
(sqlite:with-open-database (db "/tmp/blog.db")
  (let ((post-3 (car (sqlite:select db :posts :where '(:= :id 3)))))
    (format t "Post 3 published: ~a~%" (fifth post-3))))
```

**Output:** `Post 3 published: 1`

## Delete: Removing Data

### Delete a Specific Post

```common-lisp
(sqlite:with-open-database (db "/tmp/blog.db")
  (sqlite:delete-from db :posts :where '(:= :id 2))
  (format t "Post deleted!~%")
  (let ((count (length (sqlite:select db :posts))))
    (format t "Remaining posts: ~a~%" count)))
```

**Output:**
```
Post deleted!
Remaining posts: 2
```

### Delete with Complex WHERE

```common-lisp
(sqlite:with-open-database (db "/tmp/blog.db")
  ;; Delete all unpublished posts
  (sqlite:delete-from db :posts
                      :where '(:= :published 0))
  (format t "Unpublished posts deleted!~%"))
```

## Full CRUD Example

Here's a complete example combining all operations:

```common-lisp
(asdf:load-system :sqlite)

(defun run-crud-demo ()
  (sqlite:with-open-database (db "/tmp/demo-crud.db")
    ;; CREATE
    (format t "~&=== CREATE ===~%")
    (sqlite:create-table db :products
      '((:id :integer :primary-key :autoincrement)
        (:name :text :not-null)
        (:price :real)
        (:stock :integer :default 0)))
    (format t "Table created~%")

    ;; INSERT
    (format t "~&=== INSERT ===~%")
    (sqlite:insert db :products '(:name "Keyboard" :price 79.99 :stock 10))
    (sqlite:insert db :products '(:name "Mouse" :price 29.99 :stock 25))
    (sqlite:insert db :products '(:name "Monitor" :price 299.99 :stock 5))
    (format t "3 products inserted~%")

    ;; SELECT
    (format t "~&=== SELECT ===~%")
    (let ((products (sqlite:select db :products)))
      (format t "Total products: ~a~%" (length products))
      (dolist (p products)
        (format t "  ~a: $~a (stock: ~a)~%" (second p) (third p) (fourth p))))

    ;; SELECT WHERE
    (format t "~&=== SELECT WHERE ===~%")
    (let ((expensive (sqlite:select db :products :where '(:> :price 100))))
      (format t "Expensive products (>$100): ~a~%" (length expensive))
      (dolist (p expensive)
        (format t "  ~a~%" (second p))))

    ;; UPDATE
    (format t "~&=== UPDATE ===~%")
    (sqlite:update-table db :products '(:stock 20) :where '(:= :id 1))
    (format t "Updated keyboard stock~%")

    ;; DELETE
    (format t "~&=== DELETE ===~%")
    (sqlite:delete-from db :products :where '(:< :stock 8))
    (format t "Deleted low-stock items~%")

    ;; FINAL COUNT
    (format t "~&=== FINAL ===~%")
    (let ((remaining (sqlite:select db :products)))
      (format t "Final product count: ~a~%" (length remaining)))))

(run-crud-demo)
```

**Output:**
```
=== CREATE ===
Table created

=== INSERT ===
3 products inserted

=== SELECT ===
Total products: 3
  Keyboard: $79.99 (stock: 10)
  Mouse: $29.99 (stock: 25)
  Monitor: $299.99 (stock: 5)

=== SELECT WHERE ===
Expensive products (>$100): 1
  Monitor

=== UPDATE ===
Updated keyboard stock

=== DELETE ===
Deleted low-stock items

=== FINAL ===
Final product count: 2
```

## Common Mistakes

### Mistake 1: Forgetting WHERE in UPDATE/DELETE

```common-lisp
;; WRONG - updates ALL rows!
(sqlite:update-table db :users '(:active 0))

;; RIGHT - updates specific rows
(sqlite:update-table db :users '(:active 0) :where '(:= :id 1))
```

### Mistake 2: Using Strings Instead of Keywords for WHERE

```common-lisp
;; WRONG - string comparison
(sqlite:select db :users :where '(:= "age" 30))

;; RIGHT - keyword for column name
(sqlite:select db :users :where '(:= :age 30))
```

### Mistake 3: Wrong Operator Syntax

```common-lisp
;; WRONG - operator misplaced
(sqlite:select db :users :where '(:and :age :> 18))

;; RIGHT - operator first
(sqlite:select db :users :where '(:and (:> :age 18) ...))
```

## Next Steps

- [Tutorial: Transactions](./02-transactions.md)
- [Tutorial: Prepared Statements](./03-prepared-statements.md)
- [Reference: Simplified API](../reference/simplified-api.md)

---

**Status:** ✓ Complete
**Time:** ~30 minutes
**Next:** [Transactions Tutorial](./02-transactions.md)
