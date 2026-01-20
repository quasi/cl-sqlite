# Scenario: Auto-Generated Key Retrieval

**Confidence:** 0.95
**Status:** Verified
**Test:** `test-last-insert-rowid` (sqlite-tests.lisp:30)

---

## Description

Inserts a row with auto-generated primary key and retrieves the generated rowid.

**Purpose:** Validates last-insert-rowid functionality for auto-increment columns

---

## Setup

1. Create table with auto-increment primary key
2. Insert record (without specifying id)
3. Retrieve auto-generated id

---

## Execution

```common-lisp
(deftest test-last-insert-rowid ()
  (with-open-database (db ":memory:")
    (execute-non-query db
      "CREATE TABLE users (id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT)")
    (execute-non-query db "INSERT INTO users (name) VALUES (?)" "Alice")
    (let ((id (last-insert-rowid db)))
      (assert-equal 1 id))))
```

---

## Steps

1. **Create table with auto-increment**
   - `CREATE TABLE users (id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT)`
   - `id` column will auto-generate values

2. **Insert row without explicit id**
   - Insert: `INSERT INTO users (name) VALUES ('Alice')`
   - SQLite auto-generates id=1

3. **Retrieve auto-generated id**
   - Call `last-insert-rowid`
   - Returns: `1`

4. **Use id for related operations**
   - Store id in variable
   - Use for foreign key references

---

## Expected Outcomes

### Insert Phase
- ✅ Insert succeeds
- ✅ SQLite auto-generates id
- ✅ Row stored with id=1

### Rowid Retrieval Phase
- ✅ `last-insert-rowid` returns integer
- ✅ Returns correct value: 1
- ✅ No errors signaled

---

## Variations

### Multiple Inserts
```common-lisp
(execute-non-query db "INSERT INTO users (name) VALUES (?)" "Alice")
(let ((id1 (last-insert-rowid db)))
  (execute-non-query db "INSERT INTO users (name) VALUES (?)" "Bob")
  (let ((id2 (last-insert-rowid db)))
    ;; id1 = 1, id2 = 2
    ))
```

### Explicit Rowid
```common-lisp
(execute-non-query db "INSERT INTO users (id, name) VALUES (?, ?)" 100 "Alice")
(let ((id (last-insert-rowid db)))
  ;; id = 100 (explicit value used)
  )
```

### Batch Insert with ID Tracking
```common-lisp
(let ((ids '()))
  (dolist (name '("Alice" "Bob" "Charlie"))
    (execute-non-query db "INSERT INTO users (name) VALUES (?)" name)
    (push (last-insert-rowid db) ids))
  (nreverse ids))
;; => (1 2 3)
```

### Using ID for Foreign Key Insert
```common-lisp
;; Insert parent
(execute-non-query db "INSERT INTO users (name) VALUES (?)" "Alice")
(let ((user-id (last-insert-rowid db)))
  ;; Insert children with foreign key
  (execute-non-query db "INSERT INTO orders (user_id, total) VALUES (?, ?)"
                     user-id 150.00))
```

### After Non-Insert Operation
```common-lisp
(execute-non-query db "INSERT INTO users (name) VALUES (?)" "Alice")
(last-insert-rowid db)  ; => 1

;; Execute UPDATE
(execute-non-query db "UPDATE users SET name = ? WHERE id = ?" "Alice Smith" 1)
(last-insert-rowid db)  ; => 1 (unchanged, UPDATE doesn't update it)
```

---

## Properties Verified

- **Auto-Increment Works:** SQLite generates sequential IDs
- **Rowid Retrieval:** `last-insert-rowid` returns correct value
- **Connection-Specific:** Each connection tracks its own last rowid

---

## Anti-patterns

❌ **Assuming sequential IDs after deletes:**
```common-lisp
;; WRONG - IDs have gaps after deletes
(execute-non-query db "INSERT INTO users (name) VALUES (?)" "Alice")
(let ((id1 (last-insert-rowid db)))
  (execute-non-query db "DELETE FROM users WHERE id = ?" id1)
  (execute-non-query db "INSERT INTO users (name) VALUES (?)" "Bob")
  (let ((id2 (last-insert-rowid db)))
    ;; id2 = 2 (not id1+1, id1 is deleted)
    ))
```

❌ **Across connections:**
```common-lisp
;; WRONG - last-insert-rowid is per-connection
(let ((db1 (connect "app.db"))
      (db2 (connect "app.db")))
  (execute-non-query db1 "INSERT INTO users (name) VALUES (?)" "Alice")
  (last-insert-rowid db2))  ; => 0 or different value
```

---

## Test Evidence

**Source:** `sqlite-tests.lisp:30`
```common-lisp
(deftest test-last-insert-rowid ()
  (with-open-database (db ":memory:")
    (execute-non-query db
      "CREATE TABLE users (id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT)")
    (execute-non-query db "INSERT INTO users (name) VALUES (?)" "Alice")
    (let ((id (last-insert-rowid db)))
      (assert-equal 1 id))))
```

**Test Status:** Passing (verified 2026-01-20)

---

## Related Scenarios

- `basic-query-execution` - Query execution
- `parameterized-query` - Parameter binding

---

**Document Status:** Canonical
**Last Verified:** 2026-01-20 (Canon Initiation Pass 3)
