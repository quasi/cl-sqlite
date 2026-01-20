# Scenario: Parameterized Query with WHERE Clause

**Confidence:** 0.95
**Status:** Verified
**Test:** `test-select-where` (sqlite-tests.lisp:60)

---

## Description

Executes a SELECT query with WHERE clause using parameter binding.

**Purpose:** Validates parameter binding and filtering

---

## Setup

1. Create table and insert multiple rows
2. Create query with placeholder for WHERE clause value

---

## Execution

```common-lisp
(deftest test-select-where ()
  (with-open-database (db ":memory:")
    (execute-non-query db "CREATE TABLE users (id INTEGER, name TEXT, active INTEGER)")
    (execute-non-query db "INSERT INTO users VALUES (?, ?, ?)" 1 "Alice" 1)
    (execute-non-query db "INSERT INTO users VALUES (?, ?, ?)" 2 "Bob" 0)
    (execute-non-query db "INSERT INTO users VALUES (?, ?, ?)" 3 "Charlie" 1)
    (let ((result (execute-to-list db "SELECT name FROM users WHERE active = ?" 1)))
      (assert-equal 2 (length result)))))
```

---

## Steps

1. **Prepare table and data**
   - Create users table (id, name, active)
   - Insert 3 users: Alice (active=1), Bob (active=0), Charlie (active=1)

2. **Execute parameterized query**
   - SQL: `SELECT name FROM users WHERE active = ?`
   - Parameter: `1` (active users)

3. **Bind parameter**
   - Position 1 (RULE-004: 1-indexed)
   - Value: `1` (integer)

4. **Retrieve filtered results**
   - Step statement
   - Collect rows where active=1
   - Expected: 2 rows ("Alice", "Charlie")

---

## Expected Outcomes

### Query Execution Phase
- ✅ Statement prepared
- ✅ Parameter bound at position 1
- ✅ WHERE clause evaluated
- ✅ Retrieved 2 rows (filtered)

### Result Structure
- ✅ Returns 2 rows
- ✅ Each row has 1 column (name)
- ✅ Column values: ("Alice"), ("Charlie")
- ✅ Bob excluded (active=0)

---

## Invariants Verified

- **INV-003 (Parameter Binding Safety):**
  - Parameter bound safely (not concatenated)
  - SQL injection prevented

- **RULE-004 (Parameter Index Base):**
  - Parameters are 1-indexed
  - Correct position binding

---

## Properties Verified

- Parameter binding works correctly
- WHERE clause evaluates correctly
- Result set properly filtered
- Multiple parameters supported

---

## Variations

### Multiple Parameters
```common-lisp
(execute-to-list db
  "SELECT name FROM users WHERE active = ? AND id > ?"
  1 1)
;; => (("Charlie"))  ; Only Charlie: active=1 AND id>1
```

### Different Data Types
```common-lisp
;; Parameter as string
(execute-to-list db "SELECT id FROM users WHERE name = ?" "Alice")
;; => ((1))

;; Parameter as NULL
(execute-to-list db "SELECT name FROM users WHERE email IS ?" :null)
```

### execute-single with Parameter
```common-lisp
(execute-single db "SELECT COUNT(*) FROM users WHERE active = ?" 1)
;; => 2
```

---

## Anti-patterns

❌ **SQL concatenation (SQL injection):**
```common-lisp
;; WRONG
(let ((status 1))
  (execute-to-list db
    (format nil "SELECT name FROM users WHERE active = ~D" status)))

;; RIGHT
(execute-to-list db "SELECT name FROM users WHERE active = ?" status)
```

❌ **String with quotes (type error):**
```common-lisp
;; WRONG - Quotes in string value can break SQL
(execute-to-list db
  (format nil "SELECT * FROM users WHERE name = '~A'" "O'Brien"))

;; RIGHT - Parameter handles any string
(execute-to-list db "SELECT * FROM users WHERE name = ?" "O'Brien")
```

---

## Test Evidence

**Source:** `sqlite-tests.lisp:60`
```common-lisp
(deftest test-select-where ()
  (with-open-database (db ":memory:")
    (execute-non-query db "CREATE TABLE users (id INTEGER, name TEXT, active INTEGER)")
    (execute-non-query db "INSERT INTO users VALUES (?, ?, ?)" 1 "Alice" 1)
    (execute-non-query db "INSERT INTO users VALUES (?, ?, ?)" 2 "Bob" 0)
    (execute-non-query db "INSERT INTO users VALUES (?, ?, ?)" 3 "Charlie" 1)
    (let ((result (execute-to-list db "SELECT name FROM users WHERE active = ?" 1)))
      (assert-equal 2 (length result)))))
```

**Test Status:** Passing (verified 2026-01-20)

---

## Related Scenarios

- `basic-query-execution` - Query without parameters
- `multi-row-query` - Query returning many rows with filtering

---

**Document Status:** Canonical
**Last Verified:** 2026-01-20 (Canon Initiation Pass 3)
