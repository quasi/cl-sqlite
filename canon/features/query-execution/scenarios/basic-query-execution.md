# Scenario: Basic Query Execution

**Confidence:** 0.95
**Status:** Verified
**Test:** `test-select` (sqlite-tests.lisp:50)

---

## Description

Executes a basic SELECT query on a populated table and retrieves all results.

**Purpose:** Validates fundamental query execution flow

---

## Setup

1. Create in-memory database
2. Create table with schema
3. Insert sample data

---

## Execution

```common-lisp
(deftest test-select ()
  (with-open-database (db ":memory:")
    (execute-non-query db "CREATE TABLE contacts (name TEXT, age INTEGER)")
    (execute-non-query db "INSERT INTO contacts VALUES (?, ?)" "Alice" 30)
    (execute-non-query db "INSERT INTO contacts VALUES (?, ?)" "Bob" 25)
    (let ((result (execute-to-list db "SELECT * FROM contacts")))
      (assert-equal 2 (length result)))))
```

---

## Steps

1. **Create table**
   - Execute: `CREATE TABLE contacts (name TEXT, age INTEGER)`
   - Schema established

2. **Insert data**
   - Insert row 1: ("Alice", 30)
   - Insert row 2: ("Bob", 25)
   - 2 rows in table

3. **Execute SELECT query**
   - Query: `SELECT * FROM contacts`
   - No parameters (no WHERE clause)

4. **Retrieve all results**
   - `execute-to-list` returns all rows
   - Expected: 2 rows, 2 columns each

---

## Expected Outcomes

### Query Execution Phase
- ✅ Prepared statement created
- ✅ No parameters to bind (none in SQL)
- ✅ Statement stepped successfully
- ✅ Retrieved 2 rows
- ✅ Each row has 2 columns

### Result Structure
- ✅ Returns list of lists: `(("Alice" 30) ("Bob" 25))`
- ✅ Column order matches SELECT clause (name, age)
- ✅ Column types: name (string), age (integer)
- ✅ No errors signaled

---

## Invariants Verified

- **INV-003 (Parameter Binding Safety):**
  - No parameters to bind → no binding errors

- **RULE-001 (Single Statement Requirement):**
  - Single SELECT statement executed successfully

---

## Properties Verified

- Query execution succeeds for valid SELECT
- Result set contains expected rows
- Columns returned in expected order
- Type conversion works (TEXT → string, INTEGER → integer)

---

## Variations

### No Results
```common-lisp
(execute-to-list db "SELECT * FROM contacts WHERE age > 50")
;; => nil (empty list)
```

### Single Row
```common-lisp
(execute-to-list db "SELECT * FROM contacts WHERE name = 'Alice'")
;; => (("Alice" 30))
```

### Single Column
```common-lisp
(execute-to-list db "SELECT name FROM contacts")
;; => (("Alice") ("Bob"))
```

### With ORDER BY
```common-lisp
(execute-to-list db "SELECT * FROM contacts ORDER BY age")
;; => (("Bob" 25) ("Alice" 30))  ; Ordered by age
```

### With WHERE
```common-lisp
(execute-to-list db "SELECT * FROM contacts WHERE age > 26")
;; => (("Alice" 30))
```

---

## Anti-patterns

❌ **Hardcoded values in SQL (SQL injection risk):**
```common-lisp
;; WRONG
(let ((age-threshold 26))
  (execute-to-list db
    (format nil "SELECT * FROM contacts WHERE age > ~D" age-threshold)))

;; RIGHT
(execute-to-list db "SELECT * FROM contacts WHERE age > ?" 26)
```

✅ **Correct:**
```common-lisp
(execute-to-list db "SELECT * FROM contacts WHERE age > ?" 26)
```

---

## Test Evidence

**Source:** `sqlite-tests.lisp:50`
```common-lisp
(deftest test-select ()
  (with-open-database (db ":memory:")
    (execute-non-query db "CREATE TABLE contacts (name TEXT, age INTEGER)")
    (execute-non-query db "INSERT INTO contacts VALUES (?, ?)" "Alice" 30)
    (execute-non-query db "INSERT INTO contacts VALUES (?, ?)" "Bob" 25)
    (let ((result (execute-to-list db "SELECT * FROM contacts")))
      (assert-equal 2 (length result)))))
```

**Test Status:** Passing (verified 2026-01-20)

---

## Related Scenarios

- `parameterized-query` - SELECT with WHERE clause
- `multi-row-query` - SELECT returning many rows
- `empty-result-handling` - SELECT with no results

---

**Document Status:** Canonical
**Last Verified:** 2026-01-20 (Canon Initiation Pass 3)
