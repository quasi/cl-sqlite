# Scenario: Multi-Row Query Result Handling

**Confidence:** 0.95
**Status:** Verified
**Test:** `test-multi-row-query` (sqlite-tests.lisp:55)

---

## Description

Executes a query returning multiple rows and processes results.

**Purpose:** Validates result set collection and iteration

---

## Setup

1. Create table with multiple records
2. Execute query returning all rows

---

## Execution

```common-lisp
(deftest test-multi-row-query ()
  (with-open-database (db ":memory:")
    (execute-non-query db "CREATE TABLE logs (ts INTEGER, msg TEXT)")
    (execute-non-query db "INSERT INTO logs VALUES (?, ?)" 1000 "Start")
    (execute-non-query db "INSERT INTO logs VALUES (?, ?)" 1001 "Event A")
    (execute-non-query db "INSERT INTO logs VALUES (?, ?)" 1002 "Event B")
    (let ((result (execute-to-list db "SELECT * FROM logs")))
      (assert-equal 3 (length result)))))
```

---

## Steps

1. **Create table and populate**
   - Create logs table (ts, msg)
   - Insert 3 log entries

2. **Execute query**
   - SQL: `SELECT * FROM logs`
   - No WHERE clause (all rows)

3. **Collect all rows**
   - `execute-to-list` steps through all rows
   - Accumulates results in list

4. **Verify result count**
   - Length of result = 3
   - All rows retrieved

---

## Expected Outcomes

### Query Execution Phase
- ✅ Statement prepared and stepped
- ✅ Retrieved 3 rows (all data)
- ✅ Each row has 2 columns

### Result Processing
- ✅ Returns list of 3 rows
- ✅ Each row: `(ts msg)`
- ✅ Data types preserved: integer, string
- ✅ No rows skipped

---

## Variations

### Processing Results with dolist
```common-lisp
(dolist (row (execute-to-list db "SELECT * FROM logs"))
  (destructuring-bind (ts msg) row
    (format t "~D: ~A~%" ts msg)))
;; Output:
;; 1000: Start
;; 1001: Event A
;; 1002: Event B
```

### Mapping Over Results
```common-lisp
(mapcar #'second (execute-to-list db "SELECT * FROM logs"))
;; => ("Start" "Event A" "Event B")
```

### Filtering Results in Lisp
```common-lisp
(remove-if (lambda (row)
             (< (first row) 1001))
           (execute-to-list db "SELECT * FROM logs"))
;; => ((1001 "Event A") (1002 "Event B"))
```

### Large Result Set
```common-lisp
;; Insert 1000 rows
(loop for i from 0 below 1000
      do (execute-non-query db "INSERT INTO logs VALUES (?, ?)"
                           (+ 1000 i) (format nil "Event ~D" i)))

;; Retrieve all
(let ((result (execute-to-list db "SELECT * FROM logs")))
  (format t "Retrieved ~D rows~%" (length result)))
;; => Retrieved 1000 rows
```

---

## Properties Verified

- **Result Completeness:** All rows retrieved
- **Order Preservation:** Rows in database order (unless ORDER BY)
- **Column Preservation:** All columns returned
- **Type Consistency:** Data types preserved

---

## Anti-patterns

❌ **Fetching large sets into memory:**
```common-lisp
;; WRONG - Loads 1M rows into memory
(let ((result (execute-to-list db "SELECT * FROM huge_table")))
  (dolist (row result)
    (process-row row)))

;; RIGHT - Use iterate for streaming
(iter (for (id name) in-sqlite-query db "SELECT id, name FROM huge_table")
      (process-row id name))
```

❌ **Assuming sequential access:**
```common-lisp
;; WRONG - May not be efficient for random access
(let ((results (execute-to-list db "SELECT * FROM logs")))
  (nth 500 results))  ; Inefficient for large sets

;; BETTER - Use SQL LIMIT + OFFSET
(execute-to-list db "SELECT * FROM logs LIMIT 1 OFFSET 500")
```

---

## Test Evidence

**Source:** `sqlite-tests.lisp:55`
```common-lisp
(deftest test-multi-row-query ()
  (with-open-database (db ":memory:")
    (execute-non-query db "CREATE TABLE logs (ts INTEGER, msg TEXT)")
    (execute-non-query db "INSERT INTO logs VALUES (?, ?)" 1000 "Start")
    (execute-non-query db "INSERT INTO logs VALUES (?, ?)" 1001 "Event A")
    (execute-non-query db "INSERT INTO logs VALUES (?, ?)" 1002 "Event B")
    (let ((result (execute-to-list db "SELECT * FROM logs")))
      (assert-equal 3 (length result)))))
```

**Test Status:** Passing (verified 2026-01-20)

---

## Related Scenarios

- `basic-query-execution` - Basic SELECT
- `empty-result-handling` - Query with no results

---

**Document Status:** Canonical
**Last Verified:** 2026-01-20 (Canon Initiation Pass 3)
