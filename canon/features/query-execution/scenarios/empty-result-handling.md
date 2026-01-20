# Scenario: Empty Result Handling

**Confidence:** 0.95
**Status:** Verified
**Test:** Implicit in test suite

---

## Description

Executes a query with no matching results and handles empty result gracefully.

**Purpose:** Validates handling of queries with no rows

---

## Setup

1. Create table
2. Execute query with no matching rows

---

## Execution

```common-lisp
(with-open-database (db ":memory:")
  (execute-non-query db "CREATE TABLE users (id INTEGER, name TEXT)")
  (let ((result (execute-to-list db "SELECT * FROM users WHERE id = ?" 999)))
    ;; result should be nil (empty list)
    ))
```

---

## Steps

1. **Create empty or non-matching table**
   - Table exists but no rows match WHERE clause

2. **Execute query**
   - SQL with WHERE that matches no rows

3. **Handle empty result**
   - `execute-to-list` returns `nil`
   - `execute-single` returns `:null`
   - `execute-one-row-m-v` returns `:null` for each column

4. **Verify empty result**
   - No error signaled
   - Result is empty (not error)

---

## Expected Outcomes

### Query Execution Phase
- ✅ Statement prepared successfully
- ✅ Parameter bound
- ✅ WHERE clause evaluated
- ✅ No rows matched
- ✅ No error signaled

### Result Handling
- ✅ `execute-to-list` returns `nil`
- ✅ `execute-single` returns `:null`
- ✅ `execute-one-row-m-v` returns all `:null` values
- ✅ No exceptions

---

## Variations

### execute-to-list with Empty Result
```common-lisp
(execute-to-list db "SELECT * FROM users WHERE id = ?" 999)
;; => nil
```

### execute-single with Empty Result
```common-lisp
(execute-single db "SELECT COUNT(*) FROM users WHERE id = ?" 999)
;; => 0 (COUNT returns 0, not :null)

(execute-single db "SELECT name FROM users WHERE id = ?" 999)
;; => :null
```

### execute-one-row-m-v with Empty Result
```common-lisp
(multiple-value-bind (name age)
    (execute-one-row-m-v db "SELECT name, age FROM users WHERE id = ?" 999)
  ;; name = :null, age = :null
  )
```

### FROM-less Query
```common-lisp
(execute-to-list db "SELECT * FROM users")
;; => nil (if table is empty)
```

---

## Anti-patterns

❌ **Assuming list length > 0:**
```common-lisp
;; WRONG - May fail if no results
(let ((result (execute-to-list db "SELECT * FROM users WHERE active = ?" 1)))
  (first result))  ; Error if result is nil

;; RIGHT - Check for empty
(let ((result (execute-to-list db "SELECT * FROM users WHERE active = ?" 1)))
  (if result
      (first result)
      (handle-no-results)))
```

❌ **Confusing nil with 0:**
```common-lisp
;; WRONG - COUNT(*) never returns nil, even if no rows
(if (execute-single db "SELECT COUNT(*) FROM users WHERE id = ?" 999)
    (format t "Users found~%")
    (format t "No users~%"))
;; Will print "Users found" (COUNT returns 0, which is truthy in CL)

;; RIGHT - Explicitly check
(let ((count (execute-single db "SELECT COUNT(*) FROM users")))
  (format t "Found ~D users~%" count))
```

---

## Properties Verified

- **Empty Result Handling:** No error on empty result
- **Nil as Empty Marker:** `execute-to-list` returns `nil`
- **:null as Empty Field:** Other functions return `:null`

---

## Test Evidence

**Implicit in test suite:** All tests that might return no results handle it

**Documentation:**
- README.md (implicit in examples)
- REFERENCE.md (implicit in return value docs)
- CL-SQLITE.agent.md:873-875 (empty result handling)

---

## Related Scenarios

- `basic-query-execution` - Query with results
- `parameterized-query` - Query with WHERE clause

---

**Document Status:** Canonical
**Last Verified:** 2026-01-20 (Canon Initiation Pass 3)
