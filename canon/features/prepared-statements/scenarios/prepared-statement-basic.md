# Scenario: Basic Prepared Statement Lifecycle

**Confidence:** 0.95
**Status:** Verified
**Test:** `test-prepare-statement` (sqlite-tests.lisp:20)

---

## Description

Demonstrates the complete lifecycle of a prepared statement: prepare, bind, step, finalize.

**Purpose:** Validates fundamental prepared statement operations

---

## Setup

1. Create in-memory database
2. Create table with schema
3. Insert sample data

---

## Execution

```common-lisp
(deftest test-prepare-statement ()
  (with-open-database (db ":memory:")
    (execute-non-query db "CREATE TABLE users (id INTEGER, name TEXT)")
    (execute-non-query db "INSERT INTO users VALUES (?, ?)" 1 "Alice")
    (let ((stmt (prepare-statement db "SELECT * FROM users WHERE id = ?")))
      (bind-parameter stmt 1 1)
      (assert-true (step-statement stmt))
      (finalize-statement stmt))))
```

---

## Steps

1. **Prepare Table**
   - `CREATE TABLE users (id INTEGER, name TEXT)`
   - Insert: (1, "Alice")

2. **Prepare Statement**
   - SQL: `SELECT * FROM users WHERE id = ?`
   - Call: `(prepare-statement db sql)`
   - Result: `sqlite-statement` instance

3. **Verify Preparation**
   - Statement compiled
   - Column count available
   - Ready for binding

4. **Bind Parameter**
   - Position: 1 (first placeholder)
   - Value: 1 (user ID)
   - Call: `(bind-parameter stmt 1 1)`

5. **Execute (Step)**
   - Call: `(step-statement stmt)`
   - Result: `T` (row available)

6. **Verify Row Available**
   - Return value is `T`
   - Columns accessible via `statement-column-value`

7. **Finalize**
   - Call: `(finalize-statement stmt)`
   - Resources released

---

## Expected Outcomes

### Prepare Phase
- ✅ Statement instance created
- ✅ SQL compiled
- ✅ Column count: 2 (id, name)

### Bind Phase
- ✅ Parameter bound at position 1
- ✅ No errors

### Step Phase
- ✅ Query executes
- ✅ Step returns `T` (row found)
- ✅ Columns accessible

### Finalize Phase
- ✅ Resources released
- ✅ No errors

---

## Invariants Verified

- **INV-002 (Prepared Statement Lifecycle):**
  - Prepare → bind → step → finalize sequence correct

- **RULE-002 (Connection Before Operations):**
  - Connection active before statement operations

- **RULE-003 (Bind Before Step):**
  - Parameters bound before stepping

---

## Properties Verified

- Prepared statements compile correctly
- Binding works as expected
- Stepping returns correct results
- Finalization completes successfully

---

## Variations

### No Results (WHERE doesn't match)
```common-lisp
(let ((stmt (prepare-statement db "SELECT * FROM users WHERE id = ?")))
  (bind-parameter stmt 1 999)  ; Non-existent user
  (assert-false (step-statement stmt)))  ; Returns NIL
```

---

### Column Access After Step
```common-lisp
(let ((stmt (prepare-statement db "SELECT * FROM users WHERE id = ?")))
  (bind-parameter stmt 1 1)
  (when (step-statement stmt)
    (let ((id (statement-column-value stmt 0))
          (name (statement-column-value stmt 1)))
      (assert-equal 1 id)
      (assert-equal "Alice" name))))
```

---

### Reset and Re-step
```common-lisp
(let ((stmt (prepare-statement db "SELECT * FROM users WHERE id = ?")))
  (bind-parameter stmt 1 1)
  (assert-true (step-statement stmt))
  (reset-statement stmt)
  ;; Can step again with same binding
  (assert-true (step-statement stmt))
  (finalize-statement stmt))
```

---

## Anti-patterns

❌ **Forgetting to finalize:**
```common-lisp
;; WRONG - Resource leak
(let ((stmt (prepare-statement db "SELECT * FROM users WHERE id = ?")))
  (bind-parameter stmt 1 1)
  (step-statement stmt))
;; Statement never finalized!

;; RIGHT - Explicit finalization
(let ((stmt (prepare-statement db "SELECT * FROM users WHERE id = ?")))
  (unwind-protect
      (progn
        (bind-parameter stmt 1 1)
        (step-statement stmt))
    (finalize-statement stmt)))
```

❌ **Stepping without binding:**
```common-lisp
;; WRONG - Parameter not bound
(let ((stmt (prepare-statement db "SELECT * FROM users WHERE id = ?")))
  (step-statement stmt))  ; Parameter 1 unbound (NULL)
;; Finds no rows (comparing id = NULL)

;; RIGHT - Bind before step
(let ((stmt (prepare-statement db "SELECT * FROM users WHERE id = ?")))
  (bind-parameter stmt 1 1)
  (step-statement stmt))
```

---

## Test Evidence

**Source:** `sqlite-tests.lisp:20`
```common-lisp
(deftest test-prepare-statement ()
  (with-open-database (db ":memory:")
    (execute-non-query db "CREATE TABLE users (id INTEGER, name TEXT)")
    (execute-non-query db "INSERT INTO users VALUES (?, ?)" 1 "Alice")
    (let ((stmt (prepare-statement db "SELECT * FROM users WHERE id = ?")))
      (bind-parameter stmt 1 1)
      (assert-true (step-statement stmt))
      (finalize-statement stmt))))
```

**Test Status:** Passing (verified 2026-01-20)

---

## Related Scenarios

- `prepared-statement-reuse` - Reusing statements with reset

---

**Document Status:** Canonical
**Last Verified:** 2026-01-20 (Canon Initiation Pass 3)
