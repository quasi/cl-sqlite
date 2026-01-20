# Property: Row Iteration Semantics

**ID:** EXEC-001
**Confidence:** 0.95
**Status:** Verified
**Type:** Behavioral Property

---

## Statement

```
execute-to-list steps through all rows in result set,
collecting each row as it's stepped, until no more rows.
```

**Behavior:**
- Rows collected in order (database order)
- All rows collected (unless error)
- No rows skipped
- Iteration stops naturally after last row

---

## Rationale

**Design Goal:** Materialize entire result set into memory

**Benefits:**
- Can process results multiple times (list in memory)
- Predictable (not lazy evaluation)
- Easy to understand

**Trade-off:** Memory usage for result set
- Suitable for moderate result sets
- Not suitable for million-row tables (use iterate)

---

## Iteration Order

### Database Order (No ORDER BY)

**Rows returned in internal storage order:**
```common-lisp
(execute-to-list db "SELECT * FROM users")
;; => Order depends on:
;;    - Rowid order (if not deleted)
;;    - Index order (if query uses index)
;;    - Query plan chosen by optimizer
```

**Not guaranteed to be insertion order:**
```common-lisp
(execute-non-query db "INSERT INTO t VALUES (3)")
(execute-non-query db "INSERT INTO t VALUES (1)")
(execute-non-query db "INSERT INTO t VALUES (2)")

(execute-to-list db "SELECT rowid, value FROM t")
;; => Likely ((1 3) (2 1) (3 2)) - rowid order
;; NOT ((3 ...) (1 ...) (2 ...)) - insertion order
```

---

### With ORDER BY

**Rows returned in specified order:**
```common-lisp
(execute-to-list db "SELECT * FROM users ORDER BY age")
;; => Rows sorted by age (ascending)

(execute-to-list db "SELECT * FROM users ORDER BY age DESC, name ASC")
;; => Rows sorted by age (descending), then name (ascending)
```

---

## Collection Mechanism

**Location:** `sqlite.lisp:213-231`

```common-lisp
(defun execute-to-list (db sql &rest parameters)
  (let ((stmt (prepare-statement db sql)))
    (loop for i from 1
          for param in parameters
          do (bind-parameter stmt i param))
    (unwind-protect
        (loop while (step-statement stmt)
              collect (loop for i from 0 below (statement-column-count stmt)
                            collect (statement-column-value stmt i)))
      (finalize-statement stmt))))
```

**Loop semantics:**
- `(loop while (step-statement stmt) collect ...)` → collect while stepping
- `step-statement` returns `T` while rows available, `NIL` at end
- `collect` accumulates rows in list

---

## Row Representation

**Each row is a list of column values:**
```common-lisp
(execute-to-list db "SELECT id, name FROM users")
;; => ((1 "Alice")
;;     (2 "Bob")
;;     (3 "Charlie"))
```

**Column order:** Matches SELECT clause order
```common-lisp
(execute-to-list db "SELECT name, id FROM users")
;; => (("Alice" 1)
;;     ("Bob" 2)
;;     ("Charlie" 3))
;; Note: name first, then id (matches SELECT order)
```

---

## Stopping Conditions

### Normal Completion

**All rows collected:**
```common-lisp
(let ((result (execute-to-list db "SELECT * FROM users LIMIT 3")))
  ;; Loop collects rows until step returns NIL
  ;; result = 3 rows (or fewer if < 3 users exist)
  )
```

---

### No Rows

**Empty result set:**
```common-lisp
(execute-to-list db "SELECT * FROM users WHERE id = ?" 999)
;; => nil (empty list)
```

**Loop doesn't iterate (step returns NIL on first call)**

---

### Error During Iteration

**Error in step or column retrieval:**
```common-lisp
(handler-case
    (execute-to-list db "SELECT * FROM users")
  (error (e)
    ;; Partial result lost (loop interrupted)
    ))
```

**Result:** Error propagates, statement finalized via `unwind-protect`

---

## Comparison with Alternatives

### execute-to-list (Materialized)
```common-lisp
(execute-to-list db "SELECT * FROM logs")
;; => Complete list of all rows
;; Use when: Need to process results multiple times
;; Cost: Memory proportional to result size
```

### iterate (Lazy)
```common-lisp
(iter (for (ts msg) in-sqlite-query db "SELECT * FROM logs")
      (process-log ts msg))
;; => Rows processed one at a time
;; Use when: Process each row once, memory-limited
;; Cost: Constant memory (streaming)
```

### Prepared Statements (Explicit)
```common-lisp
(let ((stmt (prepare-statement db "SELECT * FROM logs")))
  (loop while (step-statement stmt)
        do (process-log (statement-column-value stmt 0) ...)))
;; => Low-level control
;; Use when: Custom iteration logic needed
```

---

## Performance Implications

### Memory Usage

**O(n) memory where n = result set size:**
```common-lisp
(execute-to-list db "SELECT * FROM logs")
;; Memory = (number of rows) × (row size)
;; For 1M rows × 100 bytes = ~100MB
```

---

### CPU Usage

**All rows parsed and converted:**
```common-lisp
;; All rows converted to Lisp objects (time proportional to rows)
(execute-to-list db "SELECT * FROM logs")
```

---

### Comparison

| Function | Memory | CPU | Use Case |
|----------|--------|-----|----------|
| execute-to-list | O(n) | O(n) | Moderate result sets |
| iterate | O(1) | O(n) | Large result sets, stream |
| execute-single | O(1) | O(1) | Single value |

---

## Edge Cases

### Large Result Sets

**Beyond practical memory limits:**
```common-lisp
;; DON'T - Will exhaust memory
(execute-to-list db "SELECT * FROM billion_rows")

;; DO - Stream with iterate
(iter (for row in-sqlite-query db "SELECT * FROM billion_rows")
      (process-row row))
```

---

### LIMIT Clause

**Restricts rows collected:**
```common-lisp
(execute-to-list db "SELECT * FROM logs LIMIT 100")
;; => Only 100 rows collected (even if table has millions)
```

---

### Complex Queries

**Iteration semantics unchanged:**
```common-lisp
(execute-to-list db
  "SELECT u.name, COUNT(o.id) as order_count
   FROM users u
   LEFT JOIN orders o ON u.id = o.user_id
   GROUP BY u.id
   ORDER BY order_count DESC")
;; => Rows grouped and ordered, then collected
```

---

## Related Properties

- **RULE-001 (Single Statement):** Single SELECT query
- **RULE-005 (Column Index Base):** Columns 0-indexed in collected rows

---

## Documentation References

- Implementation: sqlite.lisp:213-231
- Tests: sqlite-tests.lisp (multi-row query test)
- CL-SQLITE.agent.md:879-880 (execute-to-list semantics)

---

**Document Status:** Canonical
**Last Verified:** 2026-01-20 (Canon Initiation Pass 4)
