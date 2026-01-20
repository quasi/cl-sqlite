# Contract: iterate Bindings for SQLite

**Signature:** `(in-sqlite-query db sql &rest params)`
**Confidence:** 0.90
**Status:** Stable

---

## iterate Clauses

Supported iterate bindings:

- `in-sqlite-query` - Iterate over row lists: `(for row in-sqlite-query ...)`
- `in-sqlite-query-values` - Iterate over multiple values: `(for (id name) in-sqlite-query ...)`

## Example

```common-lisp
;; Single value per iteration
(iter (for row in-sqlite-query db "SELECT * FROM users")
      (format t "Row: ~A~%" row))

;; Multiple values (destructured)
(iter (for (id name age) in-sqlite-query db "SELECT id, name, age FROM users")
      (format t "~D: ~A (~D)~%" id name age))
```

## Supports

- All iterate clauses (collecting, counting, etc.)
- Parameter binding
- WHERE clauses

---

**Document Status:** Canonical
**Last Verified:** 2026-01-20 (Canon Initiation Pass 2)
