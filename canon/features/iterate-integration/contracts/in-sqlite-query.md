# Contract: in-sqlite-query (iterate Driver)

**Signature:** `(iter (for binding in-sqlite-query db sql &rest params) ...)`
**Confidence:** 0.90
**Status:** Stable

---

## Parameters

- **db:** Connected database
- **sql:** SQL SELECT query
- **params:** Query parameters (optional)

## Returns

Iterator over result rows

## Behavior

Lazy iteration over query results. Rows fetched one at a time instead of materializing entire result set.

## Example

```common-lisp
(iter (for (id name age) in-sqlite-query db
           "SELECT id, name, age FROM users WHERE active = ?" 1)
      (format t "~D: ~A (~D)~%" id name age))
```

## Advantages Over execute-to-list

- **Memory:** O(1) vs O(n)
- **Latency:** First row immediately vs all rows fetched first
- **Large datasets:** Can iterate without loading all rows

## Example: Large Dataset

```common-lisp
;; Efficient for 1M rows
(iter (for (id) in-sqlite-query db "SELECT id FROM huge_table")
      (count (lambda () (process-id id))))
```

## Related

- `execute-to-list` - Materializes entire result set
- `in-sqlite-query-values` - Multiple values per iteration

---

**Document Status:** Canonical
**Last Verified:** 2026-01-20 (Canon Initiation Pass 2)
