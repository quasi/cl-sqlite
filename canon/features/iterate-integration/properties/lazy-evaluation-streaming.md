# Property: Lazy Evaluation Streaming

**ID:** STREAM-001
**Confidence:** 0.90
**Status:** Verified
**Type:** Semantic Property

---

## Statement

```
iterate bindings (in-sqlite-query) provide lazy evaluation:
rows fetched and yielded one at a time, not materialized into memory.
```

## Behavior

**Lazy (iterate):**
```common-lisp
(iter (for row in-sqlite-query db "SELECT * FROM big_table")
      (do (process row)))
;; Rows streamed: current row in memory
;; Perfect for 1M+ rows
```

**Eager (execute-to-list):**
```common-lisp
(dolist (row (execute-to-list db "SELECT * FROM big_table"))
  (process row))
;; All rows loaded first: O(n) memory
;; Fine for 1K-100K rows
```

## Memory Profile

- **iterate:** O(1) memory (current row only)
- **execute-to-list:** O(n) memory (all rows)

## When to Use

| Size | Method |
|------|--------|
| 1-100 rows | Either |
| 100-10K rows | execute-to-list |
| 10K-1M rows | iterate (streaming) |
| 1M+ rows | iterate (only option) |

---

**Document Status:** Canonical
**Last Verified:** 2026-01-20 (Canon Initiation Pass 4)
