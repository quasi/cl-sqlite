# Scenario: Lazy Row Streaming with iterate

**Confidence:** 0.90
**Status:** Verified

---

## Description

Demonstrates lazy iteration over query results using iterate bindings.

## Example

```common-lisp
(iter (for (id name) in-sqlite-query db "SELECT id, name FROM users")
      (collect (cons id name)))
;; => ((1 . "Alice") (2 . "Bob") (3 . "Charlie"))
```

## Memory Efficiency

```common-lisp
;; Efficient for large result sets
(iter (for (id) in-sqlite-query db
           "SELECT id FROM huge_table WHERE year = ?" 2025)
      (do (process-record id)))
;; Only current row in memory, not all million rows
```

## Expected

- ✅ Rows streamed one at a time
- ✅ Minimal memory usage
- ✅ Works with iterate clauses

---

**Document Status:** Canonical
**Last Verified:** 2026-01-20 (Canon Initiation Pass 3)
