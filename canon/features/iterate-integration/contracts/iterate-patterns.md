# Contract: iterate Patterns

**Confidence:** 0.90
**Status:** Stable

---

## Common Patterns

### Counting

```common-lisp
(iter (for (id) in-sqlite-query db "SELECT id FROM users")
      (count id))
```

### Collecting

```common-lisp
(iter (for (name) in-sqlite-query db "SELECT name FROM users")
      (collect name))
```

### Accumulating

```common-lisp
(iter (for (id amount) in-sqlite-query db "SELECT id, amount FROM transactions")
      (summing amount))
```

### Filtering

```common-lisp
(iter (for (id name age) in-sqlite-query db "SELECT id, name, age FROM users")
      (when (> age 30)
        (collect name)))
```

### Side Effects

```common-lisp
(iter (for (id name) in-sqlite-query db "SELECT id, name FROM users")
      (do (send-email id name)))
```

---

**Document Status:** Canonical
**Last Verified:** 2026-01-20 (Canon Initiation Pass 2)
