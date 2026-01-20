# Property: WHERE Clause Composability

**ID:** EXEC-004
**Confidence:** 0.95
**Status:** Verified
**Type:** Semantic Property

---

## Statement

WHERE clauses are composable s-expressions: can be built programmatically and combined into complex queries.

## Examples

**Helper functions:**
```common-lisp
(defun active-users-where (min-age)
  `(:and (:= :active 1) (:> :age ,min-age)))

(defun search-where (text)
  `(:like :name ,(format nil "%~A%" text)))
```

**Composition:**
```common-lisp
(select db :users :where (active-users-where 18))

(select db :users :where
  `(:and ,(active-users-where 18) ,(search-where "Alice")))
```

**Programmatic building:**
```common-lisp
(defun build-query (db table predicates)
  (let ((where (if (rest predicates)
                   `(:and ,@predicates)
                   (first predicates))))
    (select db table :where where)))
```

## Benefits

- ✅ Reusable WHERE fragments
- ✅ Dynamic query construction
- ✅ Separation of concerns
- ✅ Testable query building

---

**Document Status:** Canonical
**Last Verified:** 2026-01-20 (Canon Initiation Pass 4)
