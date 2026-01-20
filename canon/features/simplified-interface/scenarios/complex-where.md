# Scenario: Complex WHERE Clauses

**Confidence:** 0.95
**Status:** Verified
**Test:** `simple-tests.lisp` (WHERE tests)

---

## Description

Demonstrates WHERE clause composition with multiple operators.

## Examples

```common-lisp
;; Logical AND
(select db :users :where '(:and (:> :age 18) (:< :age 65)))

;; Logical OR
(select db :users :where '(:or (:= :status "active") (:= :status "pending")))

;; IN operator
(select db :users :where '(:in :category "A" "B" "C"))

;; NULL checks
(select db :users :where '(:is-null :deleted_at))

;; NOT
(select db :users :where '(:not (:= :active 0)))

;; LIKE
(select db :users :where '(:like :name "Alice%"))

;; Nested
(select db :users
  :where '(:and
           (:> :age 18)
           (:or (:= :status "active") (:is-null :banned_at))))
```

## Operator Reference

| Operator | Example | SQL |
|----------|---------|-----|
| `:=` | `(:= :age 30)` | `age = 30` |
| `:<` | `(:< :age 30)` | `age < 30` |
| `:>` | `(:> :age 30)` | `age > 30` |
| `:<=` | `(:≤ :age 30)` | `age <= 30` |
| `:>=` | `(:≥ :age 30)` | `age >= 30` |
| `:<>` | `(:<> :status "deleted")` | `status <> 'deleted'` |
| `:and` | `(:and clause1 clause2)` | `clause1 AND clause2` |
| `:or` | `(:or clause1 clause2)` | `clause1 OR clause2` |
| `:not` | `(:not clause)` | `NOT (clause)` |
| `:in` | `(:in :col val1 val2)` | `col IN (val1, val2)` |
| `:like` | `(:like :name "A%")` | `name LIKE 'A%'` |
| `:is-null` | `(:is-null :col)` | `col IS NULL` |
| `:is-not-null` | `(:is-not-null :col)` | `col IS NOT NULL` |

## Expected Outcomes

- ✅ WHERE clauses compile correctly
- ✅ Results filtered as expected
- ✅ Operators compose properly
- ✅ SQL injection prevented

---

**Document Status:** Canonical
**Last Verified:** 2026-01-20 (Canon Initiation Pass 3)
