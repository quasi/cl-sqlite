# Scenario: Basic CRUD Operations

**Confidence:** 0.95
**Status:** Verified
**Tests:** `simple-tests.lisp` (multiple tests)

---

## Description

Complete Create-Read-Update-Delete workflow using simplified API.

## Execution

```common-lisp
(with-open-database (db ":memory:")
  ;; CREATE
  (create-table db :users
    '((:id :integer :primary-key :autoincrement)
      (:name :text :not-null)
      (:age :integer)))

  ;; INSERT
  (insert db :users '(:name "Alice" :age 30))
  (insert db :users '(:name "Bob" :age 25))

  ;; SELECT
  (let ((users (select db :users)))
    (assert-equal 2 (length users)))

  ;; UPDATE
  (update-table db :users '(:age 31) :where '(:= :name "Alice"))

  ;; DELETE
  (delete-from db :users :where '(:= :name "Bob"))

  ;; VERIFY
  (let ((remaining (select db :users)))
    (assert-equal 1 (length remaining))))
```

## Expected Outcomes

- ✅ Table created
- ✅ 2 rows inserted
- ✅ SELECT returns 2 rows
- ✅ UPDATE succeeds
- ✅ DELETE succeeds
- ✅ Final count: 1 row

## Related Scenarios

- `select-with-where` - Filtering
- `update-with-where` - Conditional update
- `complex-where-clause` - Advanced WHERE

---

**Document Status:** Canonical
**Last Verified:** 2026-01-20 (Canon Initiation Pass 3)
