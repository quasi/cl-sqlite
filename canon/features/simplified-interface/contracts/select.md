# Contract: select

**Signature:** `(select db table-name &key where order-by limit offset) => list-of-rows`
**Confidence:** 0.95
**Status:** Stable

---

## Parameters

- **db:** Connected database handle
- **table-name:** Keyword/string
- **where:** S-expression WHERE clause (optional)
- **order-by:** Column(s) to order by (optional)
- **limit:** Max rows (optional)
- **offset:** Rows to skip (optional)

## Returns

List of rows (each row is list of column values)

## WHERE Clause Syntax

```common-lisp
'(:= :age 30)              ; age = 30
'(:> :age 25)              ; age > 25
'(:and (:> :age 18) (:< :age 65))
'(:or (:= :status "active") (:= :status "pending"))
'(:in :category "A" "B" "C")
'(:is-null :deleted_at)
'(:like :name "Alice%")
```

## Examples

```common-lisp
(select db :users :where '(:= :active 1))
;; => ((1 "Alice" 30) (3 "Charlie" 35))

(select db :users :where '(:> :age 30) :order-by '(:age :desc) :limit 10)

(select db :users :limit 5 :offset 10)  ; Pagination
```

## ORDER BY

- Single column: `:age`
- With direction: `'(:age :desc)`
- Multiple: `'((:age :desc) (:name :asc))`

## Returns

Empty list if no rows match

## Related

- `insert` - Add rows
- `update-table` - Modify rows
- `compile-where` - Advanced WHERE composition

---

**Document Status:** Canonical
**Last Verified:** 2026-01-20 (Canon Initiation Pass 2)
