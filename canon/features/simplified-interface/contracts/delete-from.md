# Contract: delete-from

**Signature:** `(delete-from db table-name &key where) => nil`
**Confidence:** 0.95
**Status:** Stable

---

## Parameters

- **db:** Connected database handle
- **table-name:** Keyword/string
- **where:** S-expression WHERE clause (required for safety)

## Returns

`nil` on success

## Behavior

Constructs DELETE statement, binds parameters, executes.

## Examples

```common-lisp
(delete-from db :users :where '(:= :id 5))
;; DELETE FROM users WHERE id = 5

(delete-from db :logs :where '(:< :created_at 1000000))
;; DELETE FROM logs WHERE created_at < 1000000

(delete-from db :users :where '(:= :active 0))
;; DELETE all inactive users
```

## Safety

WHERE clause is optional but STRONGLY RECOMMENDED:
```common-lisp
;; DANGEROUS: Deletes ALL rows!
(delete-from db :users)
```

## Errors

- `sqlite-error` if operation fails

## Related

- `update-table` - Modify rows
- `select` - Query before delete

---

**Document Status:** Canonical
**Last Verified:** 2026-01-20 (Canon Initiation Pass 2)
