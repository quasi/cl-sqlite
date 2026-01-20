# Contract: update-table

**Signature:** `(update-table db table-name updates &key where) => nil`
**Confidence:** 0.95
**Status:** Stable

---

## Parameters

- **db:** Connected database handle
- **table-name:** Keyword/string
- **updates:** Plist of column:value pairs to update
- **where:** S-expression WHERE clause (optional, all rows if omitted)

## Returns

`nil` on success

## Behavior

Constructs UPDATE statement, binds parameters, executes.

## Examples

```common-lisp
(update-table db :users '(:active 0) :where '(:= :status "inactive"))
;; UPDATE users SET active = 0 WHERE status = 'inactive'

(update-table db :users '(:name "Robert" :age 31) :where '(:= :id 1))

(update-table db :counters '(:count (:+ :count 1)))  ; Increment
```

## WHERE Clause

Optional. If omitted, updates ALL rows (dangerous!):
```common-lisp
;; DO NOT DO THIS unless you mean it!
(update-table db :users '(:active 0))  ; Sets all users inactive!
```

## Errors

- `sqlite-error` if constraint violated

## Related

- `select` - Query before update
- `delete-from` - Delete rows

---

**Document Status:** Canonical
**Last Verified:** 2026-01-20 (Canon Initiation Pass 2)
