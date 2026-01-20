# Contract: compile-where

**Signature:** `(compile-where where-clause) => (values sql-string parameter-list)`
**Confidence:** 0.95
**Status:** Stable

---

## Parameters

- **where-clause:** S-expression with operators and column references

## Returns

- **sql-string:** SQL WHERE clause (e.g., "age > ? AND active = ?")
- **parameter-list:** Values to bind in order

## Supported Operators

Logical: `:and`, `:or`, `:not`
Comparison: `:=`, `:<`, `:>`, `:<=`, `:>=`, `:<>`
Pattern: `:like`
Membership: `:in`
Null: `:is-null`, `:is-not-null`

## Examples

```common-lisp
(compile-where '(:= :age 30))
;; => "age = ?", (30)

(compile-where '(:and (:> :age 18) (:< :age 65)))
;; => "(age > ? AND age < ?)", (18 65)

(compile-where '(:in :status "active" "pending"))
;; => "status IN (?, ?)", ("active" "pending")
```

## Error Handling

Unknown operator signals error:
```common-lisp
(compile-where '(:unknown :field value))
;; => RULE-010 violation error
```

## Related

- `select` - Uses compile-where internally
- `update-table` - Uses compile-where internally
- `delete-from` - Uses compile-where internally

---

**Document Status:** Canonical
**Last Verified:** 2026-01-20 (Canon Initiation Pass 2)
