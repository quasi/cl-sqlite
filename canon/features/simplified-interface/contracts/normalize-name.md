# Contract: normalize-name

**Signature:** `(normalize-name name) => string`
**Confidence:** 0.90
**Status:** Stable

---

## Parameters

- **name:** Keyword, symbol, or string

## Returns

Lowercase string

## Behavior

Converts name to lowercase string for consistent SQL identifiers.

## Examples

```common-lisp
(normalize-name :users)        ; => "users"
(normalize-name 'users)        ; => "users"
(normalize-name "USERS")       ; => "users"
(normalize-name "Users")       ; => "users"
```

## Purpose

SQLite is case-insensitive for identifiers. Normalization ensures consistent behavior.

## Related

- `create-table` - Uses normalize-name internally
- `insert` - Uses normalize-name internally
- `select` - Uses normalize-name internally

---

**Document Status:** Canonical
**Last Verified:** 2026-01-20 (Canon Initiation Pass 2)
