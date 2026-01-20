# Contract: insert

**Signature:** `(insert db table-name row-data) => nil`
**Confidence:** 0.95
**Status:** Stable

---

## Parameters

- **db:** Connected database handle
- **table-name:** Keyword/string
- **row-data:** Plist of column:value pairs

## Returns

`nil` on success

## Behavior

Constructs INSERT statement from plist, binds parameters, executes.

## Example

```common-lisp
(insert db :users '(:name "Alice" :email "alice@example.com"))
;; Generates: INSERT INTO users (name, email) VALUES (?, ?)
;; Binds: "Alice", "alice@example.com"

(insert db :users '(:id 1 :name "Bob" :active 1))
```

## NULL Values

Use `:null` keyword:
```common-lisp
(insert db :users '(:name "Charlie" :email :null))
```

## Errors

- `sqlite-error` if constraint violated or column doesn't exist

## Supports

- Type conversion (automatic)
- Parameter binding (safe, no SQL injection)
- Multiple column insert

## Related

- `create-table` - Define table
- `select` - Query data
- `update-table` - Modify rows

---

**Document Status:** Canonical
**Last Verified:** 2026-01-20 (Canon Initiation Pass 2)
