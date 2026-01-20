# Contract: create-table

**Signature:** `(create-table db table-name column-specs) => nil`
**Confidence:** 0.95
**Status:** Stable

---

## Parameters

- **db:** Connected database handle
- **table-name:** Keyword/string (normalized)
- **column-specs:** List of column definitions

## Column Specification

Each spec is a list: `(name type :constraint1 :constraint2 ...)`

```common-lisp
(create-table db :users
  '((:id :integer :primary-key :autoincrement)
    (:name :text :not-null)
    (:email :text)
    (:created_at :integer :default 0)))
```

## Supported Constraints

- `:primary-key` - Primary key
- `:autoincrement` - Auto-increment (requires PRIMARY KEY)
- `:not-null` - NOT NULL
- `:unique` - Unique constraint
- `:default N` - Default value

## Returns

`nil` on success

## Implementation

Compiles column specs to SQL CREATE TABLE statement, calls `execute-non-query`.

## Example

```common-lisp
(create-table db :contacts
  '((:id :integer :primary-key :autoincrement)
    (:name :text :not-null)
    (:phone :text :unique)
    (:active :integer :default 1)))

;; Generates:
;; CREATE TABLE contacts (
;;   id INTEGER PRIMARY KEY AUTOINCREMENT,
;;   name TEXT NOT NULL,
;;   phone TEXT UNIQUE,
;;   active INTEGER DEFAULT 1)
```

## Errors

- `sqlite-error` if table exists or syntax invalid

## Related

- `insert` - Insert rows
- `select` - Query table
- `delete-from` - Delete rows

---

**Document Status:** Canonical
**Last Verified:** 2026-01-20 (Canon Initiation Pass 2)
