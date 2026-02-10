[< Back to README](../README.md)

# SQL Injection and the Simplified Interface

This document examines how Inquisitio's simplified interface (`simple.lisp`) handles SQL injection, what we hardened, and why embedded SQLite has a different threat model than web-facing databases.

## Background

The [OWASP SQL Injection Prevention Cheat Sheet](https://cheatsheetseries.owasp.org/cheatsheets/SQL_Injection_Prevention_Cheat_Sheet.html) ranks four defenses:

1. **Prepared statements with parameterized queries** (recommended)
2. **Stored procedures** (equivalent when done right)
3. **Allowlist input validation** (for structural elements like table names)
4. **Escaping** (strongly discouraged, fragile)

We evaluated the simplified interface against these recommendations.

## What the simplified interface does well

All **user-supplied values** flow through parameterized queries. This is the single most important defense and is implemented consistently throughout.

The `compile-where` function generates `?` placeholders for every value in WHERE clauses. Comparison operators (`:=`, `:<`, `:>`, `:like`, etc.), `:in` lists, and all data in `insert` and `update-table` pass through parameter binding. The database always treats these as data, never as SQL code.

Example of what happens internally:

```lisp
;; This call:
(select db :users :where '(:= :name "Alice'; DROP TABLE users--"))

;; Generates this SQL with a bound parameter:
;;   SELECT * FROM users WHERE name = ?
;;   params: ("Alice'; DROP TABLE users--")
;;
;; The malicious string is treated as a literal value.
;; The DROP TABLE never executes.
```

This matches OWASP Defense #1.

## What was directly interpolated (before hardening)

These structural elements were concatenated into SQL strings via `format`:

| Element | Risk | Mitigation |
|---------|------|------------|
| Table names | Medium | Now validated by `normalize-name` |
| Column names | Medium | Now validated by `normalize-name` |
| ORDER BY direction | Low | Now allowlisted to ASC/DESC |
| LIMIT / OFFSET | Low | Now type-checked as non-negative integers |

Table and column names **cannot** be parameterized in SQL. Every SQL builder in every language faces this same constraint. OWASP recommends allowlist validation for these (Defense #3), which is what `normalize-name` now provides.

## What we hardened

### 1. Identifier validation (`normalize-name`)

Before: accepted any string, applied only `string-downcase`.

After: validates the result against `[a-z_][a-z0-9_]*`. Rejects semicolons, quotes, spaces, parentheses, and any other SQL metacharacter. Also converts Lisp-style hyphens to SQL-style underscores (`:user-name` becomes `user_name`).

```lisp
(normalize-name :users)          ;; => "users"
(normalize-name :user-name)      ;; => "user_name"
(normalize-name "valid_col")     ;; => "valid_col"
(normalize-name "users; DROP")   ;; => ERROR: Invalid SQL identifier
```

### 2. ORDER BY direction allowlist

Before: any symbol was converted to a string and interpolated.

After: only `:asc` and `:desc` are accepted.

```lisp
(select db :users :order-by '(:age :desc))   ;; works
(select db :users :order-by '(:age :sideways)) ;; => ERROR
```

### 3. LIMIT and OFFSET type checking

Before: any value was interpolated via format directive `~A`.

After: must be a non-negative integer when provided.

```lisp
(select db :users :limit 10)                    ;; works
(select db :users :limit "1; DROP TABLE users")  ;; => ERROR
(select db :users :offset -1)                    ;; => ERROR
```

## Does embedded SQLite need these defenses?

The honest answer: **for typical use, the risk was already minimal.** Here is why.

### No network boundary

Embedded SQLite runs in-process. There is no client-server protocol, no network socket, no authentication layer. Anyone who can call `(sqlite:select db ...)` already holds the database handle and could execute arbitrary SQL directly with `execute-non-query`. SQL injection does not escalate privileges the caller already has.

### The API boundary is Lisp code

The intended calling pattern uses keywords and symbols:

```lisp
(select db :users :where '(:= :name value))
```

Lisp keywords (`:users`, `:name`, `:id`) cannot contain SQL metacharacters. They are inherently safe structural identifiers. The risk arises only if someone passes raw, user-controlled strings as table or column names, which is a misuse of the API.

### Values are already parameterized

The part most likely to contain untrusted input (WHERE clause values, INSERT data, UPDATE data) was parameterized from the start.

### When it would matter

The hardening protects against **API misuse** — for example, a web application that passes user input directly as a table name:

```lisp
;; Dangerous pattern (don't do this):
(let ((table (get-web-parameter "table")))
  (select db table ...))
```

With the new validation, `normalize-name` rejects the malicious input before it reaches SQL. Without it, the above code would be an injection vector.

## Current status

| Attack surface | Defense | Status |
|---------------|---------|--------|
| WHERE clause values | Parameterized queries (`?` binding) | Protected since initial implementation |
| INSERT/UPDATE data values | Parameterized queries (`?` binding) | Protected since initial implementation |
| Table names | `normalize-name` identifier validation | **Hardened** |
| Column names | `normalize-name` identifier validation | **Hardened** |
| ORDER BY direction | Allowlist (ASC/DESC only) | **Hardened** |
| LIMIT / OFFSET | Integer type check | **Hardened** |

The simplified interface now validates all inputs before they reach SQL generation. Structural elements are validated against safe patterns. Data values are bound through parameterized queries. No user-supplied string is ever concatenated directly into SQL.
