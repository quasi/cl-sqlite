# Contract: sqlite-error (Exception Type)

**Type:** `sqlite-error`
**Confidence:** 0.90
**Status:** Stable

---

## Description

Exception signaled when SQLite operation fails.

## Slots

- **error-code:** Keyword (`:ok`, `:error`, `:busy`, `:constraint`, etc.)
- **error-msg:** Human-readable message
- **db-handle:** Database handle (if available)
- **stmt:** Statement (if available)

## Usage

```common-lisp
(handler-case
    (execute-non-query db "INVALID SQL")
  (sqlite-error (e)
    (format t "Error: ~A (~A)~%"
            (sqlite-error-msg e)
            (sqlite-error-code e))))
```

## Common Error Codes

- `:ok` - Success (0)
- `:error` - Generic error (1)
- `:busy` - Database locked (5)
- `:constraint` - Constraint violation (19)
- `:cantopen` - Cannot open file (14)
- `:readonly` - Read-only database (8)
- `:nomem` - Out of memory (7)

## Related

- `error-code` - Extract error code
- Error patterns in CL-SQLITE.agent.md

---

**Document Status:** Canonical
**Last Verified:** 2026-01-20 (Canon Initiation Pass 2)
