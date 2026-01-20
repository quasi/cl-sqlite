# Contract: error-code (Accessor)

**Signature:** `(error-code condition) => keyword`
**Confidence:** 0.90
**Status:** Stable

---

## Parameters

- **condition:** `sqlite-error` instance

## Returns

Error code keyword

## Codes

| Keyword | Value | Meaning |
|---------|-------|---------|
| `:ok` | 0 | Success |
| `:error` | 1 | SQL error or missing database |
| `:internal` | 2 | Internal logic error |
| `:perm` | 3 | Access permission denied |
| `:abort` | 4 | Callback routine requested abort |
| `:busy` | 5 | Database file is locked |
| `:locked` | 6 | Database table is locked |
| `:nomem` | 7 | Out of memory |
| `:readonly` | 8 | Attempt to write read-only database |
| `:ioerr` | 10 | Disk I/O error |
| `:corrupt` | 11 | Database disk image is malformed |
| `:cantopen` | 14 | Unable to open database file |
| `:constraint` | 19 | Integrity constraint violation |
| `:mismatch` | 20 | Data type mismatch |
| `:notadb` | 26 | File is not a database |

## Example

```common-lisp
(handler-case
    (execute-non-query db "INSERT INTO users (id) VALUES (1), (1)")
  (sqlite-error (e)
    (case (error-code e)
      (:constraint (format t "Duplicate key!~%"))
      (:busy (format t "Database locked!~%"))
      (t (format t "Other error: ~A~%" (error-code e))))))
```

---

**Document Status:** Canonical
**Last Verified:** 2026-01-20 (Canon Initiation Pass 2)
