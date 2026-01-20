# Scenario: Basic Error Handling

**Confidence:** 0.90
**Status:** Verified

---

## Description

Catching and handling sqlite-error exceptions.

## Example

```common-lisp
(handler-case
    (execute-non-query db "INVALID SQL")
  (sqlite-error (e)
    (format t "SQL error: ~A~%" (error-code e))))

(handler-case
    (execute-non-query db "INSERT INTO users (id) VALUES (1), (1)")
  (sqlite-error (e)
    (if (eq (error-code e) :constraint)
        (format t "Duplicate key error!~%")
        (format t "Other error: ~A~%" e))))
```

## Expected

- ✅ Error caught
- ✅ Error code extracted
- ✅ Program continues

---

**Document Status:** Canonical
**Last Verified:** 2026-01-20 (Canon Initiation Pass 3)
