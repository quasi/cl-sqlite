# Property: SQL Injection Prevention

**ID:** ANTI-001
**Confidence:** 0.95
**Status:** Verified
**Type:** Security Property

---

## Statement

Simplified API prevents SQL injection by design: all WHERE clauses compile to parameterized SQL with values bound safely.

## Mechanism

**Before (vulnerable):**
```common-lisp
;; WRONG - String concatenation
(let ((name "'; DROP TABLE users; --"))
  (execute-non-query db
    (format nil "DELETE FROM users WHERE name = '~A'" name)))
;; SQL becomes: DELETE FROM users WHERE name = ''; DROP TABLE users; --'
;; TABLE DROPPED!
```

**After (safe):**
```common-lisp
;; RIGHT - Parameter binding
(let ((name "'; DROP TABLE users; --"))
  (delete-from db :users :where '(:= :name "'; DROP TABLE users; --")))
;; Malicious input treated as literal string value
;; WHERE name = ''; DROP TABLE users; --'
;; No SQL injection - literal string comparison only
```

## How It Works

1. WHERE clause is s-expression, not string
2. `compile-where` generates SQL with `?` placeholders
3. Values bound as parameters (never as SQL)
4. SQLite C API handles parameter substitution safely

## Guarantees

- ✅ No string concatenation in library code
- ✅ All values parameterized
- ✅ Impossible to inject SQL via parameter values

---

**Document Status:** Canonical
**Last Verified:** 2026-01-20 (Canon Initiation Pass 4)
