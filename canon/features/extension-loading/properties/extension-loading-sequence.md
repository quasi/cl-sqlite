# Property: Extension Loading Sequence

**ID:** RULE-006
**Confidence:** 0.90
**Status:** Verified
**Type:** Normative Rule

---

## Statement

```
Extensions must be loaded in strict sequence:
1. enable-load-extension(db, true)
2. load-extension(db, path, null-pointer)
3. Use extension functions
4. (Optional) enable-load-extension(db, false)
```

## Rationale

SQLite disables extension loading by default for security. Explicit opt-in required.

## Violation Consequence

Calling `load-extension` without enabling returns SQLite error.

## Example

```common-lisp
;; CORRECT
(enable-load-extension db t)
(load-extension db "/path/to/vec0.so" (cffi:null-pointer))
(create-vector-table db ...)

;; WRONG - Missing enable-load-extension
(load-extension db "/path/to/vec0.so" (cffi:null-pointer))
;; => sqlite-error "Not authorized"
```

---

**Document Status:** Canonical
**Last Verified:** 2026-01-20 (Canon Initiation Pass 4)
