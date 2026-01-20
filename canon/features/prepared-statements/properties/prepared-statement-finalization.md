# Property: Prepared Statement Finalization

**ID:** ARCH-003
**Confidence:** 0.95
**Status:** Verified
**Type:** Resource Management Property

---

## Statement

Prepared statements must be explicitly finalized via `finalize-statement` to release C-level resources. Finalization is idempotent and necessary for resource cleanup.

---

## Finalization Guarantee

**When finalize-statement called:**
- C-level `sqlite3_stmt*` released
- Internal memory freed
- Statement becomes unusable

**Idempotent:**
- Multiple calls to finalize-statement safe (no-op after first)
- No error signaled

---

## Resource Cleanup

**Without finalization (resource leak):**
```common-lisp
(let ((stmt (prepare-statement db "SELECT * FROM users")))
  (step-statement stmt)
  ;; finalize-statement forgotten
  )
;; C-level resources NOT released
```

**With finalization (correct):**
```common-lisp
(let ((stmt (prepare-statement db "SELECT * FROM users")))
  (unwind-protect
      (step-statement stmt)
    (finalize-statement stmt)))
;; Resources released via unwind-protect
```

---

## Usage Patterns

### With unwind-protect
```common-lisp
(unwind-protect
    (progn
      (bind-parameter stmt 1 42)
      (step-statement stmt)
      (process-row stmt))
  (finalize-statement stmt))
;; Finalization guaranteed even on error
```

### Via execute functions (automatic)
```common-lisp
(execute-non-query db "INSERT INTO t VALUES (?)" 42)
;; Finalization automatic
```

---

## Cache Interaction

**Cached statements finalized on eviction:**
- Cache destructor calls finalize-statement
- When cache full: LRU statement finalized
- User-called finalize-statement prevents re-cache

---

## Properties

- **Deterministic:** Finalization always succeeds
- **Safe:** Can finalize multiple times
- **Necessary:** Required for resource cleanup
- **Exception-safe:** Use with unwind-protect

---

**Document Status:** Canonical
**Last Verified:** 2026-01-20 (Canon Initiation Pass 4)
