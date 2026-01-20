# Property: Column Metadata Availability

**ID:** EXEC-003
**Confidence:** 0.95
**Status:** Verified
**Type:** Semantic Property

---

## Statement

Column metadata (name, table, database, type) is available after statement preparation, before stepping. Metadata does not change during stepping.

---

## Availability Timeline

**After prepare (before step):**
- ✅ Column count available
- ✅ Column names available
- ✅ Column table/database available
- ✅ Column type available

**After step:**
- ✅ All metadata still available
- ✅ Column value available (via statement-column-value)

**After finalize:**
- ❌ All metadata unavailable (statement released)

---

## Usage Examples

### Get Headers Before Stepping
```common-lisp
(let ((stmt (prepare-statement db "SELECT id, name FROM users")))
  ;; Can get metadata before step
  (let ((headers (loop for i from 0 below (statement-column-count stmt)
                       collect (statement-column-name stmt i))))
    ;; Now step and fetch data
    (loop while (step-statement stmt)
          do (process-row headers stmt))))
```

---

### Consistent Metadata During Stepping
```common-lisp
(let ((stmt (prepare-statement db "SELECT * FROM users")))
  (loop while (step-statement stmt)
        for count from 1
        do ;; Metadata doesn't change
           (assert-equal 3 (statement-column-count stmt))))
```

---

## Related Properties

- **Column Index Base:** Metadata uses 0-indexed
- **Result Type Consistency:** Types deterministic after prepare

---

**Document Status:** Canonical
**Last Verified:** 2026-01-20 (Canon Initiation Pass 4)
