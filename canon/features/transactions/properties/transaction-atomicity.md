# Property: Transaction Atomicity

**ID:** INV-004
**Confidence:** 0.85
**Status:** Verified
**Type:** Invariant

---

## Statement

```
with-transaction ensures all-or-nothing semantics:
- On successful completion: COMMIT executes, all changes visible
- On error: ROLLBACK executes, all changes discarded
- On non-local exit: ROLLBACK executes via unwind-protect
```

## Guarantee

**Atomic execution:** Either all operations succeed or none do

```common-lisp
(with-transaction (db)
  (insert db :accounts '(:id 1 :balance 900))     ; Debit
  (insert db :accounts '(:id 2 :balance 1100))    ; Credit
  ;; If either fails or error thrown: both rolled back
  ;; If both succeed: both committed
)
```

## Failure Scenarios

**Mid-transaction error:**
```common-lisp
(with-transaction (db)
  (execute-non-query db "INSERT INTO t VALUES (1)")
  (error "Oops")  ; ROLLBACK executes, both changes discarded
)
```

**Constraint violation:**
```common-lisp
(with-transaction (db)
  (execute-non-query db "INSERT INTO users (id, name) VALUES (1, 'Alice')")
  (execute-non-query db "INSERT INTO users (id, name) VALUES (1, 'Bob')")
  ;; Second insert fails (duplicate id), entire transaction rolled back
)
```

## Implementation

**Via unwind-protect:** Guarantees cleanup
```common-lisp
(unwind-protect
    (progn
      (execute-non-query db "BEGIN TRANSACTION")
      ,@body
      (execute-non-query db "COMMIT"))
  (execute-non-query db "ROLLBACK"))
```

---

**Document Status:** Canonical
**Last Verified:** 2026-01-20 (Canon Initiation Pass 4)
