# Contract: with-transaction

**Signature:** `(with-transaction (db) &body body) => result`
**Confidence:** 0.85
**Status:** Stable
**Form:** Macro

---

## Parameters
- **db:** Connected database handle
- **body:** Forms to execute within transaction

## Returns
- Value of last form in body

## Behavior
- Executes BEGIN TRANSACTION before body
- Executes COMMIT on success
- Executes ROLLBACK on error/non-local exit
- All-or-nothing atomicity guaranteed

## Implementation
```common-lisp
(defmacro with-transaction ((db) &body body)
  `(unwind-protect
       (progn
         (execute-non-query ,db "BEGIN TRANSACTION")
         ,@body
         (execute-non-query ,db "COMMIT"))
     (execute-non-query ,db "ROLLBACK")))
```

## Usage
```common-lisp
(with-transaction (db)
  (execute-non-query db "INSERT INTO accounts SET balance = balance - 100 WHERE id = 1")
  (execute-non-query db "INSERT INTO accounts SET balance = balance + 100 WHERE id = 2"))
;; Both inserts committed atomically or rolled back on error
```

## Errors
- `sqlite-error` if transaction fails

## Validation
- Used in examples
- Implicit in test patterns

---

**Document Status:** Canonical
**Last Verified:** 2026-01-20 (Canon Initiation Pass 2)
