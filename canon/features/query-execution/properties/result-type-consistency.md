# Property: Result Type Consistency

**ID:** TYPE-001
**Confidence:** 0.95
**Status:** Verified
**Type:** Semantic Property

---

## Statement

```
∀ result column c, SQLite type t:
  query_result(c) = convert(t) where convert is deterministic type mapping
```

**Natural Language:**
SQLite column types map consistently to Lisp types across all result retrieval functions.

---

## Type Mapping

### SQLite → Lisp Conversion

| SQLite Type | Lisp Type | Conversion | Example |
|------------|-----------|-----------|---------|
| INTEGER | `integer` | Direct | `42` → `42` |
| REAL | `double-float` | Direct | `3.14` → `3.14d0` |
| TEXT | `string` | Direct | `"hello"` → `"hello"` |
| BLOB | `(simple-array (unsigned-byte 8) (*))` | Byte vector | Binary data |
| NULL | Keyword | `:null` | `NULL` → `:null` |

### NULL Handling

**NULL values always map to `:null` keyword**
- Not `nil` (to distinguish from missing value)
- Not `false` (to distinguish from boolean)
- Explicit sentinel value

```common-lisp
(execute-single db "SELECT NULL")
;; => :null

(execute-single db "SELECT email FROM users WHERE id = ?" 999)
;; => :null (if no such user, or email IS NULL)
```

---

## Enforcement Mechanisms

### In statement-column-value

**Location:** `sqlite.lisp:282-305`

```common-lisp
(defun statement-column-value (stmt column-index)
  (let ((col-type (sqlite-ffi:sqlite3-column-type stmt column-index)))
    (case col-type
      (:null :null)
      (:integer (sqlite-ffi:sqlite3-column-int64 stmt column-index))
      (:float (sqlite-ffi:sqlite3-column-double stmt column-index))
      (:text (sqlite-ffi:sqlite3-column-text stmt column-index))
      (:blob (sqlite-ffi:sqlite3-column-blob stmt column-index))
      (t (error "Unknown column type: ~A" col-type)))))
```

**Key mechanism:**
- Checks SQLite column type at runtime
- Dispatches to correct conversion function
- Deterministic (same input → same output)

---

### Conversion Consistency

**All result functions use `statement-column-value`:**
- `execute-to-list` → collects via `statement-column-value`
- `execute-single` → returns first column via `statement-column-value`
- `execute-one-row-m-v` → collects all columns via `statement-column-value`

**Result:** All functions return consistent types for same column

---

## Verification

### Type Consistency Test

```common-lisp
(deftest test-type-consistency ()
  ;; Create test table with all types
  (execute-non-query db
    "CREATE TABLE types_test (
       id INTEGER,
       val REAL,
       msg TEXT,
       data BLOB,
       opt_val INTEGER)")

  (execute-non-query db
    "INSERT INTO types_test VALUES (?, ?, ?, ?, ?)"
    42 3.14 "hello" #(1 2 3) :null)

  ;; Verify single row via all functions
  (let ((row (execute-to-list db "SELECT * FROM types_test")))
    (destructuring-bind ((id val msg data opt)) row
      (assert-equal 42 id)
      (assert-true (typep val 'double-float))
      (assert-equal "hello" msg)
      (assert-true (typep data '(simple-array (unsigned-byte 8) (*))))
      (assert-equal :null opt)))

  ;; Verify via execute-one-row-m-v
  (multiple-value-bind (id val msg data opt)
      (execute-one-row-m-v db "SELECT * FROM types_test")
    (assert-equal 42 id)
    (assert-true (typep val 'double-float))
    (assert-equal "hello" msg)
    (assert-true (typep data '(simple-array (unsigned-byte 8) (*))))
    (assert-equal :null opt)))
```

---

## Implications

### Predictable Type Handling

**Users know what to expect:**
```common-lisp
;; Always returns integer or :null
(let ((age (execute-single db "SELECT age FROM users WHERE id = ?" id)))
  (if (eq age :null)
      (format t "Age unknown~%")
      (format t "Age: ~D~%" age)))
```

---

### Type Guards Not Needed

**Don't need to check column type:**
```common-lisp
;; Works reliably - type is always consistent
(let ((results (execute-to-list db "SELECT id, name FROM users")))
  (dolist (row results)
    (destructuring-bind (id name) row
      ;; id is always integer (unless NULL → :null)
      ;; name is always string (unless NULL → :null)
      (process-user id name))))
```

---

## Edge Cases

### SQLite Type Affinity

**SQLite uses type affinity, not strict typing:**
```sql
CREATE TABLE mixed (
  id INTEGER,
  value TEXT)

INSERT INTO mixed VALUES (1, '42')    -- id: 1 (integer), value: '42' (string)
INSERT INTO mixed VALUES ('1', '42')  -- id: 1 (int affinity), value: '42' (string)
```

**Result:** Type affinity affects storage, not retrieval
- Affinity applied during storage
- Retrieval reflects actual type stored
- Conversion consistent with stored type

---

### NULL vs Empty String

**NULL ≠ empty string:**
```common-lisp
(execute-single db "SELECT NULL")
;; => :null

(execute-single db "SELECT ''")
;; => ""

(execute-single db "SELECT 0")
;; => 0
```

---

### Numeric Overflow

**Lisp integers can represent any SQLite INTEGER:**
```common-lisp
(execute-single db "SELECT 9223372036854775807")
;; => 9223372036854775807 (MAX INT64)
```

---

## Related Properties

- **RULE-005 (Column Index Base):** Columns 0-indexed in result rows
- **Parameter Type Safety:** Parameters have parallel type system

---

## Test Evidence

**All tests implicitly verify type consistency**

**Documentation:**
- README.md (examples show type usage)
- REFERENCE.md (return type documentation)
- CL-SQLITE.agent.md:273-280 (type mapping)

---

**Document Status:** Canonical
**Last Verified:** 2026-01-20 (Canon Initiation Pass 4)
