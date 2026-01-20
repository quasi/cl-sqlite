# Contract: statement-column-value

**Signature:** `(statement-column-value stmt column-index) => value`
**Confidence:** 0.95
**Status:** Stable

---

## Parameters

### stmt
**Type:** `sqlite-statement`
**Required:** Yes
**Description:** Prepared statement with current row

**Constraints:**
- Must be valid `sqlite-statement` instance
- Must be at a row (after step-statement returned T)

---

### column-index
**Type:** `integer`
**Required:** Yes
**Description:** Column index (0-based, RULE-005)

**Constraints:**
- Must be between 0 and (column-count - 1)
- First column: index 0
- Last column: index (column-count - 1)

**Example:**
```
SELECT id, name, age
```
- Column 0: id
- Column 1: name
- Column 2: age

---

## Returns

**Type:** `T` (any type)
**Description:** Column value from current row

**Type mapping (TYPE-001):**
- SQLite INTEGER → Lisp `integer`
- SQLite REAL → Lisp `double-float`
- SQLite TEXT → Lisp `string`
- SQLite BLOB → Lisp `(simple-array (unsigned-byte 8) (*))`
- SQLite NULL → Keyword `:null`

**Examples:**
```common-lisp
(statement-column-value stmt 0)  ; => 42 (integer)
(statement-column-value stmt 1)  ; => "Alice" (string)
(statement-column-value stmt 2)  ; => 3.14 (double-float)
(statement-column-value stmt 3)  ; => :null (NULL value)
(statement-column-value stmt 4)  ; => #(1 2 3) (blob)
```

---

## Side Effects

None (read-only)

---

## Errors

### sqlite-error
**When:** Column access fails

**Common error codes:**
- `:RANGE` - Column index out of range

---

## Preconditions

- Statement has current row (after step-statement returned T)
- Column index valid (0 to column-count-1)

---

## Postconditions

**Success:**
- Returns column value
- Type consistent with column type

**Failure:**
- `sqlite-error` signaled
- Statement state unchanged

---

## Implementation

**Location:** `sqlite.lisp:282-305`

**Key Code:**
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

**Mechanism:**
1. Queries SQLite for column type
2. Dispatches to appropriate FFI function based on type
3. Returns converted Lisp value

---

## Usage Examples

### Access Single Column
```common-lisp
(let ((stmt (prepare-statement db "SELECT id FROM users WHERE id = ?")))
  (bind-parameter stmt 1 42)
  (when (step-statement stmt)
    (let ((id (statement-column-value stmt 0)))
      (format t "User ID: ~D~%" id)))
  (finalize-statement stmt))
```

---

### Access Multiple Columns
```common-lisp
(let ((stmt (prepare-statement db "SELECT id, name, age FROM users")))
  (when (step-statement stmt)
    (let ((id (statement-column-value stmt 0))
          (name (statement-column-value stmt 1))
          (age (statement-column-value stmt 2)))
      (format t "~A (~D) age ~D~%" name id age)))
  (finalize-statement stmt))
```

---

### Loop Over Columns
```common-lisp
(let ((stmt (prepare-statement db "SELECT * FROM users")))
  (when (step-statement stmt)
    (loop for i from 0 below (statement-column-count stmt)
          do (format t "Col ~D: ~A~%" i (statement-column-value stmt i))))
  (finalize-statement stmt))
```

---

### Handle NULL
```common-lisp
(let ((stmt (prepare-statement db "SELECT email FROM users WHERE id = ?")))
  (bind-parameter stmt 1 42)
  (when (step-statement stmt)
    (let ((email (statement-column-value stmt 0)))
      (if (eq email :null)
          (format t "Email unknown~%")
          (format t "Email: ~A~%" email))))
  (finalize-statement stmt))
```

---

### Type-Specific Processing
```common-lisp
(let ((stmt (prepare-statement db "SELECT name, salary, data FROM employees")))
  (when (step-statement stmt)
    (let ((name (statement-column-value stmt 0))      ; TEXT
          (salary (statement-column-value stmt 1))    ; REAL
          (data (statement-column-value stmt 2)))     ; BLOB
      (format t "~A earns $~,2F~%" name salary)
      (format t "Data: ~D bytes~%" (length data)))))
```

---

## Related Contracts

- `step-statement` - Advances to row (needed before column access)
- `statement-column-count` - Gets column count (for loop bounds)
- `prepare-statement` - Creates statement (defines column schema)

---

## Patterns

**Recommended:** Use execute functions (automatic column retrieval)
```common-lisp
(execute-to-list db "SELECT * FROM users")
;; Column retrieval automatic
```

**Manual retrieval:** For control over which columns to access
```common-lisp
(let ((stmt (prepare-statement db "SELECT * FROM users")))
  (unwind-protect
      (loop while (step-statement stmt)
            collect (list (statement-column-value stmt 0)   ; Only first column
                          (statement-column-value stmt 2))) ; Only third column
    (finalize-statement stmt)))
```

---

## Type Safety

**Type consistency (TYPE-001):**
- Same column always returns same type
- Users can rely on type for processing

```common-lisp
(let ((age (statement-column-value stmt 0)))
  ;; If column is INTEGER type: age is always integer or :null
  ;; Safe to use in arithmetic
  (when (not (eq age :null))
    (format t "In 10 years: ~D~%" (+ age 10))))
```

---

## Performance Notes

- **Access cost:** O(1) per column
- **Type dispatch:** Fast (case statement)
- **Memory:** Value copied from SQLite to Lisp

---

## SQLite Type Affinity

**SQLite applies type affinity during storage:**
```common-lisp
(execute-non-query db "CREATE TABLE t (id INTEGER, name TEXT)")
(execute-non-query db "INSERT INTO t VALUES (?, ?)" "123" "Bob")

;; Even though we inserted string "123", it's stored as INTEGER 123
(let ((result (execute-to-list db "SELECT id FROM t")))
  ;; => ((123))  Not ("123")
```

**Consequence:** Retrieved type may differ from insert type

---

## Edge Cases

### NULL Column
```common-lisp
(statement-column-value stmt 0)  ; Returns :null if column is NULL
```

---

### Large BLOB
```common-lisp
;; BLOB returned as byte vector (may be large)
(let ((data (statement-column-value stmt 0)))
  (format t "Data size: ~D bytes~%" (length data)))
```

---

### Missing Column (Index Out of Range)
```common-lisp
(statement-column-value stmt 99)  ; If only 5 columns exist
;; => sqlite-error :RANGE
```

---

## Validation

**Tests:**
- Implicit in all result retrieval tests
- `execute-to-list`, `execute-single` use this internally

**Documentation:**
- README.md (examples with column access)
- REFERENCE.md:128-138
- CL-SQLITE.agent.md:887

**Verified Properties:**
- Correct type conversion
- NULL handling
- Column bounds checking

---

**Document Status:** Canonical
**Last Verified:** 2026-01-20 (Canon Initiation Pass 2)
