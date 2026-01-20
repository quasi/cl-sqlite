# Property: Column Index Base

**ID:** RULE-005
**Confidence:** 1.0
**Status:** Verified
**Type:** Normative Rule

---

## Statement

```
Column indices in result rows are 0-indexed.
First column is index 0, second is index 1, etc.
```

**Contrast with Parameter Index Base (RULE-004):**
- **Parameters:** 1-indexed (position 1, 2, 3, ...)
- **Columns:** 0-indexed (index 0, 1, 2, ...)

---

## Rationale

**Two Different Index Spaces:**

1. **Parameter Space (1-indexed):**
   - Matches SQL convention (first ? is 1)
   - FFI directly uses SQLite C binding indices (1-indexed)

2. **Column Space (0-indexed):**
   - Matches Lisp convention (vectors, lists 0-indexed)
   - Matches SQLite C API retrieval (0-indexed)
   - Natural for Lisp programming

**Why different?** Different conventions in each domain
- Parameters bound via C API (uses SQLite convention: 1-indexed)
- Columns retrieved via C API (uses C convention: 0-indexed)
- Library exposes each as naturally as possible

---

## Column Indexing

### Direct Access

**When using `statement-column-value`:**
```common-lisp
(statement-column-value stmt 0)  ; First column
(statement-column-value stmt 1)  ; Second column
(statement-column-value stmt 2)  ; Third column
```

---

### In Result Lists

**From `execute-to-list`:**
```common-lisp
(let ((row (first (execute-to-list db "SELECT id, name, age FROM users"))))
  ;; row = (1 "Alice" 30)
  (first row)   ; = 1     (column 0: id)
  (second row)  ; = "Alice" (column 1: name)
  (third row)   ; = 30    (column 2: age))
```

**Using direct indexing:**
```common-lisp
(let ((row (first (execute-to-list db "SELECT id, name, age FROM users"))))
  ;; row = (1 "Alice" 30)
  (nth 0 row)   ; = 1
  (nth 1 row)   ; = "Alice"
  (nth 2 row)   ; = 30
```

---

### Common Mistake

❌ **Using 1-indexed for column access:**
```common-lisp
;; WRONG - Columns are 0-indexed
(let ((row (first (execute-to-list db "SELECT id, name FROM users"))))
  (nth 1 row))  ; = "Alice" (not id, name's name is not "id"!)

;; Would get second column instead of first
```

✅ **Correct - Use 0-indexing:**
```common-lisp
(let ((row (first (execute-to-list db "SELECT id, name FROM users"))))
  (nth 0 row))  ; = id (first column)
  (nth 1 row))  ; = "Alice" (second column)
```

---

## Implementation

### In statement-column-value

**Location:** `sqlite.lisp:282-305`

```common-lisp
(defun statement-column-value (stmt column-index)
  ;; column-index is 0-based (0, 1, 2, ...)
  ;; Passed directly to FFI (SQLite C API uses 0-based)
  (let ((col-type (sqlite-ffi:sqlite3-column-type stmt column-index)))
    (case col-type
      (:null :null)
      (:integer (sqlite-ffi:sqlite3-column-int64 stmt column-index))
      (:float (sqlite-ffi:sqlite3-column-double stmt column-index))
      ...)))
```

**Key:** Column index passed as-is to C API (both 0-indexed)

---

### In execute-to-list

```common-lisp
(defun execute-to-list (db sql &rest parameters)
  (let ((stmt (prepare-statement db sql)))
    ...
    (loop while (step-statement stmt)
          collect (loop for i from 0 below (statement-column-count stmt)
                        collect (statement-column-value stmt i)))
          ;; Loop from 0 ↑ (0-indexed)
    ...))
```

**Loop starts from 0:** Reflects 0-indexed convention

---

## Comparison with Parameters

### Parameters (RULE-004): 1-indexed

```common-lisp
(execute-non-query db
  "INSERT INTO users (name, age, city) VALUES (?, ?, ?)"
  "Alice" 30 "Portland")

;; Binding:
;; Position 1 → "Alice" (first ?)
;; Position 2 → 30 (second ?)
;; Position 3 → "Portland" (third ?)

;; Internally: bind-parameter calls
(bind-parameter stmt 1 "Alice")    ; Position 1 (not 0!)
(bind-parameter stmt 2 30)
(bind-parameter stmt 3 "Portland")
```

---

### Columns: 0-indexed

```common-lisp
(let ((row (first (execute-to-list db "SELECT name, age, city FROM users"))))
  ;; row = ("Alice" 30 "Portland")
  (nth 0 row)   ; "Alice" (column 0)
  (nth 1 row)   ; 30 (column 1)
  (nth 2 row)   ; "Portland" (column 2)
```

---

## Off-by-One Error Prevention

### Correct Pattern

```common-lisp
;; SELECT returns 3 columns (indices 0, 1, 2)
(let ((results (execute-to-list db "SELECT id, name, age FROM users")))
  (dolist (row results)
    (destructuring-bind (id name age) row
      ;; Destructuring handles indexing automatically
      (format t "~D: ~A (~D)~%" id name age))))
```

---

### Using Destructuring (Recommended)

```common-lisp
;; Destructuring avoids manual indexing
(let ((row (first (execute-to-list db "SELECT id, name FROM users"))))
  (destructuring-bind (id name) row
    ;; id and name bound in order (0-indexed implicitly)
    (format t "User: ~A (ID: ~D)~%" name id)))
```

---

### Manual Indexing (Advanced)

```common-lisp
;; If manual indexing needed, remember 0-base
(let ((row (first (execute-to-list db "SELECT id, name FROM users"))))
  (case some-column-selection
    (0 (format t "ID: ~A~%" (nth 0 row)))
    (1 (format t "Name: ~A~%" (nth 1 row)))))
```

---

## Related Rules

- **RULE-004 (Parameter Index Base):** Parameters are 1-indexed (opposite!)
- **RULE-001 (Single Statement):** SQL query that defines columns

---

## SQLite C API Correspondence

**SQLite C API convention:**
```c
// Column indices are 0-based in SQLite C API
sqlite3_column_int(stmt, 0)   // First column
sqlite3_column_text(stmt, 1)  // Second column
```

**cl-sqlite mirrors this convention:**
```common-lisp
(statement-column-value stmt 0)  ; First column (0-indexed)
(statement-column-value stmt 1)  ; Second column
```

**Consistency:** Direct mapping to C API

---

## Edge Cases

### Column Count

**Getting column count (0-indexed range):**
```common-lisp
(let ((col-count (statement-column-count stmt)))
  ;; Valid indices: 0 to (col-count - 1)
  (loop for i from 0 below col-count
        do (format t "Column ~D: ~A~%" i (statement-column-value stmt i))))
```

---

### Single Column

```common-lisp
;; Single column is at index 0
(let ((result (execute-to-list db "SELECT COUNT(*) FROM users")))
  (let ((count (first (first result))))
    ;; count = value at (column 0)
    ))
```

---

## Validation

**Tests:**
- Implicit in all result retrieval tests
- Loop iterations start from 0

**Documentation:**
- CL-SQLITE.agent.md:181-190 (RULE-005)
- README.md (examples use destructuring)
- REFERENCE.md (return type documentation)

---

## Mnemonics

**Remember:**
- **Parameters = 1, 2, 3, ...** (SQL-like, "first parameter")
- **Columns = 0, 1, 2, ...** (Lisp-like, "first element")

**Different domains, different conventions**

---

**Document Status:** Canonical
**Last Verified:** 2026-01-20 (Canon Initiation Pass 4)
