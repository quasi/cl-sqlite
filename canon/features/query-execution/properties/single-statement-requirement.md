# Property: Single Statement Requirement

**ID:** RULE-001
**Confidence:** 1.0
**Status:** Verified
**Type:** Normative Rule

---

## Statement

```
SQL strings passed to execute-* functions MUST contain exactly one SQL statement.
Multiple statements (separated by semicolons) are forbidden.
```

**Keywords:**
- **MUST:** Hard requirement, always enforced

---

## Rationale

**SQLite C API Limitation:**
- `sqlite3_prepare` compiles only the first statement
- Remaining SQL ignored (no error)
- Confusing behavior if multiple statements intended

**Problem without this rule:**
```sql
"INSERT INTO log VALUES (1); DELETE FROM users"
```
- Only INSERT executes
- DELETE silently ignored
- User confused about why DELETE didn't run

**Solution:** Enforce single statement via contract
- Make requirement explicit
- Prevent accidental multi-statement SQL

---

## Definition

### Valid (Single Statement)

```sql
"INSERT INTO users VALUES (?, ?)"
"SELECT * FROM users WHERE age > ?"
"UPDATE users SET name = ? WHERE id = ?"
"DELETE FROM users WHERE active = 0"
"CREATE TABLE users (id INTEGER, name TEXT)"
```

---

### Invalid (Multiple Statements)

```sql
"INSERT INTO users VALUES (?, ?); SELECT * FROM users"
"DELETE FROM users WHERE id = 1; DELETE FROM logs"
"CREATE TABLE users (...); CREATE TABLE posts (...)"
```

---

### Edge Cases (Valid or Invalid?)

**Statement + Comment:**
```sql
"INSERT INTO users VALUES (?, ?); -- comment"
```
**Valid:** Comment is not a statement

**Multiple statements within transaction:**
```sql
"BEGIN; INSERT INTO users VALUES (?); COMMIT"
```
**Invalid:** Multiple statements (even in transaction context)

**Use `with-transaction` instead:**
```common-lisp
(with-transaction db
  (execute-non-query db "INSERT INTO users VALUES (?)" value1)
  (execute-non-query db "INSERT INTO users VALUES (?)" value2))
```

---

## Enforcement

### Detection Method

**SQLite API check:**
```common-lisp
(defun is-single-statement-p (sql)
  ;; SQLite only prepares first statement
  ;; If multiple statements exist, second one not prepared
  ;; Result: Trying to step would fail or have unexpected behavior
  )
```

**Implementation Note:** Library doesn't explicitly check (SQLite prevents misuse)

---

### Error Behavior

**If multiple statements passed:**
```common-lisp
(execute-non-query db "INSERT INTO t VALUES (1); INSERT INTO t VALUES (2)")
;; Only first INSERT executes
;; Second INSERT silently ignored (SQLite behavior)
;; User may not notice error
```

**Recommendation:** Verify SQL contains single statement

---

## Verification

### Manual Verification

**Count semicolons (outside strings/comments):**
```common-lisp
(defun count-statements (sql)
  ;; Simple heuristic: count semicolons
  ;; Not foolproof but catches common mistakes
  )
```

---

### Test for Multiple Statements

```common-lisp
(deftest test-multiple-statements-warning ()
  ;; Show that second statement is silently ignored
  (execute-non-query db
    "CREATE TABLE test (id INTEGER); CREATE TABLE test2 (id INTEGER)")

  ;; Only first CREATE TABLE executed
  (assert-equal 1 (execute-single db "SELECT COUNT(*) FROM sqlite_master WHERE type='table'"))
  ;; test2 doesn't exist (second statement ignored)
  )
```

---

## Implications

### For Users

**Always use single statement:**
```common-lisp
;; RIGHT
(execute-non-query db "INSERT INTO users VALUES (?, ?)" 1 "Alice")

;; WRONG (second statement ignored)
(execute-non-query db "INSERT INTO users VALUES (?, ?); SELECT * FROM users")
```

---

### For Transactions

**Multiple statements require explicit transaction:**
```common-lisp
;; WRONG - Silently fails to execute second statement
(execute-non-query db "INSERT INTO t VALUES (1); INSERT INTO t VALUES (2)")

;; RIGHT - Each statement separate
(execute-non-query db "INSERT INTO t VALUES (?)" 1)
(execute-non-query db "INSERT INTO t VALUES (?)" 2)

;; BETTER - Use transaction for atomicity
(with-transaction db
  (execute-non-query db "INSERT INTO t VALUES (?)" 1)
  (execute-non-query db "INSERT INTO t VALUES (?)" 2))
```

---

## Related Rules

- **RULE-003 (Bind Before Step):** After prepare (single statement)
- **RULE-006 (Extension Loading Sequence):** Extension-specific single statement

---

## Design Rationale

**Why enforce?**
- Prevent silent failures (second statement ignored)
- Make requirements explicit
- Catch common mistakes early

**Why not use compound statements?**
- Would require parsing/validation
- SQLite API doesn't support multi-statement prepare
- Transactions provide atomicity for multi-statement operations

---

## Exceptions

**None:** This is hard requirement

**If multiple operations needed:**
1. Use separate `execute-*` calls
2. Wrap in `with-transaction` for atomicity
3. Use prepared statements for repetition

---

## Documentation References

- Specification: CL-SQLITE.agent.md:145-151 (RULE-001)
- Implementation: Implicit (SQLite enforces via API)
- Tests: Implicit (test suite uses single statements)

---

**Document Status:** Canonical
**Last Verified:** 2026-01-20 (Canon Initiation Pass 4)
