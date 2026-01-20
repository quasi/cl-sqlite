# ADR 0002: Simplified Lispy API

**Status:** Accepted
**Confidence:** 1.0
**Decision Date:** 2025-12-30
**Decided By:** quasi (google-labs-jules bot)
**Last Updated:** 2026-01-20

---

## Context

The original cl-sqlite API requires writing raw SQL strings:

```common-lisp
(execute-non-query db "CREATE TABLE users (id INTEGER, name TEXT)")
(execute-non-query db "INSERT INTO users (name) VALUES (?)" "Alice")
(execute-to-list db "SELECT * FROM users WHERE age > ?" 18)
```

**Problems with this approach:**
1. **Not idiomatic Lisp:** SQL strings are foreign to Lisp syntax
2. **SQL injection risk:** String concatenation easy to misuse
3. **No compile-time checking:** Typos only caught at runtime
4. **Verbose:** Requires knowledge of SQL syntax

**Observation:** Many common operations (CRUD) follow predictable patterns

---

## Decision

Add a "simplified Lispy API" layer that abstracts SQL behind s-expressions and plists.

**Core Functions:**
- `create-table` - Define schema with lists
- `insert` - Insert using plists
- `select` - Query with s-expression WHERE clauses
- `update-table` - Update with plist + WHERE
- `delete-from` - Delete with WHERE

**Key Feature:** S-expression WHERE clause compilation

```common-lisp
;; Before (SQL string)
(execute-to-list db "SELECT * FROM users WHERE age > ? AND active = ?" 18 1)

;; After (s-expression)
(select db :users
        :where '(:and (:> :age 18) (:= :active 1)))
```

---

## Rationale

### 1. Idiomatic Lisp

**Goal:** Make database operations feel like native Lisp

**Benefits:**
- S-expressions are first-class in Lisp (can be manipulated, composed)
- Plists are standard Lisp data structures
- Familiar syntax for Lisp programmers

**Example:**
```common-lisp
;; Composable WHERE clauses
(defun active-users-where (min-age)
  `(:and (:= :active 1) (:> :age ,min-age)))

(select db :users :where (active-users-where 18))
```

### 2. SQL Injection Prevention

**Problem:** String concatenation is dangerous

```common-lisp
;; WRONG - SQL injection vulnerability
(execute-non-query db
  (format nil "INSERT INTO users (name) VALUES ('~A')" user-input))
```

**Solution:** Simplified API never concatenates SQL
- WHERE clauses compiled to SQL with parameter placeholders
- Values passed as parameters (bound safely)
- No user input in SQL strings

**Code:**
```common-lisp
;; compile-where returns (values sql-string param-list)
(defun compile-where (where-clause)
  ...
  (values "age > ? AND active = ?" (list 18 1)))
```

### 3. Type Safety & Normalization

**Name Normalization:**
- Symbols, keywords, strings all accepted
- Normalized to lowercase (SQLite is case-insensitive)
- Consistent behavior: `:users`, `'users`, `"users"` all equivalent

```common-lisp
(defun normalize-name (name)
  (string-downcase (string name)))
```

**Type Normalization:**
- Column types normalized to uppercase
- Matches SQL convention (INTEGER, TEXT, BLOB)

### 4. Reduced Verbosity

**Comparison:**

```common-lisp
;; Standard API (verbose)
(execute-non-query db
  "CREATE TABLE users (id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT NOT NULL)")
(execute-non-query db "INSERT INTO users (name) VALUES (?)" "Alice")

;; Simplified API (concise)
(create-table db :users
              '((:id :integer :primary-key :autoincrement)
                (:name :text :not-null)))
(insert db :users '(:name "Alice"))
```

**Savings:** Fewer lines, clearer intent

---

## Design Details

### WHERE Clause Compilation

**Supported Operators:**
- Logical: `:and`, `:or`, `:not`
- Comparison: `:=`, `:<`, `:>`, `:<=`, `:>=`, `:<>`
- Pattern: `:like`
- Membership: `:in`
- Null checks: `:is-null`, `:is-not-null`

**Examples:**
```common-lisp
'(:= :name "Alice")              → "name = ?"
'(:> :age 18)                    → "age > ?"
'(:and (:> :age 18) (:< :age 65)) → "(age > ? AND age < ?)"
'(:in :status "active" "pending") → "status IN (?, ?)"
'(:is-null :deleted_at)          → "deleted_at IS NULL"
```

**Validation:** Unknown operators signal error (RULE-010)

### ORDER BY Support

**Syntax:**
```common-lisp
;; Single column
:order-by :name

;; With direction
:order-by '(:name :desc)

;; Multiple columns
:order-by '((:age :desc) (:name :asc))
```

### LIMIT / OFFSET Support

```common-lisp
(select db :users :limit 10 :offset 20)
```

---

## Consequences

### Positive

✅ **Beginner Friendly:** No SQL knowledge required for basic CRUD
- Lower barrier to entry
- Faster development for simple cases

✅ **SQL Injection Prevention:** By design, not by discipline
- WHERE clauses never concatenated
- Values always parameterized

✅ **Composability:** S-expressions can be programmatically generated
- Build complex queries from components
- Reuse WHERE clause fragments

✅ **Type Safety:** Name/type normalization prevents case errors

✅ **Testable:** S-expression → SQL compilation is pure function

### Negative

⚠️ **Limited Expressiveness:** Not all SQL can be expressed
- No JOINs in simple API
- No subqueries
- No window functions
- Mitigation: Fall back to standard API for complex queries

⚠️ **Learning Curve:** New syntax to learn
- S-expression WHERE syntax differs from SQL
- Mitigation: Comprehensive documentation + examples

⚠️ **Performance Overhead:** Extra compilation step
- WHERE clause compilation at runtime
- Mitigation: Negligible compared to SQL parsing

### Trade-offs

**Simplicity vs. Power:**
- Chose simplicity for common cases
- Power users can use standard/prepared API
- Layered design allows mixing APIs

**Abstraction vs. Transparency:**
- Chose abstraction (hide SQL)
- Trade-off: Less control over generated SQL
- Decision: Simple API for simple cases, others have alternatives

---

## Implementation

**Commit:** a4f3818 (2025-12-30)
**PR:** #5 jules/simple-abstraction
**Files Added:**
- `simple.lisp` (142 lines)
- `simple-tests.lisp` (71 lines)

**Commit Message:**
```
feat: Add simpler lispy abstraction layer

- Added `simple.lisp` which implements `create-table`, `insert`,
  `select`, `update-table`, `delete-from` and `compile-where`.
- Added `simple-tests.lisp` with comprehensive tests for the new
  abstraction layer.
- The new interface allows writing queries using s-expressions
  and plists instead of raw SQL strings.
```

**Test Coverage:** 9 scenarios (excellent)
- create-table with constraints
- insert with plist
- select with WHERE (multiple operators)
- update-table with WHERE
- delete-from with WHERE
- Complex WHERE clauses (AND, OR, IN, NOT)
- Error on unknown operator

---

## Alternatives Considered

### 1. SQL DSL (full SQL in s-expressions)
**Example:** `(select (* :from users :where (> age 18)))`
**Rejected:** Too complex, reinventing SQL parser

### 2. ORM (object-relational mapping)
**Example:** Active Record pattern
**Rejected:** Out of scope, library focused on SQLite access

### 3. Query Builder (fluent interface)
**Example:** `(query.from :users).where(:age).gt(18).limit(10)`
**Rejected:** Not idiomatic Lisp, method chaining awkward in Lisp

### 4. Macro-based EDSL
**Example:** `(with-table users (where (> age 18)))`
**Rejected:** Macros limit runtime composability

**Decision:** S-expression WHERE with compile-where function
- Runtime compilation allows programmatic generation
- Function-based (not macro) enables composition
- Balances power and simplicity

---

## Validation

**Git Evidence:** Commit a4f3818 with full rationale

**Code Evidence:**
- `simple.lisp` implements all functions
- `compile-where` handles all operators
- Tests verify behavior

**Usage Evidence:** Tests show intended usage patterns

**Documentation:**
- README.md sections 33-69 (Quick Start with Simplified Interface)
- REFERENCE.md sections 48-105 (Simplified Interface API)

---

## Adoption

**Status:** Stable, production-ready
**Confidence:** 1.0 (fully documented, tested, deployed)
**Recommendation:** Use for simple CRUD, fall back to standard API for complex queries

**When to use Simplified API:**
- Basic CRUD operations
- Simple WHERE clauses
- No JOINs or subqueries
- Prefer Lisp syntax over SQL

**When to use Standard API:**
- Complex queries (JOINs, subqueries)
- Performance-critical (prepared statements)
- Full SQL control needed
- Existing SQL queries to port

---

## Related Decisions

- **DEC-003:** WHERE Clause Compilation (part of this decision)
- **DEC-008:** Type Normalization (part of this decision)
- **ARCH-001:** Layered API Architecture (simplified API is top layer)

---

## References

- Git commit: a4f3818
- PR: #5 jules/simple-abstraction
- Code: `simple.lisp`, `simple-tests.lisp`
- Documentation: README.md:33-69, REFERENCE.md:48-105
- Specification: RULE-010 (WHERE operator validation)

---

**Document Status:** Canonical
**Confidence:** 1.0 (complete rationale in git history)
**Recommendation:** Accepted, use for simple cases
