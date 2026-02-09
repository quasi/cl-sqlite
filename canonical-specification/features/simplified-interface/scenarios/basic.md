---
type: scenario
name: simplified-interface-basic
feature: simplified-interface
covers:
  - core
---

# simplified-interface Basic Scenario

## Given
- A database connection (in-memory or file-based)
- A need for high-level, Lispy table operations
- Tables defined using Lisp-friendly syntax

## When
- Creating tables with create-table using keyword/symbol column definitions
- Defining column types (:integer, :text, :blob) and constraints (:primary-key, :not-null, :autoincrement)
- Inserting rows using insert with property lists (:name "Alice" :age 30)
- Selecting rows using select with :where, :columns, :order-by, :limit, :offset
- Building WHERE clauses with operators (:=, :>, :<, :and, :or, :not, :in)
- Updating rows using update-table with property list values and WHERE clauses
- Deleting rows using delete-from with WHERE clauses
- Using automatic SQL identifier normalization (keywords/symbols → SQL names, hyphens → underscores)

## Then
- Tables are created with the correct schema
- Column definitions properly map Lisp keywords to SQL types and constraints
- Data is inserted safely using parameterized queries (no SQL injection)
- SELECT queries return correct filtered, ordered, and limited result sets
- Complex WHERE clauses (AND, OR, NOT, IN) work correctly
- UPDATE operations modify only matching rows
- DELETE operations remove only matching rows
- Input validation rejects unsafe identifiers (SQL injection attempts, invalid characters)
- LIMIT and OFFSET parameters are validated as non-negative integers
- ORDER BY direction is validated as :asc or :desc only
- Invalid operators in WHERE clauses signal errors
