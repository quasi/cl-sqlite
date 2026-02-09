---
type: scenario
name: core-operations-basic
feature: core-operations
covers:
  - core
---

# core-operations Basic Scenario

## Given
- An in-memory database (:memory:) or file-based database
- A table schema defined (e.g., users with id, user_name, age columns)
- Test data inserted using parameterized queries

## When
- Executing non-queries (CREATE TABLE, INSERT, UPDATE, DELETE) with parameters
- Executing single-value queries (execute-single, execute-single/named)
- Executing multi-value row queries (execute-one-row-m-v, execute-one-row-m-v/named)
- Executing queries returning result sets (execute-to-list)
- Iterating over query results using in-sqlite-query and in-sqlite-query/named
- Directly using prepared statements with bind-parameter, step-statement, statement-column-value
- Using statement metadata (statement-column-names, statement-bind-parameter-names)
- Resetting statements for reuse with reset-statement
- Clearing bindings with clear-statement-bindings

## Then
- Tables are created successfully
- Data is inserted correctly with proper parameter binding
- Single values are retrieved accurately
- Multiple values from one row are returned via multiple-value-bind
- Full result sets are returned as lists of lists
- Iteration constructs yield correct filtered results
- Prepared statements can be reused after reset
- Named parameters ($var) and positional parameters (?) both work
- Constraint violations raise appropriate errors (sqlite-constraint-error)
