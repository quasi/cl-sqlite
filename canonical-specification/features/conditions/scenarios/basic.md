---
type: scenario
name: conditions-basic
feature: conditions
covers:
  - core
---

# conditions Basic Scenario

## Given
- A database with tables that have constraints (NOT NULL, UNIQUE, PRIMARY KEY, FOREIGN KEY)
- SQL operations that may violate constraints or encounter errors
- Error handling code that catches specific SQLite condition types

## When
- Inserting NULL into a NOT NULL column
- Violating UNIQUE or PRIMARY KEY constraints
- Referential integrity violations on FOREIGN KEY constraints
- Other database errors occur during execution

## Then
- Constraint violations signal sqlite-constraint-error
- The error condition is catchable using handler-case or signals macro
- Error messages provide diagnostic information about the failure
- The error hierarchy allows catching specific error types or general sqlite-error
- Transactions can be rolled back in response to errors
- User code can gracefully handle database errors without crashing
