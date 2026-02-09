---
type: scenario
name: error-handling-basic
feature: error-handling
covers:
  - core
---

# error-handling Basic Scenario

## Given
- A database with concurrent access from multiple threads
- A busy timeout configured for handling SQLITE_BUSY errors
- Tables with constraints that may be violated
- Input validation on user-provided identifiers and parameters

## When
- Multiple threads attempt to write to the database simultaneously
- Busy timeout is set (e.g., 60000ms) using with-open-database :busy-timeout option
- Concurrent insert operations execute from multiple threads
- Constraint violations occur (NOT NULL, UNIQUE, PRIMARY KEY, FOREIGN KEY)
- Invalid SQL identifiers are provided (injection attempts, invalid characters)
- Invalid parameter values are provided (negative LIMIT/OFFSET, invalid ORDER BY direction)
- Unknown operators appear in WHERE clauses

## Then
- SQLITE_BUSY errors are retried according to the busy-timeout setting
- Concurrent operations complete successfully without data corruption
- Constraint violations signal specific error conditions (sqlite-constraint-error)
- Input validation catches and rejects unsafe identifiers before SQL generation
- Invalid parameters signal errors with clear messages
- Error hierarchy allows catching general (sqlite-error) or specific errors
- All threads can complete their operations when timeout is sufficient
- Test suite verifies no thread encounters SQLITE_BUSY with adequate timeout
