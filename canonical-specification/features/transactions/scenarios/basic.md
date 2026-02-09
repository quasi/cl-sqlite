---
type: scenario
name: transactions-basic
feature: transactions
covers:
  - core
---

# transactions Basic Scenario

## Given
- A database connection (in-memory or file-based)
- Tables that will be modified within transactional contexts
- Operations that may succeed or fail during execution

## When
- Using with-transaction to wrap multiple database operations
- Executing INSERT, UPDATE, or DELETE operations within the transaction
- Transaction body completes successfully (normal exit)
- Transaction body signals an error or throws an exception
- BLOB data (small or large) is inserted and retrieved within transactions

## Then
- Successful transaction completion commits all changes
- Transaction rollback occurs automatically when an error is signaled
- All operations within a rolled-back transaction are reverted (no partial changes)
- Data integrity is maintained across transaction boundaries
- BLOB data (binary arrays) is correctly stored and retrieved
- Large BLOBs (e.g., 10,000 bytes) are handled without corruption
- Committed data persists and is visible in subsequent queries
- Rolled-back data does not appear in the database
