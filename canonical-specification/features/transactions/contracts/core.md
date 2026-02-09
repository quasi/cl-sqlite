---
type: contract
name: transactions-core
version: 1.0.0
feature: transactions
---

# Transactions Contract

## Purpose
Provides automatic transaction management with guaranteed commit or rollback via the `with-transaction` macro.

## Macro

#### `with-transaction`
Wraps a body of code in a SQLite transaction with automatic cleanup.

**Parameters:**
```json
{
  "type": "object",
  "properties": {
    "db": {
      "type": "object",
      "description": "sqlite-handle (evaluated once and bound)"
    },
    "body": {
      "type": "forms",
      "description": "Lisp forms to execute within transaction"
    }
  },
  "required": ["db", "body"]
}
```

**Behavior:**
- Executes `BEGIN TRANSACTION` before body
- If body completes normally: executes `COMMIT TRANSACTION`
- If body exits via non-local exit (error, throw, etc.): executes `ROLLBACK TRANSACTION`
- Returns the value(s) of the last form in body

**Example:**
```lisp
(with-transaction db
  (insert db 'accounts :balance 100)
  (update-table db 'accounts
                :data '(:balance 150)
                :where '(:= :id 1)))
```

**Guarantees:**
- Transaction is ALWAYS ended (commit or rollback)
- Database is never left in a transaction state after macro exits
- Non-local exits (errors, restarts, throws) trigger rollback

**Errors:**
- `sqlite-error` when BEGIN/COMMIT/ROLLBACK fails
- Propagates any errors from body (after rollback)

**Restarts:**
Inherits restarts from `execute-non-query`:
- `retry-query` - Retry the failed operation
- `skip-query` - Skip and continue

## Implementation Details

**SQL Commands:**
```sql
BEGIN TRANSACTION    -- Executed before body
COMMIT TRANSACTION   -- Executed if body succeeds
ROLLBACK TRANSACTION -- Executed if body fails
```

**Nesting:**
- SQLite does not support nested transactions
- Nested `with-transaction` calls will fail with `:ERROR` ("cannot start a transaction within a transaction")
- Use savepoints (not provided by this macro) for nested transaction-like behavior

## Invariants

1. **RULE-TXN-001:** Transaction is ALWAYS concluded (commit or rollback)
2. **RULE-TXN-002:** Rollback occurs on any non-local exit
3. **RULE-TXN-003:** Return value(s) are preserved from body
4. **RULE-TXN-004:** DB parameter is evaluated exactly once
5. **RULE-TXN-005:** Cannot nest transactions (SQLite limitation)
