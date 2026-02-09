---
type: scenario
name: statement-cache-basic
feature: statement-cache
covers:
  - core
---

# statement-cache Basic Scenario

## Given
- A database connection is open
- Multiple queries will be executed using the same SQL strings
- Performance benefits are desired for repeated query patterns

## When
- The same SQL query is executed multiple times (e.g., in a loop)
- execute-non-query, execute-single, or other execution functions are called with identical SQL
- Prepared statements are implicitly created and cached by the library
- Cached statements are automatically finalized when the database connection closes

## Then
- SQL strings are used as cache keys for prepared statements
- First execution of a query causes statement preparation
- Subsequent executions with the same SQL reuse the cached statement
- Statement preparation overhead is avoided on repeated executions
- Cache is transparently managed without user intervention
- All cached statements are properly finalized on disconnect to prevent leaks
