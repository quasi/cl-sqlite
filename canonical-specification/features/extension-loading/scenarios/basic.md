---
type: scenario
name: extension-loading-basic
feature: extension-loading
covers:
  - core
---

# extension-loading Basic Scenario

## Given
- A database connection (in-memory or file-based)
- A compiled SQLite extension library file (e.g., vec0.so) on the filesystem
- Extension loading initially disabled for security

## When
- Enabling extension loading using enable-load-extension with flag t
- Loading an extension using load-extension with the library path
- Passing a null-pointer for the entry-point parameter to use the default entry point
- Utilizing extension-provided functions and virtual tables after loading
- Disabling extension loading using enable-load-extension with flag nil after loading

## Then
- Extension loading is successfully enabled on the database connection
- The extension library loads without errors
- Extension-provided SQL functions are available for use in queries
- Extension-provided virtual table constructors are available
- Vector operations (for vec0 extension) work correctly
- Extension loading can be disabled after loading to prevent further changes
- Security is maintained by requiring explicit enable-load-extension call
- Extensions are properly initialized and their features are accessible
