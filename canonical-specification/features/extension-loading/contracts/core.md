---
type: contract
name: extension-loading-core
version: 1.0.0
feature: extension-loading
---

# Extension Loading Contract

## Purpose
Provides functions to enable and load SQLite extensions (shared libraries) at runtime. Required for vector search (sqlite-vec), full-text search, and other extension functionality.

## Security Note

Extension loading is a **security-sensitive operation**. Extensions run with full SQLite permissions and can execute arbitrary code. Only load trusted extensions from verified sources.

## API Functions

#### `enable-load-extension`
Enables or disables the ability to load extensions for a database connection.

**Parameters:**
```json
{
  "type": "object",
  "properties": {
    "db": {
      "type": "object",
      "description": "sqlite-handle"
    },
    "onoff": {
      "type": "boolean",
      "description": "T to enable, NIL to disable",
      "default": true
    }
  },
  "required": ["db"]
}
```

**Returns:** No meaningful return value (NIL)

**Errors:**
- `sqlite-error` when SQLite rejects the request (e.g., compile-time disabled)

**Security:**
- Extension loading is disabled by default in SQLite
- Must be explicitly enabled before calling `load-extension`
- Should be disabled immediately after loading extensions
- Some SQLite builds disable extension loading at compile-time

**Example:**
```lisp
(enable-load-extension db t)        ; Enable
(load-extension db "vec0")          ; Load extension
(enable-load-extension db nil)      ; Disable for security
```

#### `load-extension`
Loads a shared library extension into the database connection.

**Parameters:**
```json
{
  "type": "object",
  "properties": {
    "db": {
      "type": "object",
      "description": "sqlite-handle"
    },
    "path": {
      "type": "string",
      "description": "Path to shared library (.so, .dylib, .dll)"
    },
    "entry-point": {
      "type": "string",
      "description": "Name of entry point function (optional)",
      "default": "sqlite3_extension_init"
    }
  },
  "required": ["db", "path"]
}
```

**Returns:** No meaningful return value (NIL)

**Errors:**
- `sqlite-error` with detailed message when:
  - Extension loading is not enabled (call `enable-load-extension` first)
  - Shared library file not found
  - Entry point function not found
  - Extension initialization fails
  - Incompatible SQLite version

**Error Message Extraction:**
SQLite provides detailed error messages via `sqlite3_load_extension` error output. These are captured and included in the `sqlite-error` condition.

**Platform-Specific Paths:**
- macOS: `.dylib` files (e.g., `libsqlitevec.dylib`)
- Linux: `.so` files (e.g., `libsqlitevec.so`)
- Windows: `.dll` files (e.g., `sqlitevec.dll`)

**Example:**
```lisp
;; Load sqlite-vec extension
(enable-load-extension db)
(load-extension db "/usr/local/lib/vec0.dylib")

;; Load with explicit entry point
(load-extension db "/path/to/custom.so" "my_custom_init")
```

## Common Extensions

### sqlite-vec (vec0)
Vector similarity search:
```lisp
(enable-load-extension db)
(load-extension db "vec0")  ; Or full path
(create-vector-table db 'embeddings '((vec 1536)))
```

### FTS5 (Full-Text Search)
Usually compiled-in, but can be loaded if needed:
```lisp
(load-extension db "fts5")
```

## Memory Management

**Extension Lifetime:**
- Extensions remain loaded for the lifetime of the database connection
- Closing the connection unloads all extensions
- Re-opening the database requires re-loading extensions

**Resource Cleanup:**
The FFI layer properly manages error message memory:
- Error messages from `sqlite3_load_extension` are allocated by SQLite
- Must be freed using `sqlite3_free` after reading
- The implementation handles this automatically

## Invariants

1. **RULE-EXT-001:** Extension loading must be enabled before calling `load-extension`
2. **RULE-EXT-002:** Extensions are loaded per-connection (not shared across connections)
3. **RULE-EXT-003:** Error messages are properly freed to prevent memory leaks
4. **RULE-EXT-004:** Default entry point is `sqlite3_extension_init`
5. **RULE-EXT-005:** Extension loading should be disabled after loading for security
6. **RULE-EXT-006:** Extensions persist for the connection lifetime
