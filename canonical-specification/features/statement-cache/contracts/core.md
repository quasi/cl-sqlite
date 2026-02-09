---
type: contract
name: statement-cache-core
version: 1.0.0
feature: statement-cache
---

# Statement Cache Contract

## Purpose
Implements a most-recently-used (MRU) cache for prepared statements to avoid repeated SQL compilation. Evicts least-recently-used entries when full.

## API Functions

### Cache Creation

#### `make-instance 'mru-cache`
Creates a new MRU cache.

**Parameters:**
```json
{
  "type": "object",
  "properties": {
    "cache-size": {
      "type": "integer",
      "description": "Maximum number of cached objects",
      "default": 100
    },
    "destructor": {
      "type": "function",
      "description": "Function called to destroy evicted objects",
      "default": "#'identity"
    }
  }
}
```

**Returns:** `mru-cache` instance

### Cache Operations

#### `get-from-cache`
Retrieves and removes the most recent cached object for a key.

**Parameters:**
```json
{
  "type": "object",
  "properties": {
    "cache": {
      "type": "object",
      "description": "mru-cache instance"
    },
    "id": {
      "type": "string",
      "description": "Cache key (SQL text for statements)"
    }
  },
  "required": ["cache", "id"]
}
```

**Returns:**
- Cached object if found
- `NIL` if nothing cached for the key

**Side Effects:**
- Decrements `total-cached` count
- Updates `last-access-time-table`
- Removes object from cache stack

#### `put-to-cache`
Stores an object in the cache under a key.

**Parameters:**
```json
{
  "type": "object",
  "properties": {
    "cache": {
      "type": "object",
      "description": "mru-cache instance"
    },
    "id": {
      "type": "string",
      "description": "Cache key (SQL text for statements)"
    },
    "object": {
      "description": "Object to cache"
    }
  },
  "required": ["cache", "id", "object"]
}
```

**Returns:** The cached object

**Side Effects:**
- May evict least-recently-used entry if cache is full
- Calls destructor on evicted object
- Increments `total-cached` count
- Updates `last-access-time-table`

#### `purge-cache`
Destroys all cached objects by calling the destructor on each.

**Parameters:**
```json
{
  "type": "object",
  "properties": {
    "cache": {
      "type": "object",
      "description": "mru-cache instance"
    }
  },
  "required": ["cache"]
}
```

**Returns:** No meaningful return value

**Side Effects:**
- Calls destructor on all cached objects
- Does NOT clear the cache tables (design decision)

## Invariants

1. **RULE-CACHE-001:** Cache never exceeds `cache-size` limit
2. **RULE-CACHE-002:** Eviction uses least-recently-used strategy
3. **RULE-CACHE-003:** `get-from-cache` removes the object from cache (caller owns it)
4. **RULE-CACHE-004:** Multiple objects with same ID are stored in a stack (LIFO)
5. **RULE-CACHE-005:** Destructor is called only during eviction or purge
