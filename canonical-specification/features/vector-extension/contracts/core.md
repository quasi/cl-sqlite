---
type: contract
name: vector-extension-core
version: 1.0.0
feature: vector-extension
---

# Vector Extension Contract

## Purpose
Provides integration with sqlite-vec extension for vector similarity search and vector operations. Includes table creation, k-nearest-neighbor search, and scalar vector functions.

## Table Operations

#### `create-vector-table`
Creates a vec0 virtual table for vector storage and search.

**Parameters:**
```json
{
  "type": "object",
  "properties": {
    "db": {
      "type": "object",
      "description": "sqlite-handle"
    },
    "name": {
      "type": "symbol",
      "description": "Table name"
    },
    "columns": {
      "type": "array",
      "description": "Vector column definitions",
      "items": {
        "oneOf": [
          {
            "type": "array",
            "description": "(name dimension) - defaults to float type",
            "minItems": 2,
            "maxItems": 2
          },
          {
            "type": "array",
            "description": "(name type dimension) - explicit type",
            "minItems": 3,
            "maxItems": 3
          }
        ]
      }
    },
    "if-not-exists": {
      "type": "boolean",
      "default": false
    }
  },
  "required": ["db", "name", "columns"]
}
```

**Column Types:**
- `:float` - 32-bit float vectors (default)
- `:bit` - Bit vectors
- `:int8` - 8-bit integer vectors

**Example:**
```lisp
(create-vector-table db 'embeddings
  '((embedding 1536)           ; float[1536]
    (features :bit 128)))      ; bit[128]
```

**Errors:**
- `sqlite-error` when vec0 extension is not loaded
- `sqlite-error` when dimension is invalid

## Vector Search

#### `vector-search`
Performs k-nearest-neighbor search on a vec0 table.

**Parameters:**
```json
{
  "type": "object",
  "properties": {
    "db": {
      "type": "object",
      "description": "sqlite-handle"
    },
    "table": {
      "type": "symbol",
      "description": "vec0 table name"
    },
    "query-vector": {
      "oneOf": [
        {"type": "array", "description": "Lisp vector (will be converted to blob)"},
        {"type": "string", "description": "Blob as string"},
        {"type": "object", "description": "Pre-converted blob"}
      ]
    },
    "k": {
      "type": "integer",
      "description": "Number of results to return",
      "default": 10
    },
    "column": {
      "type": "symbol",
      "description": "Vector column name to search",
      "default": "embedding"
    },
    "output-columns": {
      "type": "array",
      "description": "Columns to return in results",
      "default": ["rowid", "distance"]
    }
  },
  "required": ["db", "table", "query-vector"]
}
```

**Returns:** List of lists (rows), ordered by distance (ascending)

**Example:**
```lisp
(vector-search db 'embeddings #(0.1 0.2 0.3 ...)
               :k 5
               :column 'embedding
               :output-columns '(rowid distance text))
```

**Errors:**
- `sqlite-error` when table does not exist or is not a vec0 table
- `sqlite-error` when vector dimensions don't match

## Vector Operations

#### `vec-distance-L2`
Calculates L2 (Euclidean) distance between two vectors.

**Parameters:**
```json
{
  "type": "object",
  "properties": {
    "db": {"type": "object", "description": "sqlite-handle"},
    "vec1": {"description": "First vector (Lisp vector or blob)"},
    "vec2": {"description": "Second vector (Lisp vector or blob)"}
  },
  "required": ["db", "vec1", "vec2"]
}
```

**Returns:** Double-float (distance value)

**Errors:**
- `sqlite-error` when vectors have different dimensions

#### `vec-distance-cosine`
Calculates cosine distance between two vectors.

**Parameters:**
```json
{
  "type": "object",
  "properties": {
    "db": {"type": "object", "description": "sqlite-handle"},
    "vec1": {"description": "First vector (Lisp vector or blob)"},
    "vec2": {"description": "Second vector (Lisp vector or blob)"}
  },
  "required": ["db", "vec1", "vec2"]
}
```

**Returns:** Double-float (distance value, range [0, 2])

#### `vec-add`
Returns element-wise addition of two vectors.

**Parameters:**
```json
{
  "type": "object",
  "properties": {
    "db": {"type": "object", "description": "sqlite-handle"},
    "vec1": {"description": "First vector (Lisp vector or blob)"},
    "vec2": {"description": "Second vector (Lisp vector or blob)"}
  },
  "required": ["db", "vec1", "vec2"]
}
```

**Returns:** Simple-array of single-float (result vector)

**Errors:**
- `sqlite-error` when vectors have different dimensions

#### `vec-normalize`
Returns the normalized version of a vector.

**Parameters:**
```json
{
  "type": "object",
  "properties": {
    "db": {"type": "object", "description": "sqlite-handle"},
    "vec": {"description": "Vector to normalize (Lisp vector or blob)"}
  },
  "required": ["db", "vec"]
}
```

**Returns:** Simple-array of single-float (normalized vector)

#### `vec-slice`
Returns a slice of a vector.

**Parameters:**
```json
{
  "type": "object",
  "properties": {
    "db": {"type": "object", "description": "sqlite-handle"},
    "vec": {"description": "Source vector (Lisp vector or blob)"},
    "start": {"type": "integer", "description": "Start index (inclusive)"},
    "end": {"type": "integer", "description": "End index (exclusive)"}
  },
  "required": ["db", "vec", "start", "end"]
}
```

**Returns:** Simple-array of single-float (sliced vector)

**Errors:**
- `sqlite-error` when start/end indices are out of range

## Conversion Helpers

#### `float-vector-to-blob`
Converts a Lisp float vector to a byte vector (blob).

**Parameters:**
```json
{
  "type": "object",
  "properties": {
    "vector": {
      "type": "array",
      "description": "Simple-array of single-float",
      "element-type": "single-float"
    }
  },
  "required": ["vector"]
}
```

**Returns:** Simple-array of (unsigned-byte 8) - byte vector

#### `blob-to-float-vector`
Converts a byte vector (blob) to a Lisp float vector.

**Parameters:**
```json
{
  "type": "object",
  "properties": {
    "blob": {
      "type": "array",
      "description": "Byte vector",
      "element-type": "(unsigned-byte 8)"
    }
  },
  "required": ["blob"]
}
```

**Returns:** Simple-array of single-float

**Errors:**
- Error when blob length is not a multiple of 4

## Invariants

1. **RULE-VEC-001:** All vector operations require sqlite-vec extension to be loaded
2. **RULE-VEC-002:** Float vectors are serialized as little-endian 4-byte floats
3. **RULE-VEC-003:** Vector dimensions must match for binary operations
4. **RULE-VEC-004:** Blob length must be multiple of 4 for float vectors
5. **RULE-VEC-005:** k-NN search results are always ordered by distance ascending
