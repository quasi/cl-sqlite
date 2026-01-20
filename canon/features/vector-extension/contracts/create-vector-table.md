# Contract: create-vector-table

**Signature:** `(create-vector-table db table-name columns) => nil`
**Confidence:** 0.90
**Status:** Stable

---

## Parameters

- **db:** Connected database with sqlite-vec loaded
- **table-name:** Keyword/string
- **columns:** List of `(name dimension)` pairs

## Returns

`nil` on success

## Behavior

Creates vec0 virtual table for vector storage and search.

## Example

```common-lisp
;; Enable and load sqlite-vec
(enable-load-extension db t)
(load-extension db "/path/to/vec0.so" (cffi:null-pointer))

;; Create vector table
(create-vector-table db :embeddings
  '((embedding 1536)    ; OpenAI Ada-002 size
    (text_id 1)))       ; Can have other columns
```

## Dimension

Vector dimension must match embedding size (e.g., 1536 for OpenAI ada-002, 384 for smaller models).

## Related

- `enable-load-extension` - Required first
- `load-extension` - Required second
- `vector-search` - Query vectors

---

**Document Status:** Canonical
**Last Verified:** 2026-01-20 (Canon Initiation Pass 2)
