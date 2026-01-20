# Contract: vector-search

**Signature:** `(vector-search db table query-vector &key k) => list-of-results`
**Confidence:** 0.90
**Status:** Stable

---

## Parameters

- **db:** Connected database
- **table:** Vector table name
- **query-vector:** Lisp vector `(simple-array single-float (*))`
- **k:** Number of results (default: 10)

## Returns

List of rowid + distance pairs

## Behavior

Performs K-nearest neighbor search on vector table.

## Example

```common-lisp
(let ((query (get-embedding "find similar documents")))
  (vector-search db :embeddings query :k 5))
;; => ((1 0.123) (5 0.145) (3 0.167) (8 0.189) (2 0.201))
;; rowid 1 is closest (distance 0.123)
```

## Supports

- L2 distance (default)
- Cosine distance (via query syntax)
- Hamming distance (via query syntax)

## Related

- `create-vector-table` - Create table first
- `vec-distance-L2` - Direct distance computation

---

**Document Status:** Canonical
**Last Verified:** 2026-01-20 (Canon Initiation Pass 2)
