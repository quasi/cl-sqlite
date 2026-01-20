# Contract: vec-distance-L2

**Signature:** `(vec-distance-L2 db v1 v2) => distance`
**Confidence:** 0.90
**Status:** Stable

---

## Parameters

- **db:** Connected database
- **v1, v2:** Float vectors or blobs

## Returns

Float (L2 Euclidean distance)

## Behavior

Computes L2 (Euclidean) distance between two vectors.

## Example

```common-lisp
(vec-distance-L2 db
  #(1.0 2.0 3.0)
  #(4.0 5.0 6.0))
;; => 5.196...  (sqrt((4-1)² + (5-2)² + (6-3)²))
```

## Related

- `vector-search` - Uses L2 by default
- `vec-distance-cosine` - Cosine distance
- `vec-normalize` - Normalize before cosine

---

**Document Status:** Canonical
**Last Verified:** 2026-01-20 (Canon Initiation Pass 2)
