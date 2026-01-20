# Contract: vec-distance-cosine

**Signature:** `(vec-distance-cosine db v1 v2) => distance`
**Confidence:** 0.90
**Status:** Stable

---

## Parameters

- **db:** Connected database
- **v1, v2:** Float vectors (should be normalized)

## Returns

Float (cosine distance, 0-2 range)

## Behavior

Computes cosine distance between vectors.

## Example

```common-lisp
(vec-distance-cosine db
  (vec-normalize db #(1.0 0.0 0.0))
  (vec-normalize db #(1.0 1.0 0.0)))
;; => ~0.293  (cosine distance for 45° angle)
```

## Note

Best used with normalized vectors for semantic similarity.

## Related

- `vec-normalize` - Use before distance computation
- `vec-distance-L2` - Euclidean distance

---

**Document Status:** Canonical
**Last Verified:** 2026-01-20 (Canon Initiation Pass 2)
