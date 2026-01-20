# Contract: vec-normalize

**Signature:** `(vec-normalize db vector) => normalized-vector`
**Confidence:** 0.90
**Status:** Stable

---

## Parameters

- **db:** Connected database
- **vector:** Float vector

## Returns

L2-normalized vector (unit length)

## Behavior

Normalizes vector to unit length for cosine similarity.

## Example

```common-lisp
(vec-normalize db #(3.0 4.0))
;; => #(0.6 0.8)  (magnitude 1.0)
```

## Use Cases

- Cosine similarity (use with normalized vectors)
- Semantic search setup

## Related

- `vec-distance-cosine` - Use normalized vectors
- `vec-add` - Vector arithmetic

---

**Document Status:** Canonical
**Last Verified:** 2026-01-20 (Canon Initiation Pass 2)
