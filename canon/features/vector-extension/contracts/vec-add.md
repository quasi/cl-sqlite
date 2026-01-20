# Contract: vec-add

**Signature:** `(vec-add db v1 v2) => result-vector`
**Confidence:** 0.90
**Status:** Stable

---

## Parameters

- **db:** Connected database
- **v1, v2:** Float vectors (same dimension)

## Returns

Result vector (element-wise sum)

## Example

```common-lisp
(vec-add db #(1.0 2.0 3.0) #(4.0 5.0 6.0))
;; => #(5.0 7.0 9.0)
```

## Related

- `vec-normalize` - For normalization
- `vec-distance-L2` - Distance computation

---

**Document Status:** Canonical
**Last Verified:** 2026-01-20 (Canon Initiation Pass 2)
