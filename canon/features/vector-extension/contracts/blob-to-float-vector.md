# Contract: blob-to-float-vector

**Signature:** `(blob-to-float-vector blob) => vector`
**Confidence:** 0.90
**Status:** Stable

---

## Parameters

- **blob:** Byte vector from SQLite

## Returns

`(simple-array single-float (*))`

## Behavior

Converts SQLite blob to Lisp float vector. Reverse of `float-vector-to-blob`.

## Constraints

- Blob length must be multiple of 4
- Error if not valid encoding

## Example

```common-lisp
(let ((blob (get-blob-from-db)))
  (blob-to-float-vector blob))
;; => #(1.0 2.5 3.14)
```

## Related

- `float-vector-to-blob` - Forward conversion
- `statement-column-value` - Returns blob from query

---

**Document Status:** Canonical
**Last Verified:** 2026-01-20 (Canon Initiation Pass 2)
