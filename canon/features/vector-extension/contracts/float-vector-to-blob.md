# Contract: float-vector-to-blob

**Signature:** `(float-vector-to-blob vector) => blob`
**Confidence:** 0.90
**Status:** Stable

---

## Parameters

- **vector:** `(simple-array single-float (*))`

## Returns

Byte vector `(simple-array (unsigned-byte 8) (*))`

## Behavior

Converts Lisp float vector to SQLite blob (IEEE 754 32-bit floats, little-endian).

## Example

```common-lisp
(let ((vec #(1.0 2.5 3.14)))
  (float-vector-to-blob vec))
;; => Binary blob (12 bytes: 3 floats × 4 bytes)
```

## Format

- Each float: 32-bit IEEE 754
- Encoding: little-endian
- Total size: `(length vector) × 4` bytes

## Related

- `blob-to-float-vector` - Reverse conversion
- `vector-search` - Uses automatically

---

**Document Status:** Canonical
**Last Verified:** 2026-01-20 (Canon Initiation Pass 2)
