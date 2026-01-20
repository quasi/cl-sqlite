# Contract: enable-load-extension

**Signature:** `(enable-load-extension db on/off) => nil`
**Confidence:** 0.90
**Status:** Stable

---

## Parameters

- **db:** Connected database handle
- **on/off:** `T` (enable) or `NIL` (disable)

## Returns

`nil`

## Behavior

Enables/disables extension loading for this connection. Must be enabled before `load-extension`.

## Example

```common-lisp
(enable-load-extension db t)
(load-extension db "/path/to/sqlite_vec.so" (cffi:null-pointer))
(enable-load-extension db nil)  ; Disable after loading
```

## Security

Enabling extension loading opens security risk (arbitrary code execution). Only enable if loading trusted extensions.

## Related

- `load-extension` - Must call after enable-load-extension
- `RULE-006` - Extension loading sequence

---

**Document Status:** Canonical
**Last Verified:** 2026-01-20 (Canon Initiation Pass 2)
