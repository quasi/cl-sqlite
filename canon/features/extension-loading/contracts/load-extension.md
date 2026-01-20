# Contract: load-extension

**Signature:** `(load-extension db path entry-point) => nil`
**Confidence:** 0.90
**Status:** Stable

---

## Parameters

- **db:** Connected database (with extension loading enabled)
- **path:** String path to shared library (.so, .dylib, .dll)
- **entry-point:** CFFI null pointer (or extension-specific entry point)

## Returns

`nil` on success

## Behavior

Loads shared library as SQLite extension.

## Example

```common-lisp
(enable-load-extension db t)
(load-extension db "/usr/local/lib/vec0.so" (cffi:null-pointer))
;; sqlite-vec loaded, can now use vector functions
```

## Precondition

- `enable-load-extension` must be called first
- Extension library must exist at path

## Errors

- `sqlite-error` if extension not found or load fails

## Related

- `enable-load-extension` - Must call first
- `RULE-006` - Extension loading sequence

---

**Document Status:** Canonical
**Last Verified:** 2026-01-20 (Canon Initiation Pass 2)
