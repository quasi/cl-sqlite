# ADR 0003: Vector Search Extension Support

**Status:** Accepted
**Confidence:** 1.0
**Decision Date:** 2025-12-30
**Decided By:** quasi (google-labs-jules bot)
**Last Updated:** 2026-01-20

---

## Context

Modern applications increasingly use vector embeddings for:
- Semantic search
- Similarity matching
- Recommendation systems
- RAG (Retrieval-Augmented Generation) systems

**Problem:** SQLite doesn't natively support vector operations

**Solution:** [sqlite-vec extension](https://github.com/asg017/sqlite-vec) by Alex Garcia
- Adds virtual tables for vector storage
- K-nearest neighbor search
- Vector distance functions (L2, cosine, hamming)
- Vector arithmetic (add, subtract, normalize)

**Challenge:** cl-sqlite had no extension loading mechanism

---

## Decision

Add first-class support for SQLite extensions, with specific integration for sqlite-vec.

**Two-Phase Implementation:**

**Phase 1:** Extension Loading Infrastructure (PR #7)
- `enable-load-extension` - Enable extension loading (disabled by default for security)
- `load-extension` - Load shared library
- `float-vector-to-blob` - Convert Lisp vectors to SQLite blobs
- `blob-to-float-vector` - Convert SQLite blobs to Lisp vectors
- FFI bindings for extension loading

**Phase 2:** High-Level Vector API (PR #9)
- `create-vector-table` - Create vec0 virtual tables
- `vector-search` - K-nearest neighbor search
- `vec-add`, `vec-sub` - Vector arithmetic
- `vec-distance-L2`, `vec-distance-cosine` - Distance metrics
- `vec-normalize` - Normalize vectors

---

## Rationale

### 1. Enable AI/ML Use Cases

**Motivation:** Vector embeddings are core to modern AI applications

**Use Cases:**
- Store document/image embeddings
- Semantic search over text
- RAG systems (find relevant context for LLM prompts)
- Recommendation engines (find similar items)

**Example:**
```common-lisp
;; Store embeddings
(create-vector-table db :documents '((embedding 1536)))  ; OpenAI ada-002 size
(insert db :documents `(:rowid 1 :embedding ,(get-embedding "Some text")))

;; Semantic search
(vector-search db :documents query-embedding :k 5)
;; => Top 5 most similar documents
```

### 2. Keep Data In SQLite

**Benefits of in-database vectors:**
- No separate vector database needed (Pinecone, Weaviate, etc.)
- Simpler deployment (single file)
- Atomic transactions (vectors + metadata)
- Familiar SQL query interface

**Alternative:** External vector database
- **Rejected:** Adds complexity, separate deployment, no transactions across systems

### 3. Lisp-Native Vector Types

**Decision:** Support Lisp `(simple-array single-float (*))` natively
- Automatic conversion to/from SQLite blobs
- IEEE 754 32-bit encoding (4 bytes per float)
- Little-endian (platform-dependent via CFFI)

**Rationale:**
- Lisp vectors are natural representation
- Automatic conversion hides encoding details
- Users don't need to manually pack bytes

**Implementation:**
```common-lisp
(defun float-vector-to-blob (vector)
  (declare (type (simple-array single-float (*)) vector))
  (let* ((len (length vector))
         (blob (make-array (* len 4) :element-type '(unsigned-byte 8))))
    (cffi:with-pointer-to-vector-data (ptr blob)
      (loop for i from 0 below len
            do (setf (cffi:mem-aref ptr :float i) (aref vector i))))
    blob))
```

---

## Design Details

### Extension Loading Sequence (RULE-006)

**Required Order:**
1. `enable-load-extension` (security gate)
2. `load-extension` (load shared library)
3. Use extension functions

**Rationale:** SQLite disables extension loading by default for security
- Prevent loading arbitrary shared libraries
- Explicit opt-in required

**Enforcement:** SQLite returns "not authorized" if not enabled

### Vector Encoding

**Format:** 32-bit IEEE 754 little-endian floats
- 4 bytes per dimension
- Total size: `dimension × 4` bytes

**Constraint:** Blob length must be multiple of 4
- Enforced in `blob-to-float-vector`
- Error signaled if violated

**Rationale:** Matches sqlite-vec's expected format

### High-Level Wrappers

**Design:** Hide SQL details behind Lisp functions

**Example:**
```common-lisp
;; Low-level (SQL)
(execute-non-query db
  "CREATE VIRTUAL TABLE items USING vec0(embedding float[4])")
(execute-to-list db
  "SELECT rowid, distance FROM items WHERE embedding MATCH ? AND k = ?"
  query-vec 5)

;; High-level (wrapper)
(create-vector-table db :items '((embedding 4)))
(vector-search db :items query-vec :k 5)
```

**Benefit:** Consistent with simplified API philosophy

---

## Consequences

### Positive

✅ **Enable Modern Use Cases:** Vector search in SQLite
- No external vector database needed
- Single-file deployment

✅ **Lisp-Native:** Automatic vector ↔ blob conversion
- Users work with Lisp arrays
- Encoding hidden

✅ **Extensibility:** Infrastructure supports other extensions
- Not limited to sqlite-vec
- `enable-load-extension` + `load-extension` generic

✅ **Performance:** sqlite-vec is optimized
- Specialized vector indexes
- Fast K-NN search

### Negative

⚠️ **External Dependency:** Requires sqlite-vec shared library
- Must be installed separately
- Platform-specific (.so, .dylib, .dll)
- Mitigation: Document installation, provide examples

⚠️ **Platform-Dependent Encoding:** Little-endian assumption
- CFFI handles platform differences
- Potential issues on big-endian systems (rare)
- Mitigation: CFFI abstracts most differences

⚠️ **Security:** Extension loading enables arbitrary code
- `enable-load-extension` opens attack vector
- Mitigation: Explicit opt-in, document security implications

### Trade-offs

**Convenience vs. Security:**
- Chose convenience (easy extension loading)
- Trade-off: Security risk of loading arbitrary libraries
- Decision: Opt-in with `enable-load-extension`

**Lisp Vectors vs. Raw Blobs:**
- Chose Lisp vectors (automatic conversion)
- Trade-off: Conversion overhead vs. ergonomics
- Decision: Ergonomics more important (overhead negligible)

---

## Implementation

### Phase 1: Infrastructure (Commit 27b3b2b, 2025-12-30)

**PR:** #7 sqlite-vec-support

**Commit Message:**
```
feat: add sqlite-vec support

Added `enable-load-extension` and `load-extension` to support
loading SQLite extensions.
Added `float-vector-to-blob` and `blob-to-float-vector` helper
functions to convert between Common Lisp vectors and SQLite BLOBs
for vector search.
Verified functionality by loading the actual sqlite-vec extension
and performing vector operations in a test.
```

**Changes:**
- `sqlite-ffi.lisp`: FFI bindings for `sqlite3-enable-load-extension`, `sqlite3-load-extension`
- `sqlite.lisp`: High-level wrappers + vector conversion functions

### Phase 2: Syntactic Sugar (Commit 45ef406, 2026-01)

**PR:** #9 sqlite-vec-interface

**Commit Message:**
```
Add syntactic sugar for sqlite-vec
```

**Files Added:**
- `sqlite-vec.lisp`: High-level vector API
- `sqlite-vec-tests.lisp`: Comprehensive tests

**Test Coverage:**
- create-vector-table
- vector-search (string and Lisp vector)
- vec-add, vec-distance-L2, vec-normalize

---

## Alternatives Considered

### 1. External Vector Database
**Example:** Pinecone, Weaviate, Qdrant
**Rejected:** Adds deployment complexity, no transactions with SQLite data

### 2. Pure Lisp Vector Search
**Rejected:** Performance insufficient for large datasets

### 3. Different Vector Format (float64)
**Rejected:** sqlite-vec uses float32, consistency more important

### 4. Manual Blob Encoding (no conversion helpers)
**Rejected:** Poor ergonomics, error-prone

---

## Validation

**Git Evidence:**
- Commit 27b3b2b: Infrastructure
- Commit 45ef406: High-level API
- Both with clear rationale

**Code Evidence:**
- `sqlite.lisp:133-175`: Extension loading + conversion
- `sqlite-vec.lisp`: Full API implementation
- `sqlite-vec-tests.lisp`: 4 test scenarios

**Verification (from commit message):**
> "Verified functionality by loading the actual sqlite-vec extension
> and performing vector operations in a test."

**Test Evidence:**
- create-vector-table-test
- vector-search-test (string and vector inputs)
- scalar-functions-test (add, distance, normalize)

---

## Adoption

**Status:** Stable, production-ready (with sqlite-vec installed)
**Confidence:** 1.0
**Recommendation:** Use for vector similarity search in SQLite

**Prerequisites:**
1. Install sqlite-vec extension
2. Locate shared library path (.so, .dylib, .dll)
3. Enable and load in application

**Example:**
```common-lisp
(with-open-database (db ":memory:")
  ;; Enable and load
  (enable-load-extension db t)
  (load-extension db "/path/to/vec0.so" (cffi:null-pointer))

  ;; Create vector table
  (create-vector-table db :embeddings '((vector 384)))

  ;; Insert vectors
  (insert db :embeddings `(:rowid 1 :vector ,(my-embedding "text")))

  ;; Search
  (vector-search db :embeddings query-vec :k 10))
```

---

## Security Considerations

**Risk:** `load-extension` can load arbitrary shared libraries
- Could load malicious code
- Full process access

**Mitigation:**
1. **Explicit Opt-In:** `enable-load-extension` required
2. **Documentation:** Warn about security implications
3. **Validate Paths:** Only load trusted extension paths
4. **Principle of Least Privilege:** Don't enable in production unless needed

**Recommendation:**
```common-lisp
;; DON'T: Enable without validation
(enable-load-extension db t)
(load-extension db user-provided-path)  ; ❌ Security risk

;; DO: Validate path
(when (trusted-extension-path-p path)
  (enable-load-extension db t)
  (load-extension db path))
```

---

## Related Decisions

- **ARCH-001:** Layered API Architecture (vector API is extension layer)
- **DEC-002:** Simplified API (vector API follows same design)

---

## References

- Git commits: 27b3b2b, 45ef406
- PRs: #7, #9
- Code: `sqlite.lisp:133-175`, `sqlite-vec.lisp`
- Tests: `sqlite-vec-tests.lisp`
- Specification: RULE-006 (extension loading sequence)
- External: [sqlite-vec documentation](https://github.com/asg017/sqlite-vec)

---

**Document Status:** Canonical
**Confidence:** 1.0 (full rationale in git history)
**Recommendation:** Accepted, use for vector search
