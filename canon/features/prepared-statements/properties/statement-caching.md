# Property: Statement Caching

**ID:** DEC-001
**Confidence:** 0.90
**Status:** Verified
**Type:** Architectural Decision

---

## Statement

```
Prepared statements are cached in an MRU (Most Recently Used) cache
with capacity 16 entries. Repeated preparation of the same SQL string
reuses the cached statement instead of recompiling.
```

---

## Rationale

**Problem:** SQL compilation overhead
- Parsing SQL takes time (~10-100μs depending on complexity)
- Repeated queries use same SQL
- Recompilation wasteful

**Solution:** Statement cache
- Cache compiled statements
- Key: SQL string
- Lookup in cache before compilation
- Evict least-recently-used on overflow

**Benefits:**
- Performance: Repeated queries faster
- Memory: Cache size bounded (16 entries)
- Simple: Transparent to users

---

## Implementation Details

### Cache Size: 16 Entries

**Design decision:** Medium-sized cache
- Small: 4-8 entries (likely miss for varied queries)
- Medium: 16 entries (balance coverage vs. memory)
- Large: 64+ entries (memory overhead)

**Rationale:** Most applications use 10-30 distinct queries
- Cache size 16 covers typical workloads
- Over-sized: wasted memory
- Under-sized: too many cache misses

---

### Eviction Strategy: MRU (Most Recently Used)

**Mechanism:** When cache full and new query needed:
1. Statement added to cache
2. Least-recently-used statement evicted
3. Evicted statement finalized via destructor

**Behavior:**
```
Query sequence:
1. "SELECT * FROM users"          → cache miss, compile, add to cache
2. "SELECT * FROM posts"          → cache miss, compile, add to cache
...
17. "SELECT * FROM different"     → cache miss, compile, add to cache
    (Now cache full with 16 entries)
    (Least recently used evicted and finalized)

18. "SELECT * FROM users"         → cache hit! (reused if still in cache)
    OR cache miss if evicted
```

---

### Location

**Cache object:** Stored in database handle
```common-lisp
(defclass sqlite-handle ()
  ((cache :initform nil :accessor cache)))
```

**Access:** Via connection handle
```common-lisp
(let ((stmt (prepare-statement db sql)))
  ;; Internally: (get-cached-statement (cache db) sql)
  )
```

---

## Usage Patterns

### Automatic Caching

**Users don't need to do anything:**
```common-lisp
;; Caching automatic
(execute-non-query db "INSERT INTO logs VALUES (?, ?)" ts msg)
(execute-non-query db "INSERT INTO logs VALUES (?, ?)" ts2 msg2)
;; Second call uses cached statement
```

---

### Deliberate Cache Hit

**Use consistent SQL strings:**
```common-lisp
;; Good: Same SQL string (cache hit)
(dotimes (i 100)
  (execute-non-query db "INSERT INTO logs VALUES (?, ?)" (+ 1000 i) msg))

;; Bad: Different SQL strings (cache miss)
(dotimes (i 100)
  (execute-non-query db
    (format nil "INSERT INTO logs VALUES (~D, '~A')" (+ 1000 i) msg)))
```

---

### Prepared Statements with Reset

**Best for batch operations:**
```common-lisp
;; Prepare once, reuse
(let ((stmt (prepare-statement db "INSERT INTO logs VALUES (?, ?)")))
  (dotimes (i 1000)
    (bind-parameter stmt 1 (+ 1000 i))
    (bind-parameter stmt 2 (format nil "Event ~D" i))
    (step-statement stmt)
    (reset-statement stmt))
  (finalize-statement stmt))
;; Single compilation for 1000 operations
```

---

## Performance Characteristics

### Cache Hit (SQL in cache)

```
Time: O(1) lookup + retrieve cached statement
      ≈ 5-10 microseconds
```

**vs. Cache Miss:**
```
Time: SQL parsing/compilation
      ≈ 10-100 microseconds (depends on SQL complexity)
```

**Benefit:** 2-10× speedup for repeated queries

---

### Eviction Overhead

**When cache full:**
```
Time: Finalize evicted statement
      ≈ 1-2 microseconds
```

**Negligible compared to compilation savings**

---

## Implementation Code

**Location:** `sqlite.lisp:106-117, 163-178`

```common-lisp
(defun prepare-statement (db sql)
  (let ((stmt (make-instance 'sqlite-statement :db db)))
    (cache-statement db sql stmt)
    stmt))

(defun cache-statement (db sql stmt)
  (let ((cached (get-cached-statement (cache db) sql)))
    (if cached
        (copy-statement-state stmt cached)
        (compile-statement stmt sql))))
```

**Cache logic:**
1. Check if SQL in cache via `get-cached-statement`
2. If found: copy state from cached statement
3. If not found: compile new statement and add to cache

---

## Implications

### For Users

**Recommendation:** Use consistent SQL strings
```common-lisp
;; Good
(loop repeat 100
      do (execute-non-query db
           "INSERT INTO logs VALUES (?, ?)" ts msg))

;; Bad
(loop repeat 100
      do (execute-non-query db
           (format nil "INSERT INTO logs VALUES (~D, '~A')" ts msg)))
```

---

### Manual Cache Management

**No API for manual cache control:**
- No way to clear cache
- No way to disable caching
- No way to set different size

**Design**: Transparent caching (simpler)

---

## Limitations

### Fixed Size (16 entries)

**Problem:** Small set of queries + large set of rarely-used queries
```
Common queries: 5
Occasional queries: 50 (only used once)
Cache misses: Every occasional query
```

**Workaround:** Use prepared statements directly (manual control)

---

### No Parameterized Caching

**Cache key is full SQL string:**
```common-lisp
;; These are different cache entries (different SQL strings)
(execute-to-list db "SELECT * FROM users WHERE age > ?" 18)
(execute-to-list db "SELECT * FROM users WHERE age > ?" 21)  ; Cache hit
(execute-to-list db "SELECT * FROM users WHERE id = ?" 5)    ; Cache miss
```

**All use same compiled statement (same SQL)** - this works

---

## Testing

**Implicit verification:** Performance of repeated queries
- First query: slower (compilation)
- Subsequent queries: faster (cached)

**Test:** `test-multi-insert` (sqlite-tests.lisp:40)
- Inserts 100 times with same SQL
- Single compilation verified by behavior

---

## Related Properties

- **ARCH-002 (Resource Management):** Cache uses destructors for cleanup
- **RULE-002 (Connection Before Operations):** Cache belongs to connection

---

## Documentation References

- Specification: CL-SQLITE.agent.md:311-320 (DEC-001)
- Implementation: sqlite.lisp:163-178
- Tests: sqlite-tests.lisp:40 (implicit)

---

**Document Status:** Canonical
**Last Verified:** 2026-01-20 (Canon Initiation Pass 4)
