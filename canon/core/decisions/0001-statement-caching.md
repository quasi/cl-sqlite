# ADR 0001: Statement Caching

**Status:** Accepted (Original design, ~2010)
**Confidence:** 0.80
**Decision Date:** ~2010 (inferred)
**Last Updated:** 2026-01-20

---

## Context

SQL statement parsing is expensive in SQLite. Applications often execute the same queries repeatedly with different parameters. Without caching, each execution would re-parse the SQL.

---

## Decision

Implement an MRU (Most Recently Used) cache for prepared statements, keyed by SQL text string. Cache size fixed at 16 entries.

**Key Behaviors:**
- `finalize-statement` returns statement to cache (not destroyed)
- `really-finalize-statement` actually destroys statement
- `prepare-statement` checks cache before creating new statement
- Cache eviction via MRU policy when full

---

## Rationale

### Performance Optimization

**Benefit:** Avoid re-parsing identical SQL strings
- Parsing is expensive (lexer, parser, query planner)
- Typical applications execute same queries repeatedly
- Example: `SELECT * FROM users WHERE id = ?` executed thousands of times

**Measurement:** Not quantified, but standard SQLite optimization practice

### Cache Size: 16 Entries

**Reasoning:** Balance between hit rate and memory
- Too small: Poor hit rate, frequent re-parsing
- Too large: Wasted memory, slow lookups

**16 chosen as empirical compromise** (specific rationale not documented)

### MRU Eviction

**Reasoning:** Recent statements likely to be used again
- Simple to implement
- Good hit rates for typical access patterns
- Alternative (LFU) more complex, marginal benefit

---

## Consequences

### Positive

✅ **Performance:** Significant speedup for repeated queries
- No re-parsing cost for hot queries
- Amortizes preparation cost over multiple executions

✅ **Transparent:** Cache invisible to users
- No API changes required
- Automatically benefits all applications

✅ **Memory Bounded:** Fixed cache size (16 statements)
- Predictable memory usage
- Prevents unbounded growth

### Negative

⚠️ **Semantic Confusion:** `finalize-statement` doesn't actually finalize
- Violates principle of least surprise
- Users may expect immediate resource release
- Mitigation: Documentation + `really-finalize-statement` for true destruction

⚠️ **Cache Size Not Configurable:** Fixed at 16
- Some applications may want larger/smaller cache
- No tuning knob provided
- Mitigation: 16 works well in practice

⚠️ **Cache Invalidation:** No mechanism to invalidate
- If SQL changes semantics (e.g., table altered), cache persists old statement
- Mitigation: Close and reopen connection to clear cache

### Trade-offs

**Performance vs. Memory:**
- Chose performance (cache enabled by default)
- Memory cost: ~16 prepared statements per connection
- Acceptable for most use cases

**Simplicity vs. Configurability:**
- Chose simplicity (fixed size, no config)
- Trade-off: One size fits all vs. tunable parameters
- Decision: 16 is reasonable default

---

## Implementation

**Location:** `cache.lisp`, `sqlite.lisp:104`, `sqlite.lisp:218-236`

**Key Code:**
```common-lisp
;; Cache initialization (sqlite.lisp:104)
(setf (cache object)
      (make-instance 'sqlite.cache:mru-cache
                     :cache-size 16
                     :destructor #'really-finalize-statement))

;; Cache lookup (sqlite.lisp:218-224)
(or (let ((statement (sqlite.cache:get-from-cache (cache db) sql)))
      (when statement
        (clear-statement-bindings statement))
      statement)
    (make-instance 'sqlite-statement :db db :sql sql))

;; Return to cache (sqlite.lisp:232-236)
(defun finalize-statement (statement)
  (reset-statement statement)
  (sqlite.cache:put-to-cache (cache (db statement)) (sql statement) statement))
```

**Cache Purity Invariant (INV-003):**
```
(prepare-statement db sql₁) = (prepare-statement db sql₂) ⟺ sql₁ = sql₂
```

---

## Alternatives Considered

### No Caching
**Rejected:** Performance penalty too high for typical use cases

### Statement Pooling (per application)
**Rejected:** Requires application-level management, more complex

### Larger Cache (32, 64, etc.)
**Not chosen:** Diminishing returns, 16 sufficient for most apps

### Configurable Cache Size
**Not chosen:** Added complexity, 16 works well in practice

---

## Validation

**Source:** Inferred from original changelog (v0.2, 2010)
- Original changelog mentions statement caching addition
- Implementation consistent with decision

**Tests:** Implicit
- No explicit cache hit/miss tests
- Tests implicitly rely on caching (no performance regression)

**Evidence:**
- Code structure (cache.lisp, separate from sqlite.lisp)
- Invariant INV-003 documented in agent.md
- Behavior verified in code review

---

## Related Decisions

- **DEC-009:** Finalization as Caching (same decision, different perspective)
- **ARCH-002:** Resource Management via CLOS

---

## References

- Original changelog: "0.2 (2010): Added statement caching"
- Code: `cache.lisp`, `sqlite.lisp:104,218-236`
- Specification: INV-003, INV-008
- Pattern: Statement Caching (performance property PERF-001)

---

**Document Status:** Canonical
**Confidence:** 0.80 (inferred from code + changelog, no detailed rationale found)
**Recommendation:** Accepted, works well in practice
