;;;; Telos feature hierarchy for intent tracking.
;;;;
;;;; Loaded LAST, not first. Every DEFFEATURE name must be the same symbol the
;;;; DEFUN/I forms name in their (:FEATURE ...) option, and TELOS's feature
;;;; registry compares with EQ — so a feature declared in a package the members
;;;; do not live in is a different symbol and matches nothing. The members are
;;;; in :INQUISITIO and :INQUISITIO.CACHE, whose DEFPACKAGE forms are in
;;;; src/core.lisp and src/cache.lisp, so this file cannot be read before them.

(in-package :inquisitio)

(telos:deffeature inquisitio
  :purpose "SQLite interface for Common Lisp — provides safe, ergonomic access to SQLite databases"
  :goals ((:g1 "Provide complete SQLite API coverage through CFFI bindings")
          (:g2 "Support both low-level prepared statements and high-level query functions")
          (:g3 "Offer a simplified s-expression-based CRUD interface")
          (:g4 "Enable vector similarity search via sqlite-vec extension")
          (:g5 "Permit a connection handle to be shared between threads"))
  :constraints ((:c1 "Must maintain backward compatibility with :sqlite package nickname")
                (:c2 "Must not leak SQLite resources on error paths")
                (:c3 "Must guard every mutation of Lisp-side connection state with the per-handle recursive lock — callers are not required to synchronize")
                (:c4 "A SQLITE-STATEMENT object remains single-thread; only the handle is shareable"))
  :assumptions ((:a1 "SQLite3 shared library is available on the system")
                (:a2 "CFFI can locate and load the SQLite3 library")
                (:a3 "The linked SQLite3 is built with SQLITE_THREADSAFE 1 or 2, so SQLITE_OPEN_FULLMUTEX can select serialized mode per connection; SQLITE-THREADSAFE lets callers verify this rather than assume it"))
  :failure-modes ((:f1 "A path mutates Lisp-side handle state — statement list, cache, transaction depth — without holding HANDLE-LOCK. Two threads sharing the handle then corrupt that state while SQLite's own serialized mode reports no error, so the damage is invisible until a later close or query fails for an unrelated-looking reason."
                       :violates :g5)
                  (:f2 "A SQLITE-STATEMENT is handed to a second thread. Only the handle is shareable; a statement carries per-step cursor state, so concurrent stepping interleaves rows or steps a finalized statement."
                       :violates :g5)
                  (:f3 "The linked libsqlite3 was built SQLITE_THREADSAFE 0. SQLITE_OPEN_FULLMUTEX is then silently ignored rather than refused, and handle sharing corrupts SQLite's own state below the Lisp lock. SQLITE-THREADSAFE exists so this is checkable instead of assumed."
                       :violates :g5)))

(telos:deffeature inquisitio-ffi
  :belongs-to inquisitio
  :purpose "Low-level CFFI bindings to the SQLite3 C API"
  :goals ((:g1 "Expose all necessary SQLite3 C functions to Lisp"))
  :constraints ((:c1 "Bindings must match SQLite3 C API signatures exactly"))
  :failure-modes ((:f1 "A binding declares an argument or return type that does not match the C signature — :int for an sqlite3_int64, a missing pointer indirection. The call does not fail; it truncates a value or reads the wrong bytes, and the corruption surfaces far from the binding."
                       :violates :g1)
                  (:f2 "A function is used through a hand-rolled CFFI call at its use site instead of being added here. Coverage then looks complete while one call path escapes whatever this layer guarantees."
                       :violates :g1)))

(telos:deffeature inquisitio.cache::inquisitio-cache
  :belongs-to inquisitio
  :purpose "MRU cache for prepared statements to avoid repeated compilation"
  :goals ((:g1 "Reduce overhead of repeated query preparation"))
  :constraints ((:c1 "Must properly finalize statements when evicting from cache")
                (:c2 "Every cache operation must run under CACHE-LOCK, including the destructor call, so the owner can share one lock with the cache and avoid lock ordering"))
  :failure-modes ((:f1 "POP-FROM-CACHE drops an evicted statement without calling the destructor. The statement is unreachable from Lisp but still open in SQLite, so it never gets finalized and sqlite3_close later returns SQLITE_BUSY on a connection the caller believes is idle."
                       :violates :g1)
                  (:f2 "The destructor is called outside CACHE-LOCK. It finalizes a statement, which touches the owning handle's state and takes the handle lock — so a caller holding the handle lock and a caller holding the cache lock can each wait on the other. Sharing one lock is what removes the ordering problem; calling the destructor outside it puts the problem back."
                       :violates :g1)
                  (:f3 "PURGE-CACHE stops emptying its tables. A second purge then runs the destructor over statements already finalized, which is a double sqlite3_finalize on a freed pointer rather than a no-op."
                       :violates :g1)))

(telos:deffeature inquisitio-conditions
  :belongs-to inquisitio
  :purpose "Condition hierarchy for SQLite errors with structured context"
  :goals ((:g1 "Provide actionable error information including SQL, error code, and database path"))
  :constraints ((:c1 "Must distinguish constraint violations from other errors"))
  :failure-modes ((:f1 "An error path signals with the SQLite message alone and no SQL text, error code, or database path. The caller gets \"constraint failed\" with nothing identifying which statement against which file, which is exactly the case where a handler cannot recover."
                       :violates :g1)
                  (:f2 "A constraint violation is signalled as the generic SQLITE-ERROR. A handler that means to catch duplicate-key either catches every SQLite failure or none, and the distinction the hierarchy exists to make is unavailable at the only point it matters."
                       :violates :g1)))

(telos:deffeature inquisitio-core
  :belongs-to inquisitio
  :purpose "Core database operations: connect, prepare, execute, iterate"
  :goals ((:g1 "Provide safe connection lifecycle management")
          (:g2 "Support positional and named parameter binding")
          (:g3 "Offer multiple result-shape functions (list, single, multi-value)"))
  :constraints ((:c1 "Must finalize statements even on error paths")
                (:c2 "Must support iterate macro drivers for query iteration")
                (:c3 "WITH-TRANSACTION must hold the handle lock for the whole body — BEGIN/COMMIT/ROLLBACK are connection-global, so per-call locking is not sufficient")
                (:c4 "Nested WITH-TRANSACTION must use SAVEPOINT/RELEASE rather than a second BEGIN"))
  :failure-modes ((:f1 "A query path exits non-locally — an error signalled mid-step, a throw out of an iterate body — without an unwind-protect finalizing the statement. Each leak is silent; the symptom is a much later DISCONNECT that cannot close the handle."
                       :violates :g1)
                  (:f2 "WITH-TRANSACTION takes the handle lock per statement rather than across the whole body. BEGIN/COMMIT are connection-global, so a second thread's BEGIN lands inside the first thread's transaction and one COMMIT commits both threads' work — including work whose transaction later rolls back."
                       :violates :g1)
                  (:f3 "A nested WITH-TRANSACTION issues a second BEGIN instead of a SAVEPOINT. SQLite rejects it, and the recovery path most callers write — rolling back — discards the outer transaction's work too."
                       :violates :g1)
                  (:f4 "ROLLBACK TO is issued for a nested transaction without the matching RELEASE. The savepoint stays on the stack, so depth accounting and the next release refer to a savepoint that is not where the code believes it is."
                       :violates :g1)
                  (:f5 "A named parameter in the SQL has no corresponding key in the argument plist and binding treats the absence as NULL rather than an error. The query runs and returns a plausible wrong result — a WHERE that matches nothing, an INSERT that stores NULL — with no signal at all."
                       :violates :g2)))

(telos:deffeature inquisitio-simplified
  :belongs-to inquisitio
  :purpose "S-expression-based CRUD interface for common table operations"
  :goals ((:g1 "Enable table operations without writing raw SQL")
          (:g2 "Compile s-expression WHERE clauses to parameterized SQL"))
  :constraints ((:c1 "Must validate all identifiers to prevent SQL injection")
                (:c2 "Must use parameterized queries for all user values"))
  :failure-modes ((:f1 "A new code path formats a table or column name into SQL without NORMALIZE-NAME. Identifiers cannot be parameterized, so NORMALIZE-NAME's character whitelist is the only barrier — bypassing it at one site is injection through an interface whose whole claim is that callers never write SQL."
                       :violates :g1)
                  (:f2 "COMPILE-WHERE formats a value into the SQL text instead of emitting a placeholder and collecting the value. This is injection wherever the value came from a caller, and it defeats statement caching even where it does not, since every distinct value compiles a new statement."
                       :violates :g2)
                  (:f3 "COMPILE-WHERE meets an operator it does not recognize and passes it through as literal SQL rather than signalling. An unsupported clause then reads as a supported one, and the failure is not the error the caller would notice but a query with different semantics."
                       :violates :g2)))

(telos:deffeature inquisitio-vec
  :belongs-to inquisitio
  :purpose "Vector similarity search via the sqlite-vec extension"
  :goals ((:g1 "Provide Lisp-friendly wrappers for sqlite-vec functions"))
  :assumptions ((:a1 "The vec0 extension shared library is available for loading; it is not bundled, and libs/ is gitignored")
                (:a2 "The linked libsqlite3 exposes sqlite3_enable_load_extension — Apple's system build does not, which is why the darwin library search prefers Homebrew's"))
  :failure-modes ((:f1 "The linked libsqlite3 was built with SQLITE_OMIT_LOAD_EXTENSION, as Apple's system build is. Nothing about connecting or querying changes; only the first LOAD-EXTENSION fails, and it fails at runtime in whatever program depends on vector search rather than when the library was chosen."
                       :violates :g1)
                  (:f2 "The vec0 shared library is absent — it is not bundled and libs/ is gitignored — and the vec tests treat that as a skip. The suite then reports green on a machine where no vector code has been exercised at all, which is the report a broken wrapper produces too."
                       :violates :g1)
                  (:f3 "A query vector's length does not match the dimension the vec0 column was declared with. SQLite reports it as an opaque extension error naming neither the expected dimension nor the given one, so the caller debugs the wrapper instead of the vector."
                       :violates :g1)
                  (:f4 "A wrapper interpolates its vector argument into the SQL text rather than binding it. Large embeddings then cost a fresh compile per call, and any text-shaped input is injection into a path whose arguments look safely numeric."
                       :violates :g1)))
