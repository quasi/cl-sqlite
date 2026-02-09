(in-package :cl-user)

(telos:deffeature inquisitio
  :purpose "SQLite interface for Common Lisp — provides safe, ergonomic access to SQLite databases"
  :goals ((:g1 "Provide complete SQLite API coverage through CFFI bindings")
          (:g2 "Support both low-level prepared statements and high-level query functions")
          (:g3 "Offer a simplified s-expression-based CRUD interface")
          (:g4 "Enable vector similarity search via sqlite-vec extension"))
  :constraints ((:c1 "Must maintain backward compatibility with :sqlite package nickname")
                (:c2 "Must not leak SQLite resources on error paths"))
  :assumptions ((:a1 "SQLite3 shared library is available on the system")
                (:a2 "CFFI can locate and load the SQLite3 library")))

(telos:deffeature inquisitio-ffi
  :belongs-to inquisitio
  :purpose "Low-level CFFI bindings to the SQLite3 C API"
  :goals ((:g1 "Expose all necessary SQLite3 C functions to Lisp"))
  :constraints ((:c1 "Bindings must match SQLite3 C API signatures exactly")))

(telos:deffeature inquisitio-cache
  :belongs-to inquisitio
  :purpose "MRU cache for prepared statements to avoid repeated compilation"
  :goals ((:g1 "Reduce overhead of repeated query preparation"))
  :constraints ((:c1 "Must properly finalize statements when evicting from cache")))

(telos:deffeature inquisitio-conditions
  :belongs-to inquisitio
  :purpose "Condition hierarchy for SQLite errors with structured context"
  :goals ((:g1 "Provide actionable error information including SQL, error code, and database path"))
  :constraints ((:c1 "Must distinguish constraint violations from other errors")))

(telos:deffeature inquisitio-core
  :belongs-to inquisitio
  :purpose "Core database operations: connect, prepare, execute, iterate"
  :goals ((:g1 "Provide safe connection lifecycle management")
          (:g2 "Support positional and named parameter binding")
          (:g3 "Offer multiple result-shape functions (list, single, multi-value)"))
  :constraints ((:c1 "Must finalize statements even on error paths")
                (:c2 "Must support iterate macro drivers for query iteration")))

(telos:deffeature inquisitio-simplified
  :belongs-to inquisitio
  :purpose "S-expression-based CRUD interface for common table operations"
  :goals ((:g1 "Enable table operations without writing raw SQL")
          (:g2 "Compile s-expression WHERE clauses to parameterized SQL"))
  :constraints ((:c1 "Must validate all identifiers to prevent SQL injection")
                (:c2 "Must use parameterized queries for all user values")))

(telos:deffeature inquisitio-vec
  :belongs-to inquisitio
  :purpose "Vector similarity search via the sqlite-vec extension"
  :goals ((:g1 "Provide Lisp-friendly wrappers for sqlite-vec functions"))
  :assumptions ((:a1 "The vec0 extension shared library is available for loading")))
