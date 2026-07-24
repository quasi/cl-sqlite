(defsystem :inquisitio
  :name "inquisitio"
  :author "Abhijit Rao <quasi@quasilabs.in>"
  :description "SQLite interface for Common Lisp — safe, ergonomic access to SQLite databases."
  :version "1.0.0"
  :license "MIT"

  :depends-on (:iterate :cffi :telos :bordeaux-threads)

  :components ((:file "src/features"
                :description "Telos feature hierarchy for intent tracking")
               (:file "src/ffi"
                :description "Low-level CFFI bindings to the SQLite3 C API")
               (:file "src/cache"
                :description "MRU cache for prepared statement reuse")
               (:file "src/core"
                :description "Core database operations: connect, prepare, execute, iterate"
                :depends-on ("src/ffi" "src/cache"))
               (:file "src/simplified"
                :description "S-expression-based CRUD interface"
                :depends-on ("src/core"))
               (:file "src/vec"
                :description "Vector similarity search via sqlite-vec extension"
                :depends-on ("src/core" "src/simplified")))

  :in-order-to ((test-op (load-op inquisitio-tests))))

(defmethod perform ((o asdf:test-op) (c (eql (find-system :inquisitio))))
  (funcall (intern "RUN-ALL-SQLITE-TESTS" :inquisitio-tests)))
