(defsystem :inquisitio
  :name "inquisitio"
  :author "Abhijit Rao <quasi@quasilabs.in>"
  :description "SQLite interface for Common Lisp — safe, ergonomic access to SQLite databases."
  :version "1.0.0"
  :license "MIT"

  :depends-on (:iterate :cffi :telos)

  :components ((:file "features"
                :description "Telos feature hierarchy for intent tracking")
               (:file "ffi"
                :description "Low-level CFFI bindings to the SQLite3 C API")
               (:file "cache"
                :description "MRU cache for prepared statement reuse")
               (:file "core"
                :description "Core database operations: connect, prepare, execute, iterate"
                :depends-on ("ffi" "cache"))
               (:file "simplified"
                :description "S-expression-based CRUD interface"
                :depends-on ("core"))
               (:file "vec"
                :description "Vector similarity search via sqlite-vec extension"
                :depends-on ("core" "simplified")))

  :in-order-to ((test-op (load-op inquisitio-tests))))

(defmethod perform ((o asdf:test-op) (c (eql (find-system :inquisitio))))
  (funcall (intern "RUN-ALL-SQLITE-TESTS" :inquisitio-tests)))
