(defsystem :inquisitio-tests
  :name "inquisitio-tests"
  :author "Abhijit Rao <quasi@quasilabs.in>"
  :description "Tests for Inquisitio, a SQLite interface for Common Lisp."
  :version "1.0.0"
  :license "MIT"

  :depends-on (:fiveam :inquisitio :bordeaux-threads)

  :components ((:module "tests"
                :components ((:file "core-tests")
                             (:file "transaction-tests")
                             (:file "simplified-tests")
                             (:file "vec-tests")))))
