;;; Backward-compatibility shim — loads inquisitio-tests when someone asks for :sqlite-tests
(defsystem :sqlite-tests
  :description "Compatibility shim: loads inquisitio-tests (formerly sqlite-tests)"
  :depends-on (:inquisitio-tests))
