;;; Backward-compatibility shim — loads inquisitio when someone asks for :sqlite
(defsystem :sqlite
  :description "Compatibility shim: loads inquisitio (formerly cl-sqlite)"
  :depends-on (:inquisitio))
