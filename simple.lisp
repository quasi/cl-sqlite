(in-package :sqlite)

(export '(create-table drop-table insert select update-table delete-from normalize-name))

(defun normalize-type (type)
  (string-upcase (string type)))

(defun normalize-name (name)
  (string-downcase (string name)))

(defun build-column-def (col-def)
  (destructuring-bind (name type &rest options) col-def
    (with-output-to-string (s)
      (format s "~A ~A" (normalize-name name) (normalize-type type))
      (loop for opt in options do
           (case opt
             (:primary-key (format s " PRIMARY KEY"))
             (:autoincrement (format s " AUTOINCREMENT"))
             (:not-null (format s " NOT NULL"))
             (:unique (format s " UNIQUE"))
             (t (format s " ~A" opt)))))))

(defun create-table (db name columns &key (if-not-exists nil))
  (let ((sql (format nil "CREATE TABLE ~@[IF NOT EXISTS ~]~A (~{~A~^, ~})"
                     if-not-exists
                     (normalize-name name)
                     (mapcar #'build-column-def columns))))
    (execute-non-query db sql)))

(defun drop-table (db name &key (if-exists nil))
  (let ((sql (format nil "DROP TABLE ~@[IF EXISTS ~]~A"
                     if-exists
                     (normalize-name name))))
    (execute-non-query db sql)))

;;; SQL Generation Helpers

(defun compile-where (where-clause)
  "Compiles a WHERE clause (s-expression) into a SQL string and a list of parameters."
  (if (null where-clause)
      (values "" nil)
      (destructuring-bind (op &rest args) where-clause
        (case op
          (:and
           (loop for arg in args
                 for (sql params) = (multiple-value-list (compile-where arg))
                 collect sql into sqls
                 append params into all-params
                 finally (return (values (format nil "(~{~A~^ AND ~})" sqls) all-params))))
          (:or
           (loop for arg in args
                 for (sql params) = (multiple-value-list (compile-where arg))
                 collect sql into sqls
                 append params into all-params
                 finally (return (values (format nil "(~{~A~^ OR ~})" sqls) all-params))))
          (:not
           (multiple-value-bind (sql params) (compile-where (first args))
             (values (format nil "NOT (~A)" sql) params)))
          ((:= :< :> :<= :>= :<> :like)
           (let ((col (first args))
                 (val (second args)))
             (values (format nil "~A ~A ?" (normalize-name col) (case op
                                                                  (:= "=")
                                                                  (:< "<")
                                                                  (:> ">")
                                                                  (:<= "<=")
                                                                  (:>= ">=")
                                                                  (:<> "<>")
                                                                  (:like "LIKE")))
                     (list val))))
          (:in
           (let ((col (first args))
                 (vals (rest args)))
             (values (format nil "~A IN (~{~A~^, ~})"
                             (normalize-name col)
                             (make-list (length vals) :initial-element "?"))
                     vals)))
          (:is-null
           (values (format nil "~A IS NULL" (normalize-name (first args))) nil))
          (:is-not-null
           (values (format nil "~A IS NOT NULL" (normalize-name (first args))) nil))
          (t (error "Unknown operator in where clause: ~A" op))))))

(defun insert (db table data)
  "Inserts a row into the table. DATA is a plist of column names and values."
  (let* ((cols (loop for (k v) on data by #'cddr collect (normalize-name k)))
         (vals (loop for (k v) on data by #'cddr collect v))
         (placeholders (make-list (length cols) :initial-element "?"))
         (sql (format nil "INSERT INTO ~A (~{~A~^, ~}) VALUES (~{~A~^, ~})"
                      (normalize-name table) cols placeholders)))
    (apply #'execute-non-query db sql vals)))

(defun select (db table &key (columns '(*)) where order-by limit offset)
  "Selects rows from the table.
   COLUMNS: list of keywords/symbols, defaults to '(*)
   WHERE: s-expression for where clause e.g. '(:= :id 1)
   ORDER-BY: list of (:col :asc/:desc) or just :col
   LIMIT: integer
   OFFSET: integer"
  (multiple-value-bind (where-sql where-params) (compile-where where)
    (let* ((cols-sql (if (equal columns '(*))
                         "*"
                         (format nil "~{~A~^, ~}" (mapcar #'normalize-name columns))))
           (order-sql (if order-by
                          (flet ((format-one-order (x)
                                   (if (listp x)
                                       (format nil "~A ~A" (normalize-name (first x)) (string-upcase (string (second x))))
                                       (normalize-name x))))
                            (format nil " ORDER BY ~{~A~^, ~}"
                                    (if (and (listp order-by) (not (keywordp (first order-by))))
                                        (mapcar #'format-one-order order-by)
                                        (list (format-one-order order-by)))))
                          ""))
           (limit-sql (if limit (format nil " LIMIT ~A" limit) ""))
           (offset-sql (if offset (format nil " OFFSET ~A" offset) ""))
           (sql (format nil "SELECT ~A FROM ~A~A~A~A~A"
                        cols-sql (normalize-name table)
                        (if (string= where-sql "") "" (format nil " WHERE ~A" where-sql))
                        order-sql limit-sql offset-sql)))
      (apply #'execute-to-list db sql where-params))))

(defun update-table (db table data &key where)
  "Updates rows in the table.
   DATA: plist of columns to update and their new values.
   WHERE: s-expression."
  (multiple-value-bind (where-sql where-params) (compile-where where)
    (let* ((cols (loop for (k v) on data by #'cddr collect (normalize-name k)))
           (vals (loop for (k v) on data by #'cddr collect v))
           (set-clause (format nil "~{~A = ?~^, ~}" cols))
           (sql (format nil "UPDATE ~A SET ~A~A"
                        (normalize-name table)
                        set-clause
                        (if (string= where-sql "") "" (format nil " WHERE ~A" where-sql)))))
      (apply #'execute-non-query db sql (append vals where-params)))))

(defun delete-from (db table &key where)
  "Deletes rows from the table."
  (multiple-value-bind (where-sql where-params) (compile-where where)
    (let ((sql (format nil "DELETE FROM ~A~A"
                       (normalize-name table)
                       (if (string= where-sql "") "" (format nil " WHERE ~A" where-sql)))))
      (apply #'execute-non-query db sql where-params))))
