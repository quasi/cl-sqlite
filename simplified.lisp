(in-package :inquisitio)

;;; Name normalization

(telos:defun/i normalize-type (type)
  (:feature inquisitio-simplified)
  (:purpose "Convert a Lisp type keyword to its SQL string representation")
  (string-upcase (string type)))

(telos:defun/i normalize-name (name)
  "Converts NAME to a safe SQL identifier. Keywords and symbols have hyphens
converted to underscores. Signals an error if the result is not a valid identifier."
  (:feature inquisitio-simplified)
  (:purpose "Validate and convert Lisp names to safe SQL identifiers, preventing injection")
  (let* ((raw (string-downcase (string name)))
         (converted (substitute #\_ #\- raw)))
    (unless (and (plusp (length converted))
                 (every (lambda (c) (or (alpha-char-p c) (digit-char-p c) (char= c #\_))) converted)
                 (not (digit-char-p (char converted 0))))
      (sqlite-error nil "Invalid SQL identifier"))
    converted))

;;; Column definition

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

;;; Table operations

(telos:defun/i create-table (db name columns &key (if-not-exists nil))
  "Create a table with the given column definitions."
  (:feature inquisitio-simplified)
  (:purpose "Generate and execute CREATE TABLE DDL from s-expression column specs")
  (let ((sql (format nil "CREATE TABLE ~A~A (~{~A~^, ~})"
                     (if if-not-exists "IF NOT EXISTS " "")
                     (normalize-name name)
                     (mapcar #'build-column-def columns))))
    (execute-non-query db sql)))

(telos:defun/i drop-table (db name &key (if-exists nil))
  "Drop a table."
  (:feature inquisitio-simplified)
  (:purpose "Generate and execute DROP TABLE DDL")
  (let ((sql (format nil "DROP TABLE ~A~A"
                     (if if-exists "IF EXISTS " "")
                     (normalize-name name))))
    (execute-non-query db sql)))

;;; WHERE clause compilation

(telos:defun/i compile-where (where-clause)
  "Compiles a WHERE clause (s-expression) into a SQL string and a list of parameters."
  (:feature inquisitio-simplified)
  (:purpose "Transform s-expression WHERE clauses into parameterized SQL to prevent injection")
  (if (null where-clause)
      (values "" nil)
      (destructuring-bind (op &rest args) where-clause
        (case op
          (:and
           (loop for arg in args
                 for (sql params) = (multiple-value-list (compile-where arg))
                 collect sql into sqls
                 nconc params into all-params
                 finally (return (values (format nil "(~{~A~^ AND ~})" sqls) all-params))))
          (:or
           (loop for arg in args
                 for (sql params) = (multiple-value-list (compile-where arg))
                 collect sql into sqls
                 nconc params into all-params
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
          (t (sqlite-error nil (list "Unknown operator in where clause: ~A" op)))))))

;;; CRUD operations

(telos:defun/i insert (db table data)
  "Inserts a row into the table. DATA is a plist of column names and values."
  (:feature inquisitio-simplified)
  (:purpose "Generate and execute parameterized INSERT from a plist")
  (let* ((cols (loop for (k v) on data by #'cddr collect (normalize-name k)))
         (vals (loop for (k v) on data by #'cddr collect v))
         (placeholders (make-list (length cols) :initial-element "?"))
         (sql (format nil "INSERT INTO ~A (~{~A~^, ~}) VALUES (~{~A~^, ~})"
                      (normalize-name table) cols placeholders)))
    (apply #'execute-non-query db sql vals)))

(defun validate-order-direction (dir)
  "Validates that DIR is :asc or :desc. Returns the SQL string."
  (let ((s (string-upcase (string dir))))
    (unless (member s '("ASC" "DESC") :test #'string=)
      (sqlite-error nil (list "Invalid ORDER BY direction: ~S (must be :asc or :desc)" dir)))
    s))

(telos:defun/i select (db table &key (columns '(*)) where order-by limit offset)
  "Selects rows from the table.
COLUMNS: list of keywords/symbols, defaults to '(*)
WHERE: s-expression for where clause e.g. '(:= :id 1)
ORDER-BY: list of (:col :asc/:desc) or just :col
LIMIT: non-negative integer
OFFSET: non-negative integer"
  (:feature inquisitio-simplified)
  (:purpose "Generate and execute parameterized SELECT queries from keyword arguments")
  (when limit
    (unless (and (integerp limit) (>= limit 0))
      (sqlite-error nil (list "LIMIT must be a non-negative integer, got: ~S" limit))))
  (when offset
    (unless (and (integerp offset) (>= offset 0))
      (sqlite-error nil (list "OFFSET must be a non-negative integer, got: ~S" offset))))
  (multiple-value-bind (where-sql where-params) (compile-where where)
    (let* ((cols-sql (if (equal columns '(*))
                         "*"
                         (format nil "~{~A~^, ~}" (mapcar #'normalize-name columns))))
           (order-sql (if order-by
                          (flet ((format-one-order (x)
                                   (if (listp x)
                                       (format nil "~A ~A" (normalize-name (first x)) (validate-order-direction (second x)))
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

(telos:defun/i update-table (db table data &key where)
  "Updates rows in the table.
DATA: plist of columns to update and their new values.
WHERE: s-expression."
  (:feature inquisitio-simplified)
  (:purpose "Generate and execute parameterized UPDATE from a plist and where clause")
  (multiple-value-bind (where-sql where-params) (compile-where where)
    (let* ((cols (loop for (k v) on data by #'cddr collect (normalize-name k)))
           (vals (loop for (k v) on data by #'cddr collect v))
           (set-clause (format nil "~{~A = ?~^, ~}" cols))
           (sql (format nil "UPDATE ~A SET ~A~A"
                        (normalize-name table)
                        set-clause
                        (if (string= where-sql "") "" (format nil " WHERE ~A" where-sql)))))
      (apply #'execute-non-query db sql (append vals where-params)))))

(telos:defun/i delete-from (db table &key where)
  "Deletes rows from the table."
  (:feature inquisitio-simplified)
  (:purpose "Generate and execute parameterized DELETE with optional where clause")
  (multiple-value-bind (where-sql where-params) (compile-where where)
    (let ((sql (format nil "DELETE FROM ~A~A"
                       (normalize-name table)
                       (if (string= where-sql "") "" (format nil " WHERE ~A" where-sql)))))
      (apply #'execute-non-query db sql where-params))))
