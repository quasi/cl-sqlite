(defpackage :inquisitio
  (:nicknames :sqlite)
  (:use :cl :iter)
  ;; Conditions
  (:export :sqlite-error
           :sqlite-constraint-error
           :sqlite-error-db-handle
           :sqlite-error-code
           :sqlite-error-message
           :sqlite-error-sql)
  ;; Connection management
  (:export :sqlite-handle
           :handle
           :connect
           :set-busy-timeout
           :disconnect
           :with-open-database)
  ;; Statement lifecycle
  (:export :sqlite-statement
           :prepare-statement
           :with-prepared-statement
           :finalize-statement
           :step-statement
           :reset-statement
           :clear-statement-bindings)
  ;; Column and parameter access
  (:export :statement-column-value
           :statement-column-names
           :statement-bind-parameter-names
           :bind-parameter)
  ;; Query execution
  (:export :execute-non-query
           :execute-to-list
           :execute-single
           :execute-single/named
           :execute-one-row-m-v/named
           :execute-to-list/named
           :execute-non-query/named
           :execute-one-row-m-v
           :last-insert-rowid)
  ;; Transaction support
  (:export :with-transaction)
  ;; Extension loading
  (:export :enable-load-extension
           :load-extension)
  ;; Vector helpers
  (:export :float-vector-to-blob
           :blob-to-float-vector)
  ;; Simplified interface
  (:export :create-table
           :drop-table
           :insert
           :select
           :update-table
           :delete-from
           :normalize-name))

(in-package :inquisitio)

;;; Conditions

(telos:define-condition/i sqlite-error (simple-error)
  ((handle     :initform nil :initarg :db-handle
               :reader sqlite-error-db-handle)
   (error-code :initform nil :initarg :error-code
               :reader sqlite-error-code)
   (error-msg  :initform nil :initarg :error-msg
               :reader sqlite-error-message)
   (statement  :initform nil :initarg :statement
               :reader sqlite-error-statement)
   (sql        :initform nil :initarg :sql
               :reader sqlite-error-sql))
  (:feature inquisitio-conditions)
  (:purpose "Primary condition for all SQLite errors, carrying database handle, error code, message, and SQL context"))

(telos:define-condition/i sqlite-constraint-error (sqlite-error)
  ()
  (:feature inquisitio-conditions)
  (:purpose "Specialized condition for constraint violations (UNIQUE, NOT NULL, etc.)"))

(telos:defun/i sqlite-error (error-code message &key
                              statement
                              (db-handle (if statement (db statement)))
                              (sql-text (if statement (sql statement))))
  "Signal an sqlite-error or sqlite-constraint-error condition."
  (:feature inquisitio-conditions)
  (:purpose "Central error signaling function that constructs and signals the appropriate condition type")
  (error (if (eq error-code :constraint)
             'sqlite-constraint-error
             'sqlite-error)
         :format-control (if (listp message) (first message) message)
         :format-arguments (if (listp message) (rest message))
         :db-handle db-handle
         :error-code error-code
         :error-msg (if (and db-handle error-code)
                        (inquisitio.ffi:sqlite3-errmsg (handle db-handle)))
         :statement statement
         :sql sql-text))

(defmethod print-object :after ((obj sqlite-error) stream)
  (unless *print-escape*
    (when (or (and (sqlite-error-code obj)
                   (not (eq (sqlite-error-code obj) :ok)))
              (sqlite-error-message obj))
      (format stream "~&Code ~A: ~A."
              (or (sqlite-error-code obj) :OK)
              (or (sqlite-error-message obj) "no message")))
    (when (sqlite-error-db-handle obj)
      (format stream "~&Database: ~A"
              (database-path (sqlite-error-db-handle obj))))
    (when (sqlite-error-sql obj)
      (format stream "~&SQL: ~A" (sqlite-error-sql obj)))))

;;; Database handle

(telos:defclass/i sqlite-handle ()
  ((handle :accessor handle)
   (database-path :accessor database-path)
   (cache :accessor cache)
   (statements :initform nil :accessor sqlite-handle-statements))
  (:documentation "Encapsulates the connection to a SQLite database. Use connect and disconnect.")
  (:feature inquisitio-core)
  (:purpose "Wrap the raw FFI database pointer with Lisp-side state (cache, statement tracking)"))

(defmethod initialize-instance :after ((object sqlite-handle) &key (database-path ":memory:") &allow-other-keys)
  (cffi:with-foreign-object (ppdb 'inquisitio.ffi:p-sqlite3)
    (let ((error-code (inquisitio.ffi:sqlite3-open database-path ppdb)))
      (if (eq error-code :ok)
          (setf (handle object) (cffi:mem-ref ppdb 'inquisitio.ffi:p-sqlite3)
                (database-path object) database-path)
          (sqlite-error error-code (list "Could not open sqlite3 database ~A" database-path)))))
  (setf (cache object) (make-instance 'inquisitio.cache:mru-cache :cache-size 16 :destructor #'really-finalize-statement)))

;;; Connection management

(telos:defun/i connect (database-path &key busy-timeout)
  "Connect to the sqlite database at the given DATABASE-PATH. Returns the SQLITE-HANDLE connected to the database. Use DISCONNECT to disconnect.
Operations will wait for locked databases for up to BUSY-TIMEOUT milliseconds; if BUSY-TIMEOUT is NIL, then operations on locked databases will fail immediately."
  (:feature inquisitio-core)
  (:purpose "Primary entry point for establishing a database connection")
  (restart-case
      (let ((db (make-instance 'sqlite-handle
                               :database-path (etypecase database-path
                                                (string database-path)
                                                (pathname (namestring database-path))))))
        (when busy-timeout
          (set-busy-timeout db busy-timeout))
        db)
    (retry-query ()
      :report "Retry connecting to the database"
      (connect database-path :busy-timeout busy-timeout))
    (skip-query ()
      :report "Skip the connection attempt and return NIL"
      nil)))

(defun set-busy-timeout (db milliseconds)
  "Sets the maximum amount of time to wait for a locked database."
  (inquisitio.ffi:sqlite3-busy-timeout (handle db) milliseconds))

(telos:defun/i disconnect (handle)
  "Disconnects the given HANDLE from the database. All further operations on the handle are invalid."
  (:feature inquisitio-core)
  (:purpose "Clean shutdown: purge cache, finalize statements, close FFI handle")
  (inquisitio.cache:purge-cache (cache handle))
  (iter (with statements = (copy-list (sqlite-handle-statements handle)))
        (declare (dynamic-extent statements))
        (for statement in statements)
        (really-finalize-statement statement))
  (let ((error-code (inquisitio.ffi:sqlite3-close (handle handle))))
    (unless (eq error-code :ok)
      (sqlite-error error-code "Could not close sqlite3 database." :db-handle handle))
    (slot-makunbound handle 'handle)))

(defun enable-load-extension (db &optional (onoff t))
  "Enable or disable the loading of extensions.
ONOFF: T to enable, NIL to disable."
  (let ((res (inquisitio.ffi:sqlite3-enable-load-extension (handle db) (if onoff 1 0))))
    (unless (eq res :ok)
       (sqlite-error res "Could not enable/disable extension loading" :db-handle db))))

(defun load-extension (db path &optional entry-point)
  "Load an extension from the shared library at PATH.
ENTRY-POINT: Optional name of the entry point function.
Note: Extension loading must be enabled first using ENABLE-LOAD-EXTENSION."
  (cffi:with-foreign-object (errmsg-ptr :pointer)
    (setf (cffi:mem-ref errmsg-ptr :pointer) (cffi:null-pointer))
    (let ((res (inquisitio.ffi:sqlite3-load-extension (handle db) path entry-point errmsg-ptr)))
      (unless (eq res :ok)
        (let ((msg (cffi:mem-ref errmsg-ptr :string)))
          (when (and msg (not (cffi:null-pointer-p (cffi:mem-ref errmsg-ptr :pointer))))
            (inquisitio.ffi:sqlite3-free (cffi:mem-ref errmsg-ptr :pointer)))
          (sqlite-error res (format nil "Could not load extension ~A: ~A" path msg) :db-handle db))))))

;;; Vector blob helpers

(defun float-vector-to-blob (vector)
  "Convert a simple-array of single-float to a byte vector (blob) suitable for sqlite-vec."
  (declare (type (simple-array single-float (*)) vector))
  (let* ((len (length vector))
         (byte-len (* len 4))
         (blob (make-array byte-len :element-type '(unsigned-byte 8))))
    (cffi:with-pointer-to-vector-data (ptr blob)
      (loop for i from 0 below len
            do (setf (cffi:mem-aref ptr :float i) (aref vector i))))
    blob))

(defun blob-to-float-vector (blob)
  "Convert a byte vector (blob) from sqlite-vec to a simple-array of single-float."
  (declare (type (vector (unsigned-byte 8)) blob))
  (let* ((byte-len (length blob))
         (len (/ byte-len 4)))
    (unless (zerop (mod byte-len 4))
      (error "Blob length ~A is not a multiple of 4" byte-len))
    (let ((vector (make-array len :element-type 'single-float)))
      (cffi:with-pointer-to-vector-data (ptr blob)
        (loop for i from 0 below len
              do (setf (aref vector i) (cffi:mem-aref ptr :float i))))
      vector)))

;;; Prepared statements

(telos:defclass/i sqlite-statement ()
  ((db :reader db :initarg :db)
   (handle :accessor handle)
   (sql :reader sql :initarg :sql)
   (columns-count :accessor resultset-columns-count)
   (columns-names :accessor resultset-columns-names :reader statement-column-names)
   (parameters-count :accessor parameters-count)
   (parameters-names :accessor parameters-names :reader statement-bind-parameter-names))
  (:documentation "Represents a prepared SQL statement.")
  (:feature inquisitio-core)
  (:purpose "Wrap a compiled SQLite statement with metadata about columns and parameters"))

(defmethod initialize-instance :after ((object sqlite-statement) &key &allow-other-keys)
  (cffi:with-foreign-object (p-statement 'inquisitio.ffi:p-sqlite3-stmt)
    (cffi:with-foreign-object (p-tail '(:pointer :char))
      (cffi:with-foreign-string (sql (sql object))
        (let ((error-code (inquisitio.ffi:sqlite3-prepare (handle (db object)) sql -1 p-statement p-tail)))
          (unless (eq error-code :ok)
            (sqlite-error error-code "Could not prepare an sqlite statement."
                          :db-handle (db object) :sql-text (sql object)))
          (unless (zerop (cffi:mem-ref (cffi:mem-ref p-tail '(:pointer :char)) :uchar))
            (sqlite-error nil "SQL string contains more than one SQL statement." :sql-text (sql object)))
          (setf (handle object) (cffi:mem-ref p-statement 'inquisitio.ffi:p-sqlite3-stmt)
                (resultset-columns-count object) (inquisitio.ffi:sqlite3-column-count (handle object))
                (resultset-columns-names object) (loop for i below (resultset-columns-count object)
                                                       collect (inquisitio.ffi:sqlite3-column-name (handle object) i))
                (parameters-count object) (inquisitio.ffi:sqlite3-bind-parameter-count (handle object))
                (parameters-names object) (loop for i from 1 to (parameters-count object)
                                                collect (inquisitio.ffi:sqlite3-bind-parameter-name (handle object) i))))))))

(telos:defun/i prepare-statement (db sql)
  "Prepare the statement to the DB that will execute the commands that are in SQL.

Returns the SQLITE-STATEMENT.

SQL must contain exactly one statement.
SQL may have some positional (not named) parameters specified with question marks.

Example:

 select name from users where id = ?"
  (:feature inquisitio-core)
  (:purpose "Obtain a prepared statement, checking the cache first before compiling")
  (or (let ((statement (inquisitio.cache:get-from-cache (cache db) sql)))
        (when statement
          (clear-statement-bindings statement))
        statement)
      (let ((statement (make-instance 'sqlite-statement :db db :sql sql)))
        (push statement (sqlite-handle-statements db))
        statement)))

(defun really-finalize-statement (statement)
  (setf (sqlite-handle-statements (db statement))
        (delete statement (sqlite-handle-statements (db statement))))
  (inquisitio.ffi:sqlite3-finalize (handle statement))
  (slot-makunbound statement 'handle))

(defun finalize-statement (statement)
  "Finalizes the statement and signals that associated resources may be released.
Note: does not immediately release resources because statements are cached."
  (reset-statement statement)
  (inquisitio.cache:put-to-cache (cache (db statement)) (sql statement) statement))

(defun step-statement (statement)
  "Steps to the next row of the resultset of STATEMENT.
Returns T if successfully advanced to the next row and NIL if there are no more rows."
  (let ((error-code (inquisitio.ffi:sqlite3-step (handle statement))))
    (case error-code
      (:done nil)
      (:row t)
      (t
       (sqlite-error error-code "Error while stepping an sqlite statement." :statement statement)))))

(defun reset-statement (statement)
  "Resets the STATEMENT and prepares it to be called again."
  (let ((error-code (inquisitio.ffi:sqlite3-reset (handle statement))))
    (unless (eq error-code :ok)
      (sqlite-error error-code "Error while resetting an sqlite statement." :statement statement))))

(defun clear-statement-bindings (statement)
  "Sets all binding values to NULL."
  (let ((error-code (inquisitio.ffi:sqlite3-clear-bindings (handle statement))))
    (unless (eq error-code :ok)
      (sqlite-error error-code "Error while clearing bindings of an sqlite statement."
                    :statement statement))))

(defun statement-column-value (statement column-number)
  "Returns the COLUMN-NUMBER-th column's value of the current row of the STATEMENT. Columns are numbered from zero.
Returns:
 * NIL for NULL
 * INTEGER for integers
 * DOUBLE-FLOAT for floats
 * STRING for text
 * (SIMPLE-ARRAY (UNSIGNED-BYTE 8)) for BLOBs"
  (let ((type (inquisitio.ffi:sqlite3-column-type (handle statement) column-number)))
    (ecase type
      (:null nil)
      (:text (inquisitio.ffi:sqlite3-column-text (handle statement) column-number))
      (:integer (inquisitio.ffi:sqlite3-column-int64 (handle statement) column-number))
      (:float (inquisitio.ffi:sqlite3-column-double (handle statement) column-number))
      (:blob
       (let* ((blob-length (inquisitio.ffi:sqlite3-column-bytes (handle statement) column-number))
              (result (make-array blob-length :element-type '(unsigned-byte 8)))
              (blob (inquisitio.ffi:sqlite3-column-blob (handle statement) column-number)))
         (loop for i from 0 below blob-length
               do (setf (aref result i) (cffi:mem-aref blob :unsigned-char i)))
         result)))))

;;; Statement macros

(defmacro with-prepared-statement (statement-var (db sql parameters-var) &body body)
  (let ((i-var (gensym "I"))
        (value-var (gensym "VALUE"))
        (db-var (gensym "DB"))
        (sql-var (gensym "SQL"))
        (params-var (gensym "PARAMS")))
    `(let* ((,db-var ,db)
            (,sql-var ,sql)
            (,params-var ,parameters-var)
            (,statement-var (prepare-statement ,db-var ,sql-var)))
       (unwind-protect
            (progn
              (iter (for ,i-var from 1)
                    (declare (type fixnum ,i-var))
                    (for ,value-var in ,params-var)
                    (bind-parameter ,statement-var ,i-var ,value-var))
              ,@body)
         (finalize-statement ,statement-var)))))

(defmacro with-prepared-statement/named (statement-var (db sql parameters-var) &body body)
  (let ((name-var (gensym "NAME"))
        (value-var (gensym "VALUE"))
        (db-var (gensym "DB"))
        (sql-var (gensym "SQL"))
        (params-var (gensym "PARAMS")))
    `(let* ((,db-var ,db)
            (,sql-var ,sql)
            (,params-var ,parameters-var)
            (,statement-var (prepare-statement ,db-var ,sql-var)))
       (unwind-protect
            (progn
              (iter (for (,name-var ,value-var) on ,params-var by #'cddr)
                    (bind-parameter ,statement-var (string ,name-var) ,value-var))
              ,@body)
         (finalize-statement ,statement-var)))))

;;; Parameter binding

(telos:defun/i bind-parameter (statement parameter value)
  "Sets the PARAMETER-th parameter in STATEMENT to the VALUE.
PARAMETER may be parameter index (starting from 1) or parameters name.
Supported types:
 * NULL. Passed as NULL
 * INTEGER. Passed as an 64-bit integer
 * STRING. Passed as a string
 * FLOAT. Passed as a double
 * (VECTOR (UNSIGNED-BYTE 8)) and VECTOR that contains integers in range [0,256). Passed as a BLOB"
  (:feature inquisitio-core)
  (:purpose "Bind a Lisp value to a SQL parameter, dispatching on type")
  (let ((index (etypecase parameter
                 (integer parameter)
                 (string (statement-parameter-index statement parameter)))))
    (declare (type fixnum index))
    (let ((error-code (typecase value
                        (null (inquisitio.ffi:sqlite3-bind-null (handle statement) index))
                        (integer (inquisitio.ffi:sqlite3-bind-int64 (handle statement) index value))
                        (double-float (inquisitio.ffi:sqlite3-bind-double (handle statement) index value))
                        (real (inquisitio.ffi:sqlite3-bind-double (handle statement) index (coerce value 'double-float)))
                        (string (inquisitio.ffi:sqlite3-bind-text (handle statement) index value -1 (inquisitio.ffi:destructor-transient)))
                        ((vector (unsigned-byte 8))
                         (cffi:with-pointer-to-vector-data (ptr value)
                           (inquisitio.ffi:sqlite3-bind-blob (handle statement) index ptr (length value) (inquisitio.ffi:destructor-transient))))
                        (vector
                         (cffi:with-foreign-object (array :unsigned-char (length value))
                           (loop for i from 0 below (length value)
                                 do (setf (cffi:mem-aref array :unsigned-char i) (aref value i)))
                           (inquisitio.ffi:sqlite3-bind-blob (handle statement) index array (length value) (inquisitio.ffi:destructor-transient))))
                        (t
                         (sqlite-error nil
                                       (list "Do not know how to pass value ~A of type ~S to sqlite."
                                             value (class-name (class-of value)))
                                       :statement statement)))))
      (unless (eq error-code :ok)
        (sqlite-error error-code
                      (list "Error when binding parameter ~A to value ~A." parameter value)
                      :statement statement)))))

(defun statement-parameter-index (statement parameter-name)
  (inquisitio.ffi:sqlite3-bind-parameter-index (handle statement) parameter-name))

;;; Query execution

(telos:defun/i execute-non-query (db sql &rest parameters)
  "Executes the query SQL to the database DB with given PARAMETERS. Returns nothing.

Example:

\(execute-non-query db \"insert into users (user_name, real_name) values (?, ?)\" \"joe\" \"Joe the User\")

See BIND-PARAMETER for the list of supported parameter types."
  (:feature inquisitio-core)
  (:purpose "Execute a SQL statement that returns no result set")
  (declare (dynamic-extent parameters))
  (restart-case
      (with-prepared-statement statement (db sql parameters)
        (step-statement statement))
    (retry-query ()
      :report "Retry the query"
      (apply #'execute-non-query db sql parameters))
    (skip-query ()
      :report "Skip this query and return NIL"
      nil)))

(telos:defun/i execute-non-query/named (db sql &rest parameters)
  "Executes the query SQL to the database DB with given PARAMETERS. Returns nothing.

PARAMETERS is a list of alternating parameter names and values.

Example:

\(execute-non-query db \"insert into users (user_name, real_name) values (:name, :real_name)\" \":name\" \"joe\" \":real_name\" \"Joe the User\")

See BIND-PARAMETER for the list of supported parameter types."
  (:feature inquisitio-core)
  (:purpose "Execute a non-query SQL statement with named parameters")
  (declare (dynamic-extent parameters))
  (restart-case
      (with-prepared-statement/named statement (db sql parameters)
        (step-statement statement))
    (retry-query ()
      :report "Retry the query"
      (apply #'execute-non-query/named db sql parameters))
    (skip-query ()
      :report "Skip this query and return NIL"
      nil)))

(telos:defun/i execute-to-list (db sql &rest parameters)
  "Executes the query SQL to the database DB with given PARAMETERS. Returns the results as list of lists.

Example:

\(execute-to-list db \"select id, user_name, real_name from users where user_name = ?\" \"joe\")
=>
\((1 \"joe\" \"Joe the User\")
 (2 \"joe\" \"Another Joe\"))

See BIND-PARAMETER for the list of supported parameter types."
  (:feature inquisitio-core)
  (:purpose "Execute a query and collect all result rows as a list of lists")
  (declare (dynamic-extent parameters))
  (restart-case
      (with-prepared-statement stmt (db sql parameters)
        (let (result)
          (loop (if (step-statement stmt)
                    (push (iter (for i from 0 below (inquisitio.ffi:sqlite3-column-count (handle stmt)))
                                (declare (type fixnum i))
                                (collect (statement-column-value stmt i)))
                          result)
                    (return)))
          (nreverse result)))
    (retry-query ()
      :report "Retry the query"
      (apply #'execute-to-list db sql parameters))
    (skip-query ()
      :report "Skip this query and return NIL"
      nil)))

(telos:defun/i execute-to-list/named (db sql &rest parameters)
  "Executes the query SQL to the database DB with given PARAMETERS. Returns the results as list of lists.

PARAMETERS is a list of alternating parameters names and values.

Example:

\(execute-to-list db \"select id, user_name, real_name from users where user_name = :user_name\" \":user_name\" \"joe\")
=>
\((1 \"joe\" \"Joe the User\")
 (2 \"joe\" \"Another Joe\"))

See BIND-PARAMETER for the list of supported parameter types."
  (:feature inquisitio-core)
  (:purpose "Execute a query with named parameters and collect results as list of lists")
  (declare (dynamic-extent parameters))
  (restart-case
      (with-prepared-statement/named stmt (db sql parameters)
        (let (result)
          (loop (if (step-statement stmt)
                    (push (iter (for i from 0 below (inquisitio.ffi:sqlite3-column-count (handle stmt)))
                                (declare (type fixnum i))
                                (collect (statement-column-value stmt i)))
                          result)
                    (return)))
          (nreverse result)))
    (retry-query ()
      :report "Retry the query"
      (apply #'execute-to-list/named db sql parameters))
    (skip-query ()
      :report "Skip this query and return NIL"
      nil)))

(telos:defun/i execute-one-row-m-v (db sql &rest parameters)
  "Executes the query SQL to the database DB with given PARAMETERS. Returns the first row as multiple values.

Example:
\(execute-one-row-m-v db \"select id, user_name, real_name from users where id = ?\" 1)
=>
\(values 1 \"joe\" \"Joe the User\")

See BIND-PARAMETER for the list of supported parameter types."
  (:feature inquisitio-core)
  (:purpose "Execute a query and return the first row as multiple values")
  (restart-case
      (with-prepared-statement stmt (db sql parameters)
        (if (step-statement stmt)
            (return-from execute-one-row-m-v
              (values-list (iter (for i from 0 below (inquisitio.ffi:sqlite3-column-count (handle stmt)))
                                 (declare (type fixnum i))
                                 (collect (statement-column-value stmt i)))))
            (return-from execute-one-row-m-v
              (values-list (loop repeat (inquisitio.ffi:sqlite3-column-count (handle stmt)) collect nil)))))
    (retry-query ()
      :report "Retry the query"
      (apply #'execute-one-row-m-v db sql parameters))
    (skip-query ()
      :report "Skip this query and return NIL"
      nil)))

(telos:defun/i execute-one-row-m-v/named (db sql &rest parameters)
  "Executes the query SQL to the database DB with given PARAMETERS. Returns the first row as multiple values.

PARAMETERS is a list of alternating parameters names and values.

Example:
\(execute-one-row-m-v db \"select id, user_name, real_name from users where id = :id\" \":id\" 1)
=>
\(values 1 \"joe\" \"Joe the User\")

See BIND-PARAMETER for the list of supported parameter types."
  (:feature inquisitio-core)
  (:purpose "Execute a query with named parameters and return the first row as multiple values")
  (restart-case
      (with-prepared-statement/named stmt (db sql parameters)
        (if (step-statement stmt)
            (return-from execute-one-row-m-v/named
              (values-list (iter (for i from 0 below (inquisitio.ffi:sqlite3-column-count (handle stmt)))
                                 (declare (type fixnum i))
                                 (collect (statement-column-value stmt i)))))
            (return-from execute-one-row-m-v/named
              (values-list (loop repeat (inquisitio.ffi:sqlite3-column-count (handle stmt)) collect nil)))))
    (retry-query ()
      :report "Retry the query"
      (apply #'execute-one-row-m-v/named db sql parameters))
    (skip-query ()
      :report "Skip this query and return NIL"
      nil)))

(telos:defun/i execute-single (db sql &rest parameters)
  "Executes the query SQL to the database DB with given PARAMETERS. Returns the first column of the first row as single value.

Example:
\(execute-single db \"select user_name from users where id = ?\" 1)
=>
\"joe\"

See BIND-PARAMETER for the list of supported parameter types."
  (:feature inquisitio-core)
  (:purpose "Execute a query and return a single scalar value")
  (declare (dynamic-extent parameters))
  (restart-case
      (with-prepared-statement stmt (db sql parameters)
        (if (step-statement stmt)
            (statement-column-value stmt 0)
            nil))
    (retry-query ()
      :report "Retry the query"
      (apply #'execute-single db sql parameters))
    (skip-query ()
      :report "Skip this query and return NIL"
      nil)))

(telos:defun/i execute-single/named (db sql &rest parameters)
  "Executes the query SQL to the database DB with given PARAMETERS. Returns the first column of the first row as single value.

PARAMETERS is a list of alternating parameters names and values.

Example:
\(execute-single db \"select user_name from users where id = :id\" \":id\" 1)
=>
\"joe\"

See BIND-PARAMETER for the list of supported parameter types."
  (:feature inquisitio-core)
  (:purpose "Execute a query with named parameters and return a single scalar value")
  (declare (dynamic-extent parameters))
  (restart-case
      (with-prepared-statement/named stmt (db sql parameters)
        (if (step-statement stmt)
            (statement-column-value stmt 0)
            nil))
    (retry-query ()
      :report "Retry the query"
      (apply #'execute-single/named db sql parameters))
    (skip-query ()
      :report "Skip this query and return NIL"
      nil)))

;;; Row ID

(defun last-insert-rowid (db)
  "Returns the auto-generated ID of the last inserted row on the database connection DB."
  (inquisitio.ffi:sqlite3-last-insert-rowid (handle db)))

;;; Transaction support

(defmacro with-transaction (db &body body)
  "Wraps the BODY inside the transaction."
  (let ((ok (gensym "TRANSACTION-COMMIT-"))
        (db-var (gensym "DB-")))
    `(let (,ok
           (,db-var ,db))
       (execute-non-query ,db-var "begin transaction")
       (unwind-protect
            (multiple-value-prog1
                (progn ,@body)
              (setf ,ok t))
         (if ,ok
             (execute-non-query ,db-var "commit transaction")
             (execute-non-query ,db-var "rollback transaction"))))))

(defmacro with-open-database ((db path &key busy-timeout) &body body)
  `(let ((,db (connect ,path :busy-timeout ,busy-timeout)))
     (unwind-protect
          (progn ,@body)
       (disconnect ,db))))

;;; Iterate macro drivers

(defmacro-driver (FOR vars IN-SQLITE-QUERY query-expression ON-DATABASE db &optional WITH-PARAMETERS parameters)
  (let ((statement (gensym "STATEMENT-"))
        (kwd (if generate 'generate 'for)))
    `(progn (with ,statement = (prepare-statement ,db ,query-expression))
            (finally-protected (when ,statement (finalize-statement ,statement)))
            ,@(when parameters
                    (list `(initially ,@(iter (for i from 1)
                                              (for value in parameters)
                                              (collect `(inquisitio:bind-parameter ,statement ,i ,value))))))
            (,kwd ,(if (symbolp vars)
                       `(values ,vars)
                       `(values ,@vars))
                  next (progn (if (step-statement ,statement)
                                  (values ,@(iter (for i from 0 below (if (symbolp vars) 1 (length vars)))
                                                  (collect `(statement-column-value ,statement ,i))))
                                  (terminate)))))))

(defmacro-driver (FOR vars IN-SQLITE-QUERY/NAMED query-expression ON-DATABASE db &optional WITH-PARAMETERS parameters)
  (let ((statement (gensym "STATEMENT-"))
        (kwd (if generate 'generate 'for)))
    `(progn (with ,statement = (prepare-statement ,db ,query-expression))
            (finally-protected (when ,statement (finalize-statement ,statement)))
            ,@(when parameters
                    (list `(initially ,@(iter (for (name value) on parameters by #'cddr)
                                              (collect `(inquisitio:bind-parameter ,statement ,name ,value))))))
            (,kwd ,(if (symbolp vars)
                       `(values ,vars)
                       `(values ,@vars))
                  next (progn (if (step-statement ,statement)
                                  (values ,@(iter (for i from 0 below (if (symbolp vars) 1 (length vars)))
                                                  (collect `(statement-column-value ,statement ,i))))
                                  (terminate)))))))

(defmacro-driver (FOR vars ON-SQLITE-STATEMENT statement)
  (let ((statement-var (gensym "STATEMENT-"))
        (kwd (if generate 'generate 'for)))
    `(progn (with ,statement-var = ,statement)
            (,kwd ,(if (symbolp vars)
                       `(values ,vars)
                       `(values ,@vars))
                  next (progn (if (step-statement ,statement-var)
                                  (values ,@(iter (for i from 0 below (if (symbolp vars) 1 (length vars)))
                                                  (collect `(statement-column-value ,statement-var ,i))))
                                  (terminate)))))))
