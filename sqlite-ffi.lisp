(defpackage :sqlite-ffi
  (:use :cl :cffi)
  (:export :error-code
           :p-sqlite3
           :sqlite3-open
           :sqlite3-close
           :sqlite3-errmsg
           :sqlite3-busy-timeout
           :p-sqlite3-stmt
           :sqlite3-prepare
           :sqlite3-finalize
           :sqlite3-step
           :sqlite3-reset
           :sqlite3-clear-bindings
           :sqlite3-column-count
           :sqlite3-column-type
           :sqlite3-column-text
           :sqlite3-column-int64
           :sqlite3-column-double
           :sqlite3-column-bytes
           :sqlite3-column-blob
           :sqlite3-column-name
           :sqlite3-bind-parameter-count
           :sqlite3-bind-parameter-name
           :sqlite3-bind-parameter-index
           :sqlite3-bind-double
           :sqlite3-bind-int64
           :sqlite3-bind-null
           :sqlite3-bind-text
           :sqlite3-bind-blob
           :destructor-transient
           :destructor-static
           :sqlite3-last-insert-rowid
           :sqlite3-open-v2
           :sqlite3-exec
           :sqlite3-get-table
           :sqlite3-free-table
           :sqlite3-column-int
           :sqlite3-column-value
           :sqlite3-bind-zeroblob
           :sqlite3-bind-value
           :sqlite3-limit
           :sqlite3-config
           :sqlite3-create-function
           :sqlite3-create-function-v2
           :sqlite3-create-collation
           :sqlite3-user-data
           :sqlite3-aggregate-context
           :sqlite3-result-blob
           :sqlite3-result-double
           :sqlite3-result-error
           :sqlite3-result-error-code
           :sqlite3-result-error-toobig
           :sqlite3-result-error-nomem
           :sqlite3-result-int
           :sqlite3-result-int64
           :sqlite3-result-null
           :sqlite3-result-text
           :sqlite3-result-value
           :sqlite3-result-zeroblob
           :sqlite3-value-blob
           :sqlite3-value-bytes
           :sqlite3-value-double
           :sqlite3-value-int
           :sqlite3-value-int64
           :sqlite3-value-text
           :sqlite3-value-type
           :sqlite3-value-numeric-type
           :p-sqlite3-value
           :p-sqlite3-context
           :exec-callback
           :sqlite3-enable-load-extension
           :sqlite3-load-extension
           :sqlite3-free))

(in-package :sqlite-ffi)

(define-foreign-library sqlite3-lib
  (:darwin (:default "libsqlite3"))
  (:unix (:or "libsqlite3.so.0" "libsqlite3.so"))
  (t (:or (:default "libsqlite3") (:default "sqlite3"))))

(use-foreign-library sqlite3-lib)

(defcenum error-code
  (:OK 0)
  (:ERROR 1)
  (:INTERNAL 2)
  (:PERM 3)
  (:ABORT 4)
  (:BUSY 5)
  (:LOCKED 6)
  (:NOMEM 7)
  (:READONLY 8)
  (:INTERRUPT 9)
  (:IOERR 10)
  (:CORRUPT 11)
  (:NOTFOUND 12)
  (:FULL 13)
  (:CANTOPEN 14)
  (:PROTOCOL 15)
  (:EMPTY 16)
  (:SCHEMA 17)
  (:TOOBIG 18)
  (:CONSTRAINT 19)
  (:MISMATCH 20)
  (:MISUSE 21)
  (:NOLFS 22)
  (:AUTH 23)
  (:FORMAT 24)
  (:RANGE 25)
  (:NOTADB 26)
  (:NOTICE 27)
  (:WARNING 28)
  (:ROW 100)
  (:DONE 101))

(defcstruct sqlite3)

(defctype p-sqlite3 (:pointer (:struct sqlite3)))

(defcfun sqlite3-open error-code
  (filename :string)
  (db (:pointer p-sqlite3)))

(defcfun sqlite3-close error-code
  (db p-sqlite3))

(defcfun sqlite3-enable-load-extension error-code
  (db p-sqlite3)
  (onoff :int))

(defcfun sqlite3-load-extension error-code
  (db p-sqlite3)
  (file :string)
  (proc :string)
  (errmsg (:pointer :string)))

(defcfun sqlite3-free :void
  (ptr :pointer))

(defcfun sqlite3-errmsg :string
  (db p-sqlite3))

(defcfun sqlite3-busy-timeout :int
  (db p-sqlite3)
  (ms :int))

(defcstruct sqlite3-stmt)

(defctype p-sqlite3-stmt (:pointer (:struct sqlite3-stmt)))

(defcfun (sqlite3-prepare "sqlite3_prepare_v2") error-code
  (db p-sqlite3)
  (sql :string)
  (sql-length-bytes :int)
  (stmt (:pointer p-sqlite3-stmt))
  (tail (:pointer (:pointer :char))))

(defcfun sqlite3-finalize error-code
  (statement p-sqlite3-stmt))

(defcfun sqlite3-step error-code
  (statement p-sqlite3-stmt))

(defcfun sqlite3-reset error-code
  (statement p-sqlite3-stmt))

(defcfun sqlite3-clear-bindings error-code
  (statement p-sqlite3-stmt))

(defcfun sqlite3-column-count :int
  (statement p-sqlite3-stmt))

(defcenum type-code
  (:integer 1)
  (:float 2)
  (:text 3)
  (:blob 4)
  (:null 5))

(defcfun sqlite3-column-type type-code
  (statement p-sqlite3-stmt)
  (column-number :int))

(defcfun sqlite3-column-text :string
  (statement p-sqlite3-stmt)
  (column-number :int))

(defcfun sqlite3-column-int64 :int64
  (statement p-sqlite3-stmt)
  (column-number :int))

(defcfun sqlite3-column-double :double
  (statement p-sqlite3-stmt)
  (column-number :int))

(defcfun sqlite3-column-bytes :int
  (statement p-sqlite3-stmt)
  (column-number :int))

(defcfun sqlite3-column-blob :pointer
  (statement p-sqlite3-stmt)
  (column-number :int))

(defcfun sqlite3-column-name :string
  (statement p-sqlite3-stmt)
  (column-number :int))

(defcfun sqlite3-bind-parameter-count :int
  (statement p-sqlite3-stmt))

(defcfun sqlite3-bind-parameter-name :string
  (statement p-sqlite3-stmt)
  (column-number :int))

(defcfun sqlite3-bind-parameter-index :int
  (statement p-sqlite3-stmt)
  (name :string))

(defcfun sqlite3-bind-double error-code
  (statement p-sqlite3-stmt)
  (parameter-index :int)
  (value :double))

(defcfun sqlite3-bind-int64 error-code
  (statement p-sqlite3-stmt)
  (parameter-index :int)
  (value :int64))

(defcfun sqlite3-bind-null error-code
  (statement p-sqlite3-stmt)
  (parameter-index :int))

(defcfun sqlite3-bind-text error-code
  (statement p-sqlite3-stmt)
  (parameter-index :int)
  (value :string)
  (octets-count :int)
  (destructor :pointer))

(defcfun sqlite3-bind-blob error-code
  (statement p-sqlite3-stmt)
  (parameter-index :int)
  (value :pointer)
  (bytes-count :int)
  (destructor :pointer))

(defconstant destructor-transient-address (mod -1 (expt 2 (* 8 (cffi:foreign-type-size :pointer)))))

(defun destructor-transient () (cffi:make-pointer destructor-transient-address))

(defun destructor-static () (cffi:make-pointer 0))

(defcfun sqlite3-last-insert-rowid :int64
  (db p-sqlite3))

;;; Additional Core and Convenience

(defcfun sqlite3-open-v2 error-code
  (filename :string)
  (ppdb (:pointer p-sqlite3))
  (flags :int)
  (zVfs :string))

;; Exec
(defcallback exec-callback :int
    ((user-data :pointer)
     (argc :int)
     (argv (:pointer :string))
     (col-names (:pointer :string)))
  (declare (ignore user-data argc argv col-names))
  0) ;; Default implementation, likely to be overridden or used as null

(defcfun sqlite3-exec error-code
  (db p-sqlite3)
  (sql :string)
  (callback :pointer) ;; sqlite3_callback
  (arg :pointer)
  (errmsg (:pointer :string)))

;; Get table
(defcfun sqlite3-get-table error-code
  (db p-sqlite3)
  (zSql :string)
  (pazResult (:pointer (:pointer :string)))
  (pnRow (:pointer :int))
  (pnColumn (:pointer :int))
  (pzErrmsg (:pointer :string)))

(defcfun sqlite3-free-table :void
  (result (:pointer :string)))

;;; Additional Columns

(defcfun sqlite3-column-int :int
  (statement p-sqlite3-stmt)
  (iCol :int))

;; Value object (for sqlite3_column_value and extending)
(defcstruct sqlite3-value)
(defctype p-sqlite3-value (:pointer (:struct sqlite3-value)))

(defcfun sqlite3-column-value p-sqlite3-value
  (statement p-sqlite3-stmt)
  (iCol :int))

;;; Additional Bindings

(defcfun sqlite3-bind-zeroblob error-code
  (statement p-sqlite3-stmt)
  (i :int)
  (n :int))

(defcfun sqlite3-bind-value error-code
  (statement p-sqlite3-stmt)
  (i :int)
  (value p-sqlite3-value))

;;; Configuration

(defcfun sqlite3-limit :int
  (db p-sqlite3)
  (id :int)
  (new-val :int))

;; sqlite3_config is varargs, tricky in CFFI directly without specific wrappers.
;; But we can define it for specific common cases if needed, or just generic with :pointer?
;; For now, let's declare it taking an int option and a pointer argument.
;; (defcfun sqlite3-config error-code
;;   (op :int)
;;   &rest)

;;; Extending SQLite

(defcstruct sqlite3-context)
(defctype p-sqlite3-context (:pointer (:struct sqlite3-context)))

;; Callbacks for create_function
(defctype x-func :pointer) ;; void (*xFunc)(sqlite3_context*,int,sqlite3_value**)
(defctype x-step :pointer) ;; void (*xStep)(sqlite3_context*,int,sqlite3_value**)
(defctype x-final :pointer) ;; void (*xFinal)(sqlite3_context*)
(defctype x-destroy :pointer) ;; void(*xDestroy)(void*)

(defcfun sqlite3-create-function error-code
  (db p-sqlite3)
  (zFunctionName :string)
  (nArg :int)
  (eTextRep :int)
  (pApp :pointer)
  (xFunc x-func)
  (xStep x-step)
  (xFinal x-final))

(defcfun sqlite3-create-function-v2 error-code
  (db p-sqlite3)
  (zFunctionName :string)
  (nArg :int)
  (eTextRep :int)
  (pApp :pointer)
  (xFunc x-func)
  (xStep x-step)
  (xFinal x-final)
  (xDestroy x-destroy))

(defcfun sqlite3-create-collation error-code
  (db p-sqlite3)
  (zName :string)
  (eTextRep :int)
  (pArg :pointer)
  (xCompare :pointer)) ;; int(*xCompare)(void*,int,const void*,int,const void*)

(defcfun sqlite3-user-data :pointer
  (context p-sqlite3-context))

(defcfun sqlite3-aggregate-context :pointer
  (context p-sqlite3-context)
  (nBytes :int))

;; Result functions
(defcfun sqlite3-result-blob :void
  (context p-sqlite3-context)
  (value :pointer)
  (n :int)
  (destroy :pointer))

(defcfun sqlite3-result-double :void
  (context p-sqlite3-context)
  (value :double))

(defcfun sqlite3-result-error :void
  (context p-sqlite3-context)
  (msg :string)
  (n :int))

(defcfun sqlite3-result-error-code :void
  (context p-sqlite3-context)
  (code :int))

(defcfun sqlite3-result-error-toobig :void
  (context p-sqlite3-context))

(defcfun sqlite3-result-error-nomem :void
  (context p-sqlite3-context))

(defcfun sqlite3-result-int :void
  (context p-sqlite3-context)
  (value :int))

(defcfun sqlite3-result-int64 :void
  (context p-sqlite3-context)
  (value :int64))

(defcfun sqlite3-result-null :void
  (context p-sqlite3-context))

(defcfun sqlite3-result-text :void
  (context p-sqlite3-context)
  (value :string)
  (n :int)
  (destroy :pointer))

(defcfun sqlite3-result-value :void
  (context p-sqlite3-context)
  (value p-sqlite3-value))

(defcfun sqlite3-result-zeroblob :void
  (context p-sqlite3-context)
  (n :int))

;; Value functions (extracting arguments in functions)
(defcfun sqlite3-value-blob :pointer
  (value p-sqlite3-value))

(defcfun sqlite3-value-bytes :int
  (value p-sqlite3-value))

(defcfun sqlite3-value-double :double
  (value p-sqlite3-value))

(defcfun sqlite3-value-int :int
  (value p-sqlite3-value))

(defcfun sqlite3-value-int64 :int64
  (value p-sqlite3-value))

(defcfun sqlite3-value-text :string
  (value p-sqlite3-value))

(defcfun sqlite3-value-type :int
  (value p-sqlite3-value))

(defcfun sqlite3-value-numeric-type :int
  (value p-sqlite3-value))
