(in-package :inquisitio-tests)

(in-suite inquisitio-suite)

;;;; Regression tests for the thread-safety defects reported 2026-07-25.
;;;;
;;;; These exercise a SINGLE handle shared across threads, which is the case the
;;;; pre-existing TEST-CONCURRENT-INSERTS does not cover (it gives each thread
;;;; its own connection).

(defun spawn-and-join (count function)
  "Run (funcall FUNCTION i) for i below COUNT, one thread each, and wait for all."
  (let ((threads (iter (for i from 0 below count)
                       (collect (bt:make-thread (let ((i i))
                                                  (lambda () (funcall function i))))))))
    (mapc #'bt:join-thread threads)))

(defun collecting-errors (function)
  "Return a closure that runs FUNCTION and collects any error it signals.
Returns two values: the closure and a thunk yielding the collected messages."
  (let ((errors '())
        (lock (bt:make-lock "collected-errors")))
    (values (lambda (i)
              (handler-case (funcall function i)
                (error (c)
                  (bt:with-lock-held (lock)
                    (push (princ-to-string c) errors)))))
            (lambda () errors))))

(defun cache-bookkeeping-consistent-p (cache)
  "T when the cache's TOTAL-CACHED counter matches the objects it actually holds."
  (let ((real 0))
    (maphash (lambda (id stack)
               (declare (ignore id))
               (incf real (length stack)))
             (inquisitio.cache::objects-table cache))
    (= real (inquisitio.cache::total-cached cache))))

(defun cached-statements (cache)
  "Every statement currently parked in CACHE."
  (let ((result '()))
    (maphash (lambda (id stack)
               (declare (ignore id))
               (iter (for stmt in-vector stack)
                     (push stmt result)))
             (inquisitio.cache::objects-table cache))
    result))

;;; D2 + D3 — MRU cache containers and the statements list

#+thread-support
(test test-shared-handle-concurrent-queries
  "N threads hammering one handle must not corrupt the cache or the statements list."
  (let ((threads-count 8)
        (iterations 150))
    (with-open-database (db ":memory:")
      (execute-non-query db "create table t (id integer primary key, v text)")
      (execute-non-query db "insert into t (v) values ('seed')")
      (multiple-value-bind (worker collected)
          ;; Several distinct SQL texts, so the 16-entry MRU cache is under pressure.
          (collecting-errors
           (lambda (n)
             (iter (for i from 0 below iterations)
                   (execute-to-list db "select id, v from t where id = ?" 1)
                   (execute-single db "select count(*) from t")
                   (execute-single db "select max(id) from t")
                   (execute-to-list db "select v from t")
                   (execute-non-query db "insert into t (v) values (?)"
                                      (format nil "~D-~D" n i)))))
        (spawn-and-join threads-count worker)
        (is (null (funcall collected))
            "Worker threads signalled: ~{~A~^~%~}" (funcall collected)))
      (is-true (cache-bookkeeping-consistent-p (inquisitio::cache db))
               "TOTAL-CACHED drifted from the cache's real contents")
      ;; Every parked statement must still be tracked, or DISCONNECT leaks it.
      (let ((untracked (set-difference (cached-statements (inquisitio::cache db))
                                       (inquisitio::sqlite-handle-statements db))))
        (is (null untracked)
            "~D cached statement(s) dropped from the handle's statements list"
            (length untracked)))
      (is (= (* threads-count iterations)
             (1- (execute-single db "select count(*) from t")))))))

#+thread-support
(test test-shared-handle-disconnect-succeeds
  "DISCONNECT must not signal SQLITE_BUSY after concurrent use of one handle."
  (let ((db (connect ":memory:")))
    (execute-non-query db "create table t (id integer primary key)")
    (spawn-and-join 6
                    (lambda (n)
                      (declare (ignore n))
                      (iter (repeat 100)
                            (execute-non-query db "insert into t (id) values (null)")
                            (execute-single db "select count(*) from t"))))
    (finishes (disconnect db))))

;;; D1 — transactions

(test test-nested-transaction-commits
  "A nested WITH-TRANSACTION must not abort the outer one."
  (with-open-database (db ":memory:")
    (execute-non-query db "create table t (id integer primary key)")
    (finishes
      (with-transaction db
        (execute-non-query db "insert into t (id) values (1)")
        (with-transaction db
          (execute-non-query db "insert into t (id) values (2)"))
        (execute-non-query db "insert into t (id) values (3)")))
    (is (equal '((1) (2) (3)) (execute-to-list db "select id from t order by id")))))

(test test-nested-transaction-inner-rollback
  "An inner failure rolls back only the inner scope; the outer still commits."
  (with-open-database (db ":memory:")
    (execute-non-query db "create table t (id integer primary key)")
    (let ((inner-ran nil))
      (with-transaction db
        (execute-non-query db "insert into t (id) values (1)")
        (ignore-errors
         (with-transaction db
           (execute-non-query db "insert into t (id) values (2)")
           (setf inner-ran t)
           (error "inner scope fails")))
        (execute-non-query db "insert into t (id) values (3)"))
      ;; Without SAVEPOINT support the inner BEGIN errors out and the body never
      ;; runs, which would make the row assertion below pass for the wrong reason.
      (is-true inner-ran "The inner transaction body never executed"))
    (is (equal '((1) (3)) (execute-to-list db "select id from t order by id")))))

(test test-nested-transaction-outer-rollback
  "An outer failure discards released inner scopes too."
  (with-open-database (db ":memory:")
    (execute-non-query db "create table t (id integer primary key)")
    (let ((inner-ran nil))
      (ignore-errors
       (with-transaction db
         (with-transaction db
           (execute-non-query db "insert into t (id) values (1)")
           (setf inner-ran t))
         (error "outer scope fails")))
      (is-true inner-ran "The inner transaction body never executed"))
    (is (= 0 (execute-single db "select count(*) from t")))))

#+thread-support
(test test-concurrent-transactions-shared-handle
  "Threads transacting on one handle must not merge or discard each other's work."
  (let ((threads-count 4)
        (rows-per-thread 25))
    (with-open-database (db ":memory:")
      (execute-non-query db "create table t (owner integer, n integer)")
      (multiple-value-bind (worker collected)
          (collecting-errors
           (lambda (owner)
             (iter (for n from 0 below rows-per-thread)
                   (with-transaction db
                     (execute-non-query db "insert into t (owner, n) values (?, ?)" owner n)))))
        (spawn-and-join threads-count worker)
        (is (null (funcall collected))
            "Transacting threads signalled: ~{~A~^~%~}" (funcall collected)))
      (is (= (* threads-count rows-per-thread)
             (execute-single db "select count(*) from t"))
          "Rows were lost to an interleaved transaction")
      (iter (for owner from 0 below threads-count)
            (is (= rows-per-thread
                   (execute-single db "select count(*) from t where owner = ?" owner)))))))

#+thread-support
(test test-transaction-rollback-does-not-discard-other-thread-work
  "A rolled-back transaction in one thread must not destroy another thread's rows."
  (with-open-database (db ":memory:")
    (execute-non-query db "create table t (v text)")
    (spawn-and-join
     2
     (lambda (i)
       (if (zerop i)
           (iter (repeat 50)
                 (with-transaction db
                   (execute-non-query db "insert into t (v) values ('keep')")))
           (iter (repeat 50)
                 (ignore-errors
                  (with-transaction db
                    (execute-non-query db "insert into t (v) values ('drop')")
                    (error "abort")))))))
    (is (= 50 (execute-single db "select count(*) from t where v = 'keep'")))
    (is (= 0 (execute-single db "select count(*) from t where v = 'drop'")))))

(test test-transaction-returns-body-values
  "WITH-TRANSACTION must still return all of BODY's values through the lock."
  (with-open-database (db ":memory:")
    (is (equal '(1 2 3)
               (multiple-value-list (with-transaction db (values 1 2 3)))))))

#+thread-support
(test test-with-database-lock-makes-insert-and-rowid-atomic
  "WITH-DATABASE-LOCK is the documented remedy for connection-global reads."
  (let ((threads-count 4)
        (rows-per-thread 40))
    (with-open-database (db ":memory:")
      (execute-non-query db "create table t (id integer primary key, owner integer)")
      (multiple-value-bind (worker collected)
          (collecting-errors
           (lambda (owner)
             (iter (repeat rows-per-thread)
                   (let ((id (with-database-lock (db)
                               (execute-non-query db "insert into t (owner) values (?)" owner)
                               (last-insert-rowid db))))
                     (assert (= owner (execute-single db "select owner from t where id = ?" id))
                             () "Row ~D belongs to another thread" id)))))
        (spawn-and-join threads-count worker)
        (is (null (funcall collected))
            "LAST-INSERT-ROWID returned another thread's row: ~{~A~^~%~}" (funcall collected))))))

;;; D5 — busy timeout default

(defun busy-timeout-test-file ()
  (namestring (merge-pathnames "inquisitio-busy-timeout-test.sqlite"
                               (uiop:temporary-directory))))

(defmacro with-write-lock-held-elsewhere ((path &key (hold-seconds 1.0)) &body body)
  "Run BODY while another thread holds a write transaction on the database at PATH.
The other thread releases the lock after HOLD-SECONDS."
  (let ((taken (gensym "TAKEN-"))
        (holder (gensym "HOLDER-"))
        (db (gensym "DB-"))
        (path-var (gensym "PATH-")))
    `(let* ((,path-var ,path)
            (,taken (bt:make-semaphore))
            (,holder (bt:make-thread
                      (lambda ()
                        (with-open-database (,db ,path-var)
                          (execute-non-query ,db "begin immediate transaction")
                          (execute-non-query ,db "insert into t (id) values (1)")
                          (bt:signal-semaphore ,taken)
                          (sleep ,hold-seconds)
                          (execute-non-query ,db "commit transaction"))))))
       (bt:wait-on-semaphore ,taken :timeout 10)
       (unwind-protect (progn ,@body)
         (bt:join-thread ,holder)))))

(defmacro with-fresh-test-database ((path) &body body)
  (let ((path-var (gensym "PATH-"))
        (db (gensym "DB-")))
    `(let ((,path-var (busy-timeout-test-file)))
       (when (probe-file ,path-var) (delete-file ,path-var))
       (unwind-protect
            (let ((,path ,path-var))
              (with-open-database (,db ,path)
                (execute-non-query ,db "create table t (id integer primary key)"))
              ,@body)
         (when (probe-file ,path-var) (delete-file ,path-var))))))

#+thread-support
(test test-connect-defaults-to-a-busy-timeout
  "CONNECT's default must wait for a locked database rather than failing immediately."
  (with-fresh-test-database (path)
    ;; The holder releases after 0.3s; a sane default busy-timeout rides that out.
    (with-write-lock-held-elsewhere (path :hold-seconds 0.3)
      (with-open-database (db path)
        (finishes (execute-non-query db "insert into t (id) values (2)"))))))

#+thread-support
(test test-explicit-nil-busy-timeout-still-fails-fast
  "Passing :BUSY-TIMEOUT NIL must keep the fail-fast behaviour."
  (with-fresh-test-database (path)
    (with-write-lock-held-elsewhere (path)
      (with-open-database (db path :busy-timeout nil)
        (signals sqlite-error
          (execute-non-query db "insert into t (id) values (2)"))))))

;;; D4 — threading-mode introspection

(test test-sqlite-threadsafe-is-introspectable
  "Callers must be able to ask what threading mode the linked SQLite was built with."
  (is (typep (sqlite-threadsafe) '(integer 0 2))))
