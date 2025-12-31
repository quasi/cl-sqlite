(in-package :cl-user)
(require :sqlite)
(require :fiveam)

(defpackage :sqlite-vec-tests
  (:use :cl :sqlite :fiveam))

(in-package :sqlite-vec-tests)

(def-suite :sqlite-vec-suite :description "Tests for sqlite-vec syntactic sugar")
(in-suite :sqlite-vec-suite)

(defvar *db* nil)

(defmacro with-test-db (&body body)
  `(with-open-database (*db* ":memory:")
     (enable-load-extension *db* t)
     (let ((extension-path (merge-pathnames "libs/vec0.so" (uiop:getcwd))))
       ;; Try loading extension, if it fails, skip tests that depend on it?
       ;; Or just fail.
       (load-extension *db* (namestring extension-path) (cffi:null-pointer)))
     ,@body))

(test create-vector-table-test
  (with-test-db
    (create-vector-table *db* "vec_items" '((embedding 4)))
    ;; Verify table exists and has correct columns (by inserting)
    (execute-non-query *db* "INSERT INTO vec_items(rowid, embedding) VALUES (1, '[0.1, 0.2, 0.3, 0.4]')")
    (is (= 1 (execute-single *db* "SELECT count(*) FROM vec_items")))))

(test vector-search-test
  (with-test-db
    (create-vector-table *db* "items" '((embedding 2)))
    ;; Insert some vectors
    (execute-non-query *db* "INSERT INTO items(rowid, embedding) VALUES (1, '[1.0, 1.0]')")
    (execute-non-query *db* "INSERT INTO items(rowid, embedding) VALUES (2, '[2.0, 2.0]')")
    (execute-non-query *db* "INSERT INTO items(rowid, embedding) VALUES (3, '[5.0, 5.0]')")

    ;; Search for [1.1, 1.1], should find row 1 first
    (let ((results (vector-search *db* "items" "[1.1, 1.1]" :k 2)))
      (is (= 2 (length results)))
      (is (= 1 (first (first results)))) ;; rowid 1
      (is (< (second (first results)) (second (second results)))) ;; distance check
      )

    ;; Search with Lisp vector
    (let ((vec (make-array 2 :element-type 'single-float :initial-contents '(1.1 1.1))))
      (let ((results (vector-search *db* "items" vec :k 1)))
        (is (= 1 (length results)))
        (is (= 1 (first (first results))))))))

(test scalar-functions-test
  (with-test-db
    (let* ((v1 (make-array 2 :element-type 'single-float :initial-contents '(1.0 2.0)))
           (v2 (make-array 2 :element-type 'single-float :initial-contents '(3.0 4.0)))
           (sum (vec-add *db* v1 v2)))
      (is (equalp (make-array 2 :element-type 'single-float :initial-contents '(4.0 6.0))
                  sum)))

    (let* ((v1 (make-array 2 :element-type 'single-float :initial-contents '(1.0 2.0)))
           (v2 (make-array 2 :element-type 'single-float :initial-contents '(1.0 2.0)))
           (dist (vec-distance-L2 *db* v1 v2)))
      (is (= 0.0 dist)))

    (let* ((v1 (make-array 2 :element-type 'single-float :initial-contents '(1.0 0.0)))
           (norm (vec-normalize *db* v1)))
      (is (equalp (make-array 2 :element-type 'single-float :initial-contents '(1.0 0.0))
                  norm)))
    ))

(defun run-tests ()
  (run! :sqlite-vec-suite))

(run-tests)
