(in-package :inquisitio-tests)

(in-suite inquisitio-suite)

(defvar *vec-db* nil)

(defmacro with-vec-test-db (&body body)
  `(with-open-database (*vec-db* ":memory:")
     (enable-load-extension *vec-db* t)
     (let ((extension-path (merge-pathnames "libs/vec0.so" (uiop:getcwd))))
       (load-extension *vec-db* (namestring extension-path) (cffi:null-pointer)))
     ,@body))

(test create-vector-table-test
  (with-vec-test-db
    (create-vector-table *vec-db* "vec_items" '((embedding 4)))
    (execute-non-query *vec-db* "INSERT INTO vec_items(rowid, embedding) VALUES (1, '[0.1, 0.2, 0.3, 0.4]')")
    (is (= 1 (execute-single *vec-db* "SELECT count(*) FROM vec_items")))))

(test vector-search-test
  (with-vec-test-db
    (create-vector-table *vec-db* "items" '((embedding 2)))
    (execute-non-query *vec-db* "INSERT INTO items(rowid, embedding) VALUES (1, '[1.0, 1.0]')")
    (execute-non-query *vec-db* "INSERT INTO items(rowid, embedding) VALUES (2, '[2.0, 2.0]')")
    (execute-non-query *vec-db* "INSERT INTO items(rowid, embedding) VALUES (3, '[5.0, 5.0]')")

    (let ((results (vector-search *vec-db* "items" "[1.1, 1.1]" :k 2)))
      (is (= 2 (length results)))
      (is (= 1 (first (first results))))
      (is (< (second (first results)) (second (second results)))))

    (let ((vec (make-array 2 :element-type 'single-float :initial-contents '(1.1 1.1))))
      (let ((results (vector-search *vec-db* "items" vec :k 1)))
        (is (= 1 (length results)))
        (is (= 1 (first (first results))))))))

(test scalar-functions-test
  (with-vec-test-db
    (let* ((v1 (make-array 2 :element-type 'single-float :initial-contents '(1.0 2.0)))
           (v2 (make-array 2 :element-type 'single-float :initial-contents '(3.0 4.0)))
           (sum (vec-add *vec-db* v1 v2)))
      (is (equalp (make-array 2 :element-type 'single-float :initial-contents '(4.0 6.0))
                  sum)))

    (let* ((v1 (make-array 2 :element-type 'single-float :initial-contents '(1.0 2.0)))
           (v2 (make-array 2 :element-type 'single-float :initial-contents '(1.0 2.0)))
           (dist (vec-distance-L2 *vec-db* v1 v2)))
      (is (= 0.0 dist)))

    (let* ((v1 (make-array 2 :element-type 'single-float :initial-contents '(1.0 0.0)))
           (norm (vec-normalize *vec-db* v1)))
      (is (equalp (make-array 2 :element-type 'single-float :initial-contents '(1.0 0.0))
                  norm)))))
