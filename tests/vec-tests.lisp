(in-package :inquisitio-tests)

(in-suite inquisitio-suite)

(defvar *vec-db* nil)

(defun vec-extension-path ()
  "Namestring of the sqlite-vec loadable extension, or NIL when it is absent.
The binary is deliberately not in the repository — libs/ is gitignored. Fetch a
release from https://github.com/asg017/sqlite-vec/releases and drop vec0.dylib
\(macOS) or vec0.so (Linux) into libs/ to enable these tests."
  (some (lambda (name)
          (let ((path (asdf:system-relative-pathname
                       :inquisitio (concatenate 'string "libs/" name))))
            (when (probe-file path) (namestring path))))
        #+darwin '("vec0.dylib" "vec0.so")
        #-darwin '("vec0.so" "vec0.dylib")))

(defmacro with-vec-test-db (&body body)
  "Run BODY against an in-memory database with sqlite-vec loaded.
Skips rather than fails when the extension is not installed locally, so that a
missing optional binary does not read as a broken library."
  (let ((path (gensym "EXTENSION-PATH-")))
    `(let ((,path (vec-extension-path)))
       (if (null ,path)
           (skip "sqlite-vec extension not installed; see VEC-EXTENSION-PATH for how to enable these tests")
           (with-open-database (*vec-db* ":memory:")
             (enable-load-extension *vec-db* t)
             (load-extension *vec-db* ,path (cffi:null-pointer))
             ,@body)))))

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
