(in-package :sqlite)

(export '(create-vector-table
          vector-search
          vec-add
          vec-sub
          vec-distance-L2
          vec-distance-cosine
          vec-distance-hamming
          vec-normalize
          vec-slice
          vec-to-json
          vec-length))

(defun create-vector-table (db name columns &key (if-not-exists nil))
  "Creates a virtual table for vector search using vec0.
   COLUMNS is a list of column definitions. Each definition can be:
   - (name dimension) : defaults to float (vec_f32)
   - (name type dimension) : type can be :float, :bit, :int8

   Example:
     (create-vector-table db 'items '((embedding 4) (features :bit 8)))"
  (let ((col-defs (loop for col in columns
                        collect (destructuring-bind (name &optional type dim) col
                                  (let ((d (or dim type))
                                        (t-type (if dim type :float)))
                                    (format nil "~A ~A[~A]"
                                            (normalize-name name)
                                            (case t-type
                                              (:float "float")
                                              (:bit "bit")
                                              (:int8 "int8")
                                              (t (string-downcase (string t-type))))
                                            d))))))
    (let ((sql (format nil "CREATE VIRTUAL TABLE ~@[IF NOT EXISTS ~]~A USING vec0(~{~A~^, ~})"
                       if-not-exists
                       (normalize-name name)
                       col-defs)))
      (execute-non-query db sql))))

(defun vector-search (db table query-vector &key (k 10) (column 'embedding) (output-columns '(rowid distance)))
  "Performs a vector search on a vec0 virtual table.
   QUERY-VECTOR: a Lisp vector (will be converted to blob if needed) or a string/blob.
   K: limit (default 10).
   COLUMN: the vector column name to match against (default 'embedding).
   OUTPUT-COLUMNS: list of columns to return (default '(rowid distance)).

   Returns a list of lists (rows)."
  (let* ((q-vec (if (typep query-vector 'string)
                    query-vector
                    (if (vectorp query-vector)
                        (float-vector-to-blob (coerce query-vector '(simple-array single-float (*))))
                        query-vector)))
         (cols-sql (format nil "~{~A~^, ~}" (mapcar #'normalize-name output-columns)))
         (sql (format nil "SELECT ~A FROM ~A WHERE ~A MATCH ? AND k = ? ORDER BY distance"
                      cols-sql
                      (normalize-name table)
                      (normalize-name column))))
    (execute-to-list db sql q-vec k)))

;; Scalar function wrappers

(defun wrap-scalar-vec-func (db func-name arg1 &optional arg2)
  (let ((sql (if arg2
                 (format nil "SELECT ~A(?, ?)" func-name)
                 (format nil "SELECT ~A(?)" func-name))))
    (cond
      ((and arg1 arg2)
       (let ((a1 (if (vectorp arg1) (float-vector-to-blob (coerce arg1 '(simple-array single-float (*)))) arg1))
             (a2 (if (vectorp arg2) (float-vector-to-blob (coerce arg2 '(simple-array single-float (*)))) arg2)))
         (execute-single db sql a1 a2)))
      (arg1
       (let ((a1 (if (vectorp arg1) (float-vector-to-blob (coerce arg1 '(simple-array single-float (*)))) arg1)))
         (execute-single db sql a1)))
      (t (error "At least one argument required")))))

(defun vec-add (db vec1 vec2)
  "Returns the element-wise addition of two vectors."
  (blob-to-float-vector (wrap-scalar-vec-func db "vec_add" vec1 vec2)))

(defun vec-sub (db vec1 vec2)
  "Returns the element-wise subtraction of two vectors."
  (blob-to-float-vector (wrap-scalar-vec-func db "vec_sub" vec1 vec2)))

(defun vec-distance-L2 (db vec1 vec2)
  "Calculates L2 distance between two vectors."
  (wrap-scalar-vec-func db "vec_distance_L2" vec1 vec2))

(defun vec-distance-cosine (db vec1 vec2)
  "Calculates Cosine distance between two vectors."
  (wrap-scalar-vec-func db "vec_distance_cosine" vec1 vec2))

(defun vec-distance-hamming (db vec1 vec2)
  "Calculates Hamming distance between two vectors."
  (wrap-scalar-vec-func db "vec_distance_hamming" vec1 vec2))

(defun vec-normalize (db vec)
  "Returns the normalized vector."
  (blob-to-float-vector (wrap-scalar-vec-func db "vec_normalize" vec)))

(defun vec-length (db vec)
  "Returns the length (dimension) of the vector."
  (wrap-scalar-vec-func db "vec_length" vec))

(defun vec-to-json (db vec)
  "Returns the JSON string representation of the vector."
  (wrap-scalar-vec-func db "vec_to_json" vec))

(defun vec-slice (db vec start end)
  "Returns a slice of the vector."
  (let ((v (if (vectorp vec) (float-vector-to-blob (coerce vec '(simple-array single-float (*)))) vec))
        (sql "SELECT vec_slice(?, ?, ?)"))
    (blob-to-float-vector (execute-single db sql v start end))))
