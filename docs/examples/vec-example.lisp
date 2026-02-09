(in-package :cl-user)

(defpackage :inquisitio-vec-example
  (:use :cl :inquisitio))

(in-package :inquisitio-vec-example)

(defun run-examples ()
  (format t "~&Starting inquisitio vec examples...~%")

  (with-open-database (db ":memory:")
    ;; Load the extension
    (enable-load-extension db t)
    (let ((extension-path (merge-pathnames "libs/vec0.so" (uiop:getcwd))))
      (format t "~&Loading extension from: ~A~%" extension-path)
      (load-extension db (namestring extension-path) (cffi:null-pointer)))

    ;; 1. Meta functions
    (format t "~&~%--- Meta Functions ---~%")
    (format t "vec_version: ~A~%" (execute-single db "select vec_version()"))

    ;; 2. Constructors
    (format t "~&~%--- Constructors ---~%")
    (let ((vec (execute-single db "select vec_f32('[0.1, 0.2, 0.3, 0.4]')")))
      (format t "vec_f32 (blob length): ~A~%" (length vec)))

    (let* ((lisp-vec (make-array 4 :element-type 'single-float :initial-contents '(1.0 2.0 3.0 4.0)))
           (blob (inquisitio:float-vector-to-blob lisp-vec))
           (result-blob (execute-single db "select vec_f32(?)" blob)))
       (format t "vec_f32 from blob (result length): ~A~%" (length result-blob)))

    (let ((vec (execute-single db "select vec_int8('[1, 2, 3, 4]')")))
      (format t "vec_int8 (blob length): ~A~%" (length vec)))

    (let ((vec (execute-single db "select vec_bit(X'F0')")))
      (format t "vec_bit (blob length): ~A~%" (length vec)))

    ;; 3. Operations
    (format t "~&~%--- Operations ---~%")

    (format t "vec_length('[0.1, 0.2]'): ~A~%"
            (execute-single db "select vec_length('[0.1, 0.2]')"))

    (format t "vec_type('[0.1, 0.2]'): ~A~%"
            (execute-single db "select vec_type('[0.1, 0.2]')"))

    (format t "vec_add: ~A~%"
            (execute-single db "select vec_to_json(vec_add('[0.1, 0.2]', '[0.3, 0.4]'))"))

    (format t "vec_sub: ~A~%"
            (execute-single db "select vec_to_json(vec_sub('[0.5, 0.6]', '[0.1, 0.2]'))"))

    (format t "vec_normalize: ~A~%"
            (execute-single db "select vec_to_json(vec_normalize('[2, 0]'))"))

    (format t "vec_slice: ~A~%"
            (execute-single db "select vec_to_json(vec_slice('[1, 2, 3, 4]', 0, 2))"))

    (format t "vec_to_json: ~A~%"
            (execute-single db "select vec_to_json(vec_f32('[1.1, 2.2]'))"))

    ;; 4. Distance Functions
    (format t "~&~%--- Distance Functions ---~%")
    (format t "vec_distance_L2: ~A~%"
            (execute-single db "select vec_distance_L2('[1, 1]', '[2, 2]')"))

    (format t "vec_distance_cosine: ~A~%"
            (execute-single db "select vec_distance_cosine('[1, 0]', '[0, 1]')"))

    (format t "vec_distance_hamming: ~A~%"
            (execute-single db "select vec_distance_hamming(vec_bit(X'F0'), vec_bit(X'FF'))"))

    ;; 5. Quantization
    (format t "~&~%--- Quantization ---~%")
    (let ((blob (execute-single db "select vec_quantize_binary('[1, 2, 3, 4, -1, -2, -3, -4]')")))
      (format t "vec_quantize_binary result hex: ~A~%"
              (execute-single db "select hex(?)" blob)))

    ;; 6. vec_each
    (format t "~&~%--- vec_each ---~%")
    (format t "Iterating over '[1.1, 2.2, 3.3]':~%")
    (let ((results (execute-to-list db "select rowid, value from vec_each('[1.1, 2.2, 3.3]')")))
      (loop for row in results
            do (format t "  Row ~A: ~A~%" (first row) (second row))))

    ;; 7. Using with float-vector-to-blob/blob-to-float-vector helpers
    (format t "~&~%--- Helpers ---~%")
    (let* ((original-vec (make-array 3 :element-type 'single-float :initial-contents '(0.5 0.25 0.125)))
           (blob-in (inquisitio:float-vector-to-blob original-vec))
           (blob-out (execute-single db "select vec_add(?, ?)" blob-in blob-in))
           (result-vec (inquisitio:blob-to-float-vector blob-out)))
      (format t "Original: ~A~%" original-vec)
      (format t "Result (Original + Original): ~A~%" result-vec)))

  (format t "~&~%Done.~%"))
