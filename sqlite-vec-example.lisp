(in-package :cl-user)

(require :sqlite)

(defpackage :sqlite-vec-example
  (:use :cl :sqlite))

(in-package :sqlite-vec-example)

(defun run-examples ()
  (format t "~&Starting sqlite-vec examples...~%")

  (with-open-database (db ":memory:")
    ;; Load the extension
    (enable-load-extension db t)
    ;; Try to load vec0.so from libs/ directory relative to current working directory
    (let ((extension-path (merge-pathnames "libs/vec0.so" (uiop:getcwd))))
      (format t "~&Loading extension from: ~A~%" extension-path)
      ;; Pass explicit NULL pointer for entry-point, not NIL.
      ;; The issue is that load-extension calls sqlite3-load-extension which expects a string for entry-point.
      ;; If we pass NIL, CFFI complains it's not a string or pointer.
      ;; Checking load-extension implementation: it takes optional entry-point.
      ;; If entry-point is NIL, we should probably pass a null pointer to CFFI or let CFFI handle it.
      ;; But in sqlite.lisp: (sqlite-ffi:sqlite3-load-extension (handle db) path entry-point errmsg-ptr)
      ;; And in sqlite-ffi.lisp: (defcfun sqlite3-load-extension ... (proc :string) ...)
      ;; :string type in CFFI expects a Lisp string or a pointer. NIL is not valid unless :string+ptr is used or similar?
      ;; Actually usually :string handles NIL as NULL. But apparently not here or specific CFFI version.
      ;; Wait, I can't easily change sqlite.lisp yet. I should try to pass a null-pointer explicitly?
      ;; No, entry-point argument in load-extension is passed directly to sqlite3-load-extension.
      ;; If I pass (cffi:null-pointer), it might work if :string accepts pointers.
      (load-extension db (namestring extension-path) (cffi:null-pointer)))

    ;; 1. Meta functions
    (format t "~&~%--- Meta Functions ---~%")
    (format t "vec_version: ~A~%" (execute-single db "select vec_version()"))
    ;; vec_debug might return multiple lines or complex info
    ;(format t "vec_debug: ~A~%" (execute-single db "select vec_debug()"))

    ;; 2. Constructors
    (format t "~&~%--- Constructors ---~%")
    ;; vec_f32 from JSON
    (let ((vec (execute-single db "select vec_f32('[0.1, 0.2, 0.3, 0.4]')")))
      (format t "vec_f32 (blob length): ~A~%" (length vec)))

    ;; vec_f32 from BLOB using float-vector-to-blob helper
    (let* ((lisp-vec (make-array 4 :element-type 'single-float :initial-contents '(1.0 2.0 3.0 4.0)))
           (blob (sqlite:float-vector-to-blob lisp-vec))
           (result-blob (execute-single db "select vec_f32(?)" blob)))
       (format t "vec_f32 from blob (result length): ~A~%" (length result-blob)))

    ;; vec_int8
    (let ((vec (execute-single db "select vec_int8('[1, 2, 3, 4]')")))
      (format t "vec_int8 (blob length): ~A~%" (length vec)))

    ;; vec_bit
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

    ;; vec_slice (start, end)
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
           (blob-in (sqlite:float-vector-to-blob original-vec))
           (blob-out (execute-single db "select vec_add(?, ?)" blob-in blob-in))
           (result-vec (sqlite:blob-to-float-vector blob-out)))
      (format t "Original: ~A~%" original-vec)
      (format t "Result (Original + Original): ~A~%" result-vec))

  )
  (format t "~&~%Done.~%"))

(run-examples)
