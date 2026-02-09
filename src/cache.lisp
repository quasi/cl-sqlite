(defpackage :inquisitio.cache
  (:nicknames :sqlite.cache)
  (:use :cl :iter)
  (:export :mru-cache
           :get-from-cache
           :put-to-cache
           :purge-cache))

(in-package :inquisitio.cache)

(telos:defclass/i mru-cache ()
  ((objects-table :accessor objects-table :initform (make-hash-table :test 'equal))
   (last-access-time-table :accessor last-access-time-table :initform (make-hash-table :test 'equal))
   (total-cached :type fixnum :accessor total-cached :initform 0)
   (cache-size :type fixnum :accessor cache-size :initarg :cache-size :initform 100)
   (destructor :accessor destructor :initarg :destructor :initform #'identity))
  (:documentation "Most-recently-used cache for prepared statements.
Evicts the least-recently-used entry when the cache is full.")
  (:feature inquisitio-cache)
  (:purpose "Cache prepared statements keyed by SQL text to avoid repeated compilation"))

(defun get-from-cache (cache id)
  "Retrieve and remove the most recently cached object for ID.
Returns the object, or NIL if nothing is cached for ID."
  (let ((available-objects-stack (gethash id (objects-table cache))))
    (when (and available-objects-stack (> (length available-objects-stack) 0))
      (decf (total-cached cache))
      (setf (gethash id (last-access-time-table cache)) (get-internal-run-time))
      (vector-pop available-objects-stack))))

(defun remove-empty-objects-stacks (cache)
  (let ((table (objects-table cache)))
    (maphash (lambda (key value)
               (when (zerop (length value))
                 (remhash key table)
                 (remhash key (last-access-time-table cache))))
             table)))

(defun pop-from-cache (cache)
  (let ((id (iter (for (id time) in-hashtable (last-access-time-table cache))
                  (when (not (zerop (length (gethash id (objects-table cache)))))
                    (finding id minimizing time)))))
    (let ((object (vector-pop (gethash id (objects-table cache)))))
      (funcall (destructor cache) object)))
  (remove-empty-objects-stacks cache)
  (decf (total-cached cache)))

(defun put-to-cache (cache id object)
  "Store OBJECT in the cache under ID.
Evicts the least-recently-used entry if the cache is full.
Returns OBJECT."
  (when (>= (total-cached cache) (cache-size cache))
    (pop-from-cache cache))
  (let ((available-objects-stack (or (gethash id (objects-table cache))
                                     (setf (gethash id (objects-table cache))
                                           (make-array 0 :adjustable t :fill-pointer t)))))
    (vector-push-extend object available-objects-stack)
    (setf (gethash id (last-access-time-table cache)) (get-internal-run-time))
    (incf (total-cached cache))
    object))

(defun purge-cache (cache)
  "Destroy all cached objects by calling the destructor on each."
  (iter (for (id items) in-hashtable (objects-table cache))
        (declare (ignorable id))
        (when items
          (iter (for item in-vector items)
                (funcall (destructor cache) item)))))
