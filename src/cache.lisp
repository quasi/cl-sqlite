(defpackage :inquisitio.cache
  (:nicknames :sqlite.cache)
  (:use :cl :iter)
  (:export :mru-cache
           :cache-lock
           :get-from-cache
           :put-to-cache
           :purge-cache))

(in-package :inquisitio.cache)

(telos:defclass/i mru-cache ()
  ((objects-table :accessor objects-table :initform (make-hash-table :test 'equal))
   (last-access-time-table :accessor last-access-time-table :initform (make-hash-table :test 'equal))
   (total-cached :type fixnum :accessor total-cached :initform 0)
   (cache-size :type fixnum :accessor cache-size :initarg :cache-size :initform 100)
   (destructor :accessor destructor :initarg :destructor :initform #'identity)
   (lock :reader cache-lock :initarg :lock
         :initform (bt:make-recursive-lock "inquisitio-cache")))
  (:documentation "Most-recently-used cache for prepared statements.
Evicts the least-recently-used entry when the cache is full.

Every operation is serialized by CACHE-LOCK, a recursive lock. The owner may
supply its own lock via the :LOCK initarg so that the cache and the owner's
other state are covered by a single lock; the destructor is called with the
lock held, so a destructor that touches the owner's state cannot deadlock
against it.")
  (:feature inquisitio-cache)
  (:purpose "Cache prepared statements keyed by SQL text to avoid repeated compilation"))

(defmacro with-cache-lock ((cache) &body body)
  `(bt:with-recursive-lock-held ((cache-lock ,cache))
     ,@body))

(defun get-from-cache (cache id)
  "Retrieve and remove the most recently cached object for ID.
Returns the object, or NIL if nothing is cached for ID."
  (with-cache-lock (cache)
    (let ((available-objects-stack (gethash id (objects-table cache))))
      (when (and available-objects-stack (> (length available-objects-stack) 0))
        (decf (total-cached cache))
        (setf (gethash id (last-access-time-table cache)) (get-internal-run-time))
        (vector-pop available-objects-stack)))))

(defun remove-empty-objects-stacks (cache)
  (let ((table (objects-table cache)))
    (maphash (lambda (key value)
               (when (zerop (length value))
                 (remhash key table)
                 (remhash key (last-access-time-table cache))))
             table)))

(defun pop-from-cache (cache)
  (with-cache-lock (cache)
    (let ((id (iter (for (id time) in-hashtable (last-access-time-table cache))
                    (when (not (zerop (length (gethash id (objects-table cache)))))
                      (finding id minimizing time)))))
      (let ((object (vector-pop (gethash id (objects-table cache)))))
        (funcall (destructor cache) object)))
    (remove-empty-objects-stacks cache)
    (decf (total-cached cache))))

(defun put-to-cache (cache id object)
  "Store OBJECT in the cache under ID.
Evicts the least-recently-used entry if the cache is full.
Returns OBJECT."
  (with-cache-lock (cache)
    (when (>= (total-cached cache) (cache-size cache))
      (pop-from-cache cache))
    (let ((available-objects-stack (or (gethash id (objects-table cache))
                                       (setf (gethash id (objects-table cache))
                                             (make-array 0 :adjustable t :fill-pointer t)))))
      (vector-push-extend object available-objects-stack)
      (setf (gethash id (last-access-time-table cache)) (get-internal-run-time))
      (incf (total-cached cache))
      object)))

(defun purge-cache (cache)
  "Destroy all cached objects by calling the destructor on each, and empty the cache."
  (with-cache-lock (cache)
    (iter (for (id items) in-hashtable (objects-table cache))
          (declare (ignorable id))
          (when items
            (iter (for item in-vector items)
                  (funcall (destructor cache) item))))
    ;; Emptying is what makes PURGE-CACHE idempotent: without it a second purge
    ;; would run the destructor over already-destroyed objects.
    (clrhash (objects-table cache))
    (clrhash (last-access-time-table cache))
    (setf (total-cached cache) 0)))
