(in-package :cldk-driver)

;;;
;;; threaded driver
;;;

(defclass threaded-driver-mixin (driver)
  ((lock :initform (bt:make-recursive-lock "driver lock")
         :accessor driver-lock)))

(defmacro with-driver-locked ((driver) &body body)
  (let ((fn (gensym "CONT.")))
    `(labels ((,fn ()
               ,@body))
      (declare (dynamic-extent #',fn))
      (invoke-with-driver-locked ,driver #',fn))))

(defgeneric invoke-with-driver-locked (driver continuation))

(defmethod invoke-with-driver-locked ((driver threaded-driver-mixin) continuation)
  (bt:with-recursive-lock-held ((driver-lock driver))
    (funcall continuation)))

;;;
;;; single threaded driver
;;;

(defclass single-threaded-driver-mixin (threaded-driver-mixin)
  ())

;;;
;;; multi threaded driver
;;;

(defclass multi-threaded-driver-mixin (threaded-driver-mixin)
  ())

(defmethod register-driver-object :around ((driver multi-threaded-driver-mixin) driver-object)
  (with-driver-locked (driver)
    (call-next-method)))

(defmethod unregister-driver-object :around ((driver multi-threaded-driver-mixin) driver-object)
  (with-driver-locked (driver)
    (call-next-method)))
