(in-package :cldk-internals)

;;;
;;; threaded driver
;;;

(defclass threaded-driver-mixin (driver)
  ((dlock :initform (bt:make-recursive-lock "driver lock")
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

;;;
;;; driver with thread
;;;

(defclass driver-with-thread-mixin ()
  ((driver-thread :initform nil
                  :reader driver-thread)))

(defgeneric driver-start-thread (driver))
(defgeneric driver-destroy-thread (driver))
(defgeneric driver-loop-step (driver))
(defgeneric driver-loop (driver &key min-loop-time))
(defgeneric driver-loop-fn (driver))

(defmethod driver-start-thread ((driver driver-with-thread-mixin))
  (with-slots (driver-thread) driver
    (setf driver-thread
          (bt:make-thread #'(lambda ()
                              (driver-loop-fn driver))
                          :name (format nil "cldk ~A driver" (driver-id driver))))))

(defmethod driver-destroy-thread ((driver driver-with-thread-mixin))
  (with-slots (driver-thread) driver
    (when (and driver-thread
               (bt:thread-alive-p driver-thread))
      (log:warn "destroy cldk ~A thread" (driver-id driver))
      (bt:destroy-thread driver-thread))))

(defmethod driver-loop ((driver driver-with-thread-mixin)  &key (min-loop-time 0.01))
  (block loop
    (loop
       (let ((end-time (+ (get-internal-real-time) (* min-loop-time internal-time-units-per-second))))
         (driver-loop-step driver)
         (let ((wait-time (- end-time (get-internal-real-time))))
           (when (> wait-time 0)
             (sleep (/ wait-time internal-time-units-per-second)))))
       (when (or (driver-stopping-p driver)
                 (driver-stopped-p driver))
         (return-from loop)))))
  
(defmethod driver-loop-fn ((driver driver-with-thread-mixin))
  (driver-start driver)
  (block loop
    (loop
       (with-simple-restart
           (restart-driver-loop
            "restart cldk ~A driver loop." (driver-id driver))
         (driver-loop driver)
         (return-from loop)))))

(defmethod start-driver ((driver driver-with-thread-mixin))
  (driver-start-thread driver))

(defmethod destroy-driver :after ((driver driver-with-thread-mixin))
  (driver-destroy-thread driver))

