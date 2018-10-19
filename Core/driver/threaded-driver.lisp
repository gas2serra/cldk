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
  ((kernel-thread :initform nil
                  :reader driver-kernel-thread)))

(defgeneric driver-start-thread (driver))
(defmethod driver-start-thread ((driver driver-with-thread-mixin))
  (with-slots (kernel-thread) driver
    (setf kernel-thread
          (bt:make-thread #'(lambda ()
                              (driver-loop-fn driver))
                          :name (format nil "cldk driver ~A" (driver-id driver))))))

(defmethod driver-stop :after ((driver driver-with-thread-mixin))
  (bt:join-thread (driver-kernel-thread driver))
  (when (bt:thread-alive-p (driver-kernel-thread driver))
    (driver-kill driver)))

(defmethod driver-kill :after ((driver driver-with-thread-mixin))
  (with-slots (kernel-thread) driver
    (when kernel-thread
      (bt:destroy-thread kernel-thread)
      (setf kernel-thread nil))))

(defmethod driver-destroy :around ((driver driver-with-thread-mixin))
  (unwind-protect
       (call-next-method)
    (when (bt:thread-alive-p (driver-kernel-thread driver))
      (driver-kill driver))))

(defgeneric driver-loop-step (driver))

(defgeneric driver-loop (driver &key min-loop-time))

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
  
(defgeneric driver-loop-fn (driver))
(defmethod driver-loop-fn ((driver driver-with-thread-mixin))
  (driver-start driver)
  (block loop
    (loop
       (with-simple-restart
           (restart-driver-loop
            "restart cldk's driver loop.")
         (driver-loop driver)
         (return-from loop)))))
