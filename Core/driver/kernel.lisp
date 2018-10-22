(in-package :cldk-internals)

;;;
;;; kernel mode
;;;

(defvar *kernel-mode* nil)

(defun check-kernel-mode ()
  (unless *kernel-mode*
    (error "a thread not in kernel mode is calling a kernel function")))

(defun check-user-mode ()
  (when *kernel-mode*
    (error "a thread in kernel mode is calling a non kernel function")))

;;;
;;; kernel
;;;

(defclass kerneled-driver-mixin (driver)
  ())
  
(defgeneric kernel-call (driver continuation block-p)
  (:method :around ((driver kerneled-driver-mixin) continuation block-p)
           (declare (ignore continuation block-p))
           (when (driver-running-p driver)
             (call-next-method)))
  (:method ((driver kerneled-driver-mixin) continuation block-p)
    (declare (ignore driver block-p))
    (let ((*kernel-mode* t))
      (funcall continuation))))

(defgeneric kernel-callback (driver continuation block-p)
  (:method :around ((driver kerneled-driver-mixin) continuation block-p)
           (declare (ignore continuation block-p))
           (when (driver-running-p driver)
             (call-next-method)))
  (:method ((driver kerneled-driver-mixin) continuation block-p)
    (declare (ignore driver block-p))
    (let ((*kernel-mode* nil))
      (funcall continuation))))

(defmacro within-kernel-mode ((driver &key (block-p t)) &body body)
  (let ((fn (gensym "CONT.")))
    `(labels ((,fn ()
               ,@body))
      (invoke-within-kernel-mode ,driver #',fn ,block-p))))

(defgeneric invoke-within-kernel-mode (driver continuation block-p))

(defmethod invoke-within-kernel-mode ((driver kerneled-driver-mixin) continuation block-p)
  (if *kernel-mode*
      (when (driver-running-p driver)
        (funcall continuation))
      (kernel-call driver continuation block-p)))

(defmacro within-user-mode ((driver &key (block-p t)) &body body)
  (let ((fn (gensym "CONT.")))
    `(labels ((,fn ()
               ,@body))
       (invoke-within-user-mode ,driver #',fn ,block-p))))

(defgeneric invoke-within-user-mode (driver continuation block-p))

(defmethod invoke-within-user-mode ((driver kerneled-driver-mixin) continuation block-p)
  (if *kernel-mode*
      (kernel-callback driver continuation block-p)
      (when (driver-running-p driver)
        (funcall continuation))))

(defmethod driver-loop-fn :around ((driver kerneled-driver-mixin))
  (let ((*kernel-mode* t))
    (call-next-method)))

(defmethod destroy-driver ((driver kerneled-driver-mixin))
  (within-kernel-mode (driver :block-p t)
    (driver-stop driver)
    (sleep 0.1)))
  
(defmethod driver-start :around ((driver kerneled-driver-mixin))
  (check-kernel-mode)
  (call-next-method))
(defmethod driver-stop :around ((driver kerneled-driver-mixin))
  (check-kernel-mode)
  (call-next-method))
(defmethod driver-kill :around ((driver kerneled-driver-mixin))
  (check-kernel-mode)
  (call-next-method))
(defmethod driver-ping :around ((driver kerneled-driver-mixin))
  (check-kernel-mode)
  (call-next-method))
(defmethod driver-force-output :around ((driver kerneled-driver-mixin))
  (check-kernel-mode)
  (call-next-method))
(defmethod driver-process-next-event :around ((driver kerneled-driver-mixin))
  (check-kernel-mode)
  (call-next-method))

;;;
;;; queue 
;;;

(defclass queued-kerneled-driver-mixin (kerneled-driver-mixin)
  ())

(defgeneric %next-kernel-queued-continuation (driver queue &key block-p kernel-mode-p))
(defgeneric %inqueue-kernel-continuation (driver queue continuation &key block-p))
(defgeneric %empty-kernel-queue (driver queue))

(defclass call-queued-kerneled-driver-mixin (queued-kerneled-driver-mixin)
  ((call-queue :reader kernel-call-queue)))

(defmethod kernel-call ((driver call-queued-kerneled-driver-mixin) continuation block-p)
  (%inqueue-kernel-continuation driver (kernel-call-queue driver) continuation :block-p block-p))

(defgeneric next-kernel-call (driver block-p)
  (:method :around ((driver call-queued-kerneled-driver-mixin) block-p)
           (declare (ignore block-p))
           (if (driver-running-p driver)
               (call-next-method)
               :disabled))
  (:method ((driver call-queued-kerneled-driver-mixin) block-p)
    (%next-kernel-queued-continuation driver (kernel-call-queue driver)
                                      :block-p block-p :kernel-mode-p t)))

(defgeneric process-next-kernel-calls (driver &key maxtime)
  (:method ((driver call-queued-kerneled-driver-mixin) &key (maxtime 0.03))
    (let ((end-time (+ (get-internal-real-time) (* maxtime internal-time-units-per-second)))
          (count 0))
      (loop with res = nil do
           (setq res (next-kernel-call driver nil))
           (setq count (1+ count))
         while (and res
                    (< (get-internal-real-time) end-time)))
      (when (> (get-internal-real-time) end-time)
        (log:info "next calls time exceded after ~A steps" count)))))

(defmethod driver-stop :after ((driver call-queued-kerneled-driver-mixin))
  (%empty-kernel-queue driver (kernel-call-queue driver))
  (%inqueue-kernel-continuation driver (kernel-call-queue driver) #'(lambda ())))

(defmethod driver-kill :after ((driver call-queued-kerneled-driver-mixin))
  (%empty-kernel-queue driver (kernel-callback-queue driver))
  (%inqueue-kernel-continuation driver (kernel-call-queue driver) #'(lambda ())))

(defclass callback-queued-kerneled-driver-mixin (queued-kerneled-driver-mixin)
  ((callback-queue :reader kernel-callback-queue)))

(defmethod kernel-callback ((driver callback-queued-kerneled-driver-mixin) continuation block-p)
  (%inqueue-kernel-continuation driver (kernel-callback-queue driver) continuation :block-p block-p))

(defgeneric next-kernel-callback (driver block-p)
  (:method :around ((driver callback-queued-kerneled-driver-mixin) block-p)
           (declare (ignore block-p))
           (if (or (eql (driver-status driver) :starting)
                   (driver-running-p driver))
               (call-next-method)
               :disabled))
  (:method ((driver callback-queued-kerneled-driver-mixin) block-p)
    (%next-kernel-queued-continuation driver (kernel-callback-queue driver)
                                      :block-p block-p :kernel-mode-p nil)))

(defgeneric process-next-kernel-callback-loop (driver)
  (:method ((driver callback-queued-kerneled-driver-mixin))
    (loop with res = nil do
         (setq res (next-kernel-callback driver t))
       while (not (eql res :disabled)))))

(defmethod driver-stop :after ((driver callback-queued-kerneled-driver-mixin))
  (%empty-kernel-queue driver (kernel-callback-queue driver))
  (%inqueue-kernel-continuation driver (kernel-callback-queue driver) #'(lambda ())))

(defmethod driver-kill :after ((driver callback-queued-kerneled-driver-mixin))
  (%empty-kernel-queue driver (kernel-callback-queue driver))
  (%inqueue-kernel-continuation driver (kernel-callback-queue driver) #'(lambda ())))

;;;
;;;
;;;

(defclass callback-queued-kerneled-driver-with-thread-mixin
    (callback-queued-kerneled-driver-mixin driver-with-thread-mixin)
  ((callback-thread :initform nil
                    :reader driver-callback-thread)))

(defmethod start-driver :after ((driver callback-queued-kerneled-driver-with-thread-mixin))
  (with-slots (callback-thread) driver
    (setf callback-thread (bt:make-thread #'(lambda ()
                                              (callback-loop-fn driver))
                                          :name (format nil "cldk ~A callback driver"
                                                        (driver-id driver))))))

(defmethod destroy-driver :after ((driver callback-queued-kerneled-driver-with-thread-mixin))
  (with-slots (callback-thread) driver
    (when (and callback-thread
               (bt:thread-alive-p callback-thread))
      (log:warn "destroy cldk ~A callback thread" (driver-id driver))
      (bt:destroy-thread callback-thread)
      (setf callback-thread nil))))

(defgeneric callback-loop (driver)
  (:method ((driver callback-queued-kerneled-driver-with-thread-mixin))
    (process-next-kernel-callback-loop driver)))

(defun callback-loop-fn (driver)
  (block loop
    (loop
       (with-simple-restart
           (restart-callback-loop
            "restart cldk ~A callback loop."  (driver-id driver))
         (callback-loop driver)
         (return-from loop)))))
