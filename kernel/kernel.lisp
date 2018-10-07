(in-package :cldk-kernel)

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
  (:method ((driver kerneled-driver-mixin) continuation block-p)
    (declare (ignore block-p))
    (let ((*kernel-mode* t))
      (unless (eq continuation :stop)
        (funcall continuation)))))

(defgeneric kernel-callback (driver continuation block-p)
  (:method ((driver kerneled-driver-mixin) continuation block-p)
    (declare (ignore block-p))
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
      (funcall continuation)
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
      (funcall continuation)))
