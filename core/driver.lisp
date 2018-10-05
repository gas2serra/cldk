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
;;; driver
;;;

(defclass server-driver-mixin ()
  ())

(defgeneric driver-start (server)
  (:method :before ((server server-driver-mixin))
           (check-kernel-mode)))
(defgeneric driver-stop (server)
  (:method :before ((server server-driver-mixin))
           (check-kernel-mode)))
(defgeneric driver-kill (server)
  (:method :before ((server server-driver-mixin))
           (check-kernel-mode))
  (:method ((server server-driver-mixin))
    t))
(defgeneric driver-ping (server)
  (:method :before ((server server-driver-mixin))
           (check-kernel-mode))
  (:method ((server server-driver-mixin))
    t))
(defgeneric driver-force-output (server)
  (:method :before ((server server-driver-mixin))
           (check-kernel-mode)))
(defgeneric driver-process-next-event (server)
  (:method :before ((server server-driver-mixin))
           (check-kernel-mode)))

;;;
;;; driver object
;;;

(defclass driver-object-mixin ()
  ())

(defgeneric driver-object-id (object)
  (:method ((object driver-object-mixin))
    (check-kernel-mode)
    object))

