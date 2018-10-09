(in-package :cldk-internals)

;;;
;;; kernel
;;;

(defclass server-kernel (kerneled-driver-mixin)
  ())
#|
(defmacro <call+ (server fn &rest args)
  `(within-kernel-mode (,server :block-p t)
     (funcall ,fn ,@args)))

(defmacro <call- (server fn &rest args)
  `(within-kernel-mode (,server :block-p nil)
     (funcall ,fn ,@args)))
|#
#|
(defmacro <callback+ (server fn &rest args)
  `(within-user-mode (,server :block-p t)
     (funcall ,fn ,@args)))

(defmacro <callback- (server fn &rest args)
  `(within-user-mode (,server :block-p nil)
     (funcall ,fn ,@args)))
|#

;;;
;;; Core Functions
;;;

(defun k-start (kernel)
  (check-kernel-mode)
  (driver-start kernel))

(defun k-stop (kernel)
  (check-kernel-mode)
  (driver-stop kernel))

(defun k-kill (kernel)
  (check-kernel-mode)
  (driver-kill kernel))

(defun k-ping (kernel)
  (check-kernel-mode)
  (driver-ping kernel))

(defun k-force-output (kernel)
  (check-kernel-mode)
  (driver-force-output kernel))

(defun k-process-next-event (kernel)
   (check-kernel-mode)
   (driver-process-next-event kernel))

;;;
;;;
;;;

(defgeneric k-process-next-driver-events (kernel &key maxtime)
  (:method ((kernel server-kernel) &key (maxtime 0.01))
    (check-kernel-mode)
    (driver-process-next-events kernel :maxtime maxtime)))
