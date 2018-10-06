(in-package :cldk-internals)

;;;
;;; driver
;;;

(defclass server-driver-mixin ()
  ())

(defgeneric driver-start (driver))
(defgeneric driver-stop (driver))
(defgeneric driver-kill (driver)
  (:method ((driver server-driver-mixin))
    t))
(defgeneric driver-ping (driver)
  (:method ((driver server-driver-mixin))
    t))
(defgeneric driver-force-output (driver))
(defgeneric driver-process-next-event (driver))

;;;
;;; driver object
;;;

(defclass driver-object-mixin ()
  ())

(defgeneric driver-object-id (object)
  (:method ((object driver-object-mixin))
    object))
