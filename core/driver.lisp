(in-package :cldk-internals)

;;;
;;; driver
;;;

(defclass driver ()
  ())

(defgeneric driver-start (driver))
(defgeneric driver-stop (driver))
(defgeneric driver-kill (driver)
  (:method ((driver driver))
    t))

(defgeneric driver-ping (driver)
  (:method ((driver driver))
    t))

(defgeneric driver-force-output (driver))

(defgeneric driver-process-next-event (driver kernel))

;;;
;;; driver object
;;;

(defclass driver-object ()
  ())

(defgeneric driver-object-id (object)
  (:method ((object driver-object))
    object))
