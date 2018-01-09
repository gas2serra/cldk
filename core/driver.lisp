(in-package :cldk-internals)

;;;
;;; driver
;;;

(defclass driver ()
  ((options :initform nil
            :initarg :options
            :reader driver-options)))

(defgeneric driver-start (driver))
(defgeneric driver-stop (driver))
(defgeneric driver-kill (driver)
  (:method ((driver driver))
    t))

(defgeneric driver-ping (driver)
  (:method ((driver driver))
    t))

(defgeneric driver-force-output (driver))

;;;
;;; driver object
;;;

(defclass driver-object ()
  ())

(defgeneric driver-object-id (object)
  (:method ((object driver-object))
    object))
