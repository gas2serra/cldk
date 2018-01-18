(in-package :cldk-internals)

;;;
;;; driver
;;;

(defclass server-driver-mixin ()
  ())

(defgeneric driver-start (server))
(defgeneric driver-stop (server))
(defgeneric driver-kill (server)
  (:method ((server server-driver-mixin))
    t))

(defgeneric driver-ping (server)
  (:method ((server server-driver-mixin))
    t))

(defgeneric driver-force-output (server))

(defgeneric driver-process-next-event (server))

;;;
;;; driver object
;;;

(defclass driver-object-mixin ()
  ())

(defgeneric driver-object-id (object)
  (:method ((object driver-object-mixin))
    object))
