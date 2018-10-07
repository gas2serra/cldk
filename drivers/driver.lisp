(in-package :cldk-driver)

;;;
;;; driver
;;;

(defclass driver ()
  ((options :accessor driver-options)
   (driver-object-id->driver-object :initform (make-hash-table))
   (callback-handler :accessor driver-callback-handler :initarg :callback-handler)))

(defgeneric register-driver-object (driver driver-object)
  (:method ((driver driver) driver-object)
    (with-slots (driver-object-id->driver-object) driver
      (setf (gethash (driver-object-id driver-object) driver-object-id->driver-object)
            driver-object))))

(defgeneric unregister-driver-object (driver driver-object)
  (:method ((driver driver) driver-object)
    (with-slots (driver-object-id->driver-object) driver
      (setf (gethash (driver-object-id driver-object) driver-object-id->driver-object)
            nil))))

(defgeneric lookup-driver-object (driver driver-object-id)
  (:method ((driver driver) driver-object-id)
    (with-slots (driver-object-id->driver-object) driver
      (gethash driver-object-id driver-object-id->driver-object))))

(defgeneric driver-start (driver))
(defgeneric driver-stop (driver))
(defgeneric driver-kill (driver)
  (:method ((driver driver))
    t))
(defgeneric driver-ping (driver)
  (:method ((driver driver))
    t))
(defgeneric driver-force-output (driver))
(defgeneric driver-process-next-event (driver))

;;;
;;; callback handler
;;;

(defclass driver-callback-handler ()
  ())

;;;
;;; object
;;;

(defclass driver-object ()
  ())

(defgeneric driver-object-id (object)
  (:method ((object driver-object))
    object))
