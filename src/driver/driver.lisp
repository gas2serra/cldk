(in-package :cldk-internals)

;;;
;;; driver
;;;

(defclass driver ()
  ((options :accessor driver-options)
   (driver-object-id->driver-object :initform (make-hash-table))
   (callback-handler :accessor driver-callback-handler :initarg :callback-handler)))

(defgeneric driver-id (driver)
  (:method ((driver driver))
    (getf (driver-options driver) :id :null)))

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

(defgeneric driver-process-next-events (driver &key maxtime)
  (:method ((driver driver) &key (maxtime 0.03))
    (let ((end-time (+ (get-internal-real-time) (* maxtime internal-time-units-per-second))))
      (loop with event-p = nil do
           (setq event-p (driver-process-next-event driver))
         while (and event-p
                    (< (get-internal-real-time) end-time)))
      (when (> (get-internal-real-time) end-time)
        (log:info "event time exceded")))))
  
;;;
;;; callback handler
;;;

(defclass driver-callback-handler ()
  ())

;;;
;;; object
;;;

(defclass driver-object ()
  ((driver :initform nil
           :initarg :driver
           :reader driver)))

(defgeneric driver-object-id (object)
  (:method ((object driver-object))
    object))
