(in-package :cldk-internals)

;;;
;;; driver
;;;

(defclass driver ()
  ((status :initform :stopped
           :type (member :starting :running :stopping :stopped)
           :reader driver-status)
   (options :accessor driver-options)
   (driver-object-id->driver-object :initform (make-hash-table))
   (callback-handler :accessor driver-callback-handler
                     :initarg :callback-handler)))

(defun driver-running-p (driver)
  (eql (driver-status driver) :running))

(defun driver-stopped-p (driver)
  (eql (driver-status driver) :stopped))

(defun driver-stopping-p (driver)
  (eql (driver-status driver) :stopping))

(defgeneric driver-id (driver)
  (:method ((driver driver))
    (getf (driver-options driver) :id :null)))

(defgeneric register-driver-object (driver driver-object))
(defgeneric unregister-driver-object (driver driver-object))
(defgeneric lookup-driver-object (driver driver-object-id))

(defmethod lookup-driver-object ((driver driver) driver-object-id)
  (with-slots (driver-object-id->driver-object) driver
    (gethash driver-object-id driver-object-id->driver-object)))

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
    (let ((end-time (+ (get-internal-real-time)
                       (* maxtime internal-time-units-per-second))))
      (loop with event-p = nil do
           (setq event-p (driver-process-next-event driver))
         while (and event-p
                    (< (get-internal-real-time) end-time)))
      (when (> (get-internal-real-time) end-time)
        (log:info "event time exceded")))))


(defmethod driver-start :around ((driver driver))
  (if (driver-running-p driver)
      (error "cldk driver is already running")
      (call-next-method)))
(defmethod driver-start :before ((driver driver))
  (with-slots (status) driver
    (setf status :starting)))
(defmethod driver-start :after ((driver driver))
  (with-slots (status) driver
    (setf status :running)))

(defmethod driver-stop :around ((driver driver))
  (if (not (driver-running-p driver))
      (error "cldk driver is not running")
      (call-next-method)))
(defmethod driver-top :before ((driver driver))
  (with-slots (status) driver
    (setf status :stopping)))
(defmethod driver-stop :after ((driver driver))
  (with-slots (status) driver
    (setf status :stopped)))
(defmethod driver-kill :around ((driver driver))
  (if (driver-stopped-p driver)
      (error "cldk driver is already stopped")
      (progn
        (log:warn "killing cldk driver")
        (call-next-method))))

(defmethod driver-restart ((driver driver))
  (driver-stop driver)
  (driver-start driver))

(defmethod driver-destroy ((driver driver))
  (when (driver-running-p driver)
    (driver-stop driver)))

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

(defmethod register-driver-object ((driver driver) (driver-object driver-object))
  (with-slots (driver-object-id->driver-object) driver
    (setf (gethash (driver-object-id driver-object)
                   driver-object-id->driver-object)
          driver-object)))
(defmethod unregister-driver-object ((driver driver) (driver-object  driver-object))
  (with-slots (driver-object-id->driver-object) driver
    (setf (gethash (driver-object-id driver-object)
                   driver-object-id->driver-object)
          nil)))
