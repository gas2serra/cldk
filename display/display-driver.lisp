(in-package :cldk-internals)

(defclass display-driver (driver)
  ((options :initform nil
            :initarg :options
            :reader driver-options)
   (default-screen-index :initform 0
     :initarg :screen
     :accessor driver-default-screen-index)
   (driver-object-id->server-object :initform (make-hash-table))))

;;; driver objects
(defclass driver-object ()
  ())

(defgeneric driver-object-id (object)
  (:method ((object driver-object))
    object))

(defun register-server-object (driver driver-object server-object)
  (with-slots (driver-object-id->server-object) driver
    (setf (gethash (driver-object-id driver-object) driver-object-id->server-object)
          server-object)))

(defun unregister-server-object (driver driver-object)
  (with-slots (driver-object-id->server-object) driver
    (setf (gethash (driver-object-id driver-object) driver-object-id->server-object)
          nil)))

(defun lookup-server-object (driver driver-object-id)
  (with-slots (driver-object-id->server-object) driver
    (gethash driver-object-id driver-object-id->server-object)))

;;;
;;; API
;;;

(defgeneric driver-start (driver))
(defgeneric driver-stop (driver))
(defgeneric driver-ping (driver)
  (:method ((driver display-driver))
    t))
(defgeneric driver-force-output (driver))
(defgeneric driver-process-next-event (driver kernel &key timeout))

;;; screen
(defgeneric driver-screen-num (driver))
(defgeneric driver-screen-size (driver screen-index units))
(defgeneric driver-screen-dpi (driver screen-index))
(defgeneric driver-screen-pointer-position (driver))

;;; window
(defclass driver-window (driver-object)
  ())

(defgeneric driver-create-window (driver name pretty-name x y
                                  width height mode))
(defgeneric driver-destroy-window (driver window))
(defgeneric driver-show-window (driver window))
(defgeneric driver-hide-window (driver window))
(defgeneric driver-window-position (driver window))
(defgeneric driver-window-size (driver window))
(defgeneric driver-set-window-position (driver window x y))
(defgeneric driver-set-window-size (driver window width height))
(defgeneric driver-set-window-hints (driver window x y width height
                                     max-width max-height
                                     min-width min-height))
(defgeneric driver-raise-window (driver window))
(defgeneric driver-bury-window (driver window))
(defgeneric driver-window-pointer-position (driver window))

(defgeneric driver-grab-pointer (driver window pointer))
(defgeneric driver-ungrab-pointer (driver window pointer))

(defclass driver-cursor (driver-object)
  ())

(defgeneric driver-avaiable-cursor-names (driver))
(defgeneric driver-create-cursor (driver named-cursor))
(defgeneric driver-destroy-cursor (driver cursor))
(defgeneric driver-set-window-cursor (driver window cursor))

;;; buffer
(defclass driver-buffer (driver-object)
  ())

(defgeneric driver-create-buffer (driver width height))
(defgeneric driver-destroy-buffer (driver buffer))
(defgeneric driver-copy-buffer-to-window (driver buffer x y width height window to-x to-y))
(defgeneric driver-copy-image-to-buffer (driver image x y width height buffer))

