(in-package :cldk-internals)

(defclass display-driver (driver)
  ((default-screen-index :initform 0
     :initarg :screen
     :accessor driver-default-screen-index)))

(defclass display-driver-object (driver-object)
  ())

;;;
;;; API
;;;

;;; screen
(defgeneric driver-screen-num (driver))
(defgeneric driver-screen-size (driver screen-index units))
(defgeneric driver-screen-dpi (driver screen-index))
(defgeneric driver-screen-pointer-position (driver))

;;; window
(defclass driver-window (display-driver-object)
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

;;; cursor
(defclass driver-cursor (display-driver-object)
  ())

(defgeneric driver-avaiable-cursor-names (driver))
(defgeneric driver-create-cursor (driver named-cursor))
(defgeneric driver-destroy-cursor (driver cursor))
(defgeneric driver-set-window-cursor (driver window cursor))

;;; buffer
(defclass driver-buffer (display-driver-object)
  ())

(defgeneric driver-initialize-buffer (driver buffer width height))
(defgeneric driver-update-buffer (driver buffer width height))
(defgeneric driver-destroy-buffer (driver buffer))
(defgeneric driver-copy-buffer-to-window (driver buffer x y width height window to-x to-y))
