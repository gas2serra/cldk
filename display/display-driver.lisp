(in-package :cldk-internals)

(defclass display-driver-mixin (server-driver-mixin)
  ((default-screen-index :initform 0
     :initarg :screen
     :accessor driver-default-screen-index)))

(defclass display-driver-object-mixin (driver-object-mixin)
  ())

;;;
;;; API
;;;

;;; screen
(defgeneric driver-screen-num (server))
(defgeneric driver-screen-size (server screen-index units))
(defgeneric driver-screen-dpi (server screen-index))
(defgeneric driver-screen-pointer-position (server))

;;; window
(defclass driver-window (display-driver-object-mixin)
  ())

(defgeneric driver-initialize-window (server window name pretty-name x y
                                      width height mode))
(defgeneric driver-destroy-window (server window))
(defgeneric driver-show-window (server window))
(defgeneric driver-hide-window (server window))
(defgeneric driver-window-position (server window))
(defgeneric driver-window-size (server window))
(defgeneric driver-set-window-position (server window x y))
(defgeneric driver-set-window-size (server window width height))
(defgeneric driver-set-window-hints (server window x y width height
                                     max-width max-height
                                     min-width min-height))
(defgeneric driver-raise-window (server window))
(defgeneric driver-bury-window (server window))
(defgeneric driver-window-pointer-position (server window))

(defgeneric driver-grab-pointer (server window pointer))
(defgeneric driver-ungrab-pointer (server window pointer))

;;; cursor
(defclass driver-cursor (display-driver-object-mixin)
  ())

(defgeneric driver-avaiable-cursor-names (server))
(defgeneric driver-create-cursor (server named-cursor))
(defgeneric driver-destroy-cursor (server cursor))
(defgeneric driver-set-window-cursor (server window cursor))

;;; buffer
(defclass driver-buffer (display-driver-object-mixin)
  ())

(defgeneric driver-initialize-buffer (server buffer width height))
(defgeneric driver-update-buffer (server buffer width height))
(defgeneric driver-destroy-buffer (server buffer))
(defgeneric driver-copy-buffer-to-window (server buffer x y width height window to-x to-y))
