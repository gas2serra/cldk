(in-package :cldk-internals)

(defclass display-driver (driver)
  ((default-screen-index :initform 0
     :initarg :screen
     :accessor driver-default-screen-index)))

(defclass display-driver-object (driver-object)
  ())

(defclass display-driver-callback-handler (driver-callback-handler)
  ())

;;;
;;; driver API
;;;

;;; screen
(defgeneric driver-screen-num (driver))
(defgeneric driver-screen-size (driver screen-index units))
(defgeneric driver-screen-dpi (driver screen-index))
(defgeneric driver-screen-pointer-position (driver))

;;; window
(defclass driver-window (display-driver-object)
  ())

(defgeneric driver-initialize-window (driver window name pretty-name
                                      x y width height mode)
  (:method :after ((driver display-driver) window name pretty-name
                   x y width height mode)
           (register-driver-object driver window)))
(defgeneric driver-destroy-window (driver window)
  (:method :before ((driver display-driver) window)
           (unregister-driver-object driver window)))
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

(defgeneric driver-initialize-buffer (driver buffer width height)
  (:method :after ((driver display-driver) buffer width height)
           (register-driver-object driver buffer)))
(defgeneric driver-update-buffer (driver buffer width height))
(defgeneric driver-destroy-buffer (driver buffer)
  (:method :before ((driver display-driver) buffer)
           (unregister-driver-object driver buffer)))
(defgeneric driver-copy-buffer-to-window (driver buffer x y width height
                                          window to-x to-y))
(defgeneric driver-buffer-rgb-get-fn (driver buffer dx dy))
(defgeneric driver-buffer-rgb-set-fn (driver buffer dx dy))

;;;
;;; driver callback
;;;

;;; Constants dealing with events
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +pointer-left-button+   #x01)
  (defconstant +pointer-middle-button+ #x02)
  (defconstant +pointer-right-button+  #x04)
  (defconstant +pointer-x1-button+     #x08)
  (defconstant +pointer-x2-button+     #x10)
  
  (defconstant +shift-key+             #x0100)
  (defconstant +control-key+           #x0200)
  (defconstant +meta-key+              #x0400)
  (defconstant +super-key+             #x0800)
  (defconstant +hyper-key+             #x1000)
  (defconstant +alt-key+               #x2000))

(defgeneric driver-cb-window-configuration-event (handler driver window
                                                  x y width height time))
(defgeneric driver-cb-repaint-event (handler driver window
                                     x y width height time))
(defgeneric driver-cb-scroll-event (handler driver pointer
                                    dx dy window timestamp))
(defgeneric driver-cb-button-event (handler driver kind pointer
                                    button window timestamp))
(defgeneric driver-cb-motion-event (handler driver pointer
                                    x y root-x root-y window timestamp))
(defgeneric driver-cb-key-event (handler driver kind keyname
                                 character modifiers window timestamp))
(defgeneric driver-cb-enter-event (handler driver pointer
                                   x y root-x root-y window time))
(defgeneric driver-cb-leave-event (handler driver pointer window time))
(defgeneric driver-cb-wm-delete-event (handler driver window time))
