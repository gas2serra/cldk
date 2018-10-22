(in-package :cldk-internals)

(defclass display-driver (driver)
  ((screen-index :initform 0
     :initarg :screen
     :accessor driver-screen-index)))

(defclass display-driver-object (driver-object)
  ())

(defclass display-driver-callback-handler (driver-callback-handler)
  ())

;;;
;;; driver API
;;;

;;; root
(defclass driver-root (display-driver-object)
  ())
(defgeneric driver-initialize-root (driver root))
(defgeneric driver-destroy-root (root))
(defgeneric driver-root-size (root units))
(defgeneric driver-root-pointer-position (root))

;;; window
(defclass driver-window (display-driver-object)
  ())

(defgeneric driver-initialize-window (driver window name pretty-name
                                      x y width height mode)
  (:method :after ((driver display-driver) window name pretty-name
                   x y width height mode)
           (register-driver-object driver window)))
(defgeneric driver-destroy-window (window)
  (:method :before ((window driver-window))
           (unregister-driver-object (driver window) window)))
(defgeneric driver-show-window (window))
(defgeneric driver-hide-window (window))
(defgeneric driver-window-position (window))
(defgeneric driver-window-size (window))
(defgeneric driver-set-window-position (window x y))
(defgeneric driver-set-window-size (window width height))
(defgeneric driver-set-window-hints (window x y width height
                                     max-width max-height
                                     min-width min-height))
(defgeneric driver-raise-window (window))
(defgeneric driver-bury-window (window))
(defgeneric driver-window-pointer-position (window))
(defgeneric driver-copy-image-to-window (image x y width height
                                         window to-x to-y))

(defgeneric driver-grab-pointer (driver window pointer))
(defgeneric driver-ungrab-pointer (driver window pointer))

;;; cursor
(defclass driver-cursor (display-driver-object)
  ())

(defgeneric driver-avaiable-cursor-names (driver))
(defgeneric driver-create-cursor (driver named-cursor))
(defgeneric driver-destroy-cursor (cursor))
(defgeneric driver-set-window-cursor (window cursor))

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
