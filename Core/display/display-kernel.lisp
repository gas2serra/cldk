(in-package :cldk-internals)

(defvar *default-event-handler*)

;;;
;;;
;;;

(defclass display ()
  ())

(defgeneric screen-num (display))
(defgeneric screen-size (display &optional screen-index units))
(defgeneric screen-dpi (display &optional screen-index))
(defgeneric screen-pointer-position (display))
(defgeneric avaiable-cursor-names (display))

  
;;;
;;;
;;;

(defclass kerneled-display-mixin (display-driver kerneled-driver-mixin)
  ((event-handler :initform *default-event-handler*
                  :accessor event-handler)
   (kwindows :initform nil
             :reader kernel-kwindows)
   (cursor-table :initform (make-hash-table :test #'eq)
                 :accessor server-cursor-table)))

(defmethod screen-num ((display kerneled-display-mixin))
  (within-kernel-mode (display :block-p t)
    (driver-screen-num display)))

(defmethod screen-size (display &optional (screen-index nil) (units :device))
  (within-kernel-mode (display :block-p t)
    (driver-screen-size display screen-index units)))

(defmethod screen-dpi (display &optional (screen-index nil))
  (within-kernel-mode (display :block-p t)
    (driver-screen-dpi display screen-index)))

(defmethod screen-pointer-position ((display kerneled-display-mixin))
  (within-kernel-mode (display :block-p t)
    (driver-screen-pointer-position display)))

(defmethod avaiable-cursor-names ((display kerneled-display-mixin))
  (within-kernel-mode (display :block-p t)
    (driver-avaiable-cursor-names display)))
