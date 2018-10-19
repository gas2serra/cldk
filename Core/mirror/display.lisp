(in-package :cldk-internals)

(defvar *default-event-handler*)

;;;
;;;
;;;

(defclass display ()
  ())

(defgeneric avaiable-cursor-names (display))

(defgeneric refresh-windows (display))
;;;
;;;
;;;

(defclass kerneled-display-mixin (display-driver kerneled-driver-mixin)
  ((event-handler :initform *default-event-handler*
                  :accessor event-handler)
   (kwindows :initform nil
             :reader kernel-kwindows)
   (cursor-table :initform (make-hash-table :test #'eq)
                 :accessor server-cursor-table))
  (:default-initargs :callback-handler
      *default-event-handler*))

(defmethod avaiable-cursor-names ((display kerneled-display-mixin))
  (within-kernel-mode (display :block-p t)
    (driver-avaiable-cursor-names display)))

(defmethod refresh-windows ((display kerneled-display-mixin))
  (dolist (win (kernel-kwindows display))
    (refresh-window win)))
