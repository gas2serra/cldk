(in-package :cldk-internals)

(defvar *default-event-handler*)

;;;
;;;
;;;

(defclass display ()
  ())

(defgeneric screen-size (display &optional units))
(defgeneric screen-dpi (display))
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

(defmethod screen-size (display &optional (units :device))
  (within-kernel-mode (display :block-p t)
    (driver-screen-size display units)))

(defmethod screen-dpi (driver)
  (within-kernel-mode (driver :block-p t)
    (multiple-value-bind (w h)
        (driver-screen-size driver :device)
      (multiple-value-bind (in-w in-h)
          (driver-screen-size driver :inches)        
        (values (/ w in-w) (/ h in-h))))))

(defmethod screen-pointer-position ((display kerneled-display-mixin))
  (within-kernel-mode (display :block-p t)
    (driver-screen-pointer-position display)))

(defmethod avaiable-cursor-names ((display kerneled-display-mixin))
  (within-kernel-mode (display :block-p t)
    (driver-avaiable-cursor-names display)))
