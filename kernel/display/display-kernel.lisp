(in-package :cldk-kernel)

(defvar *default-event-handler*)

(defclass display-kernel-mixin (display-driver kerneled-driver-mixin)
  ((event-handler :initform *default-event-handler*
                  :accessor event-handler)
   (kwindows :initform nil
             :reader kernel-kwindows)
   (cursor-table :initform (make-hash-table :test #'eq)
                 :accessor server-cursor-table)))

(defun screen-num (display)
  (within-kernel-mode (display :block-p t)
    (driver-screen-num display)))

(defun screen-size (display &optional (screen-index nil) (units :device))
  (within-kernel-mode (display :block-p t)
    (driver-screen-size display screen-index units)))

(defun screen-dpi (display &optional (screen-index nil))
  (within-kernel-mode (display :block-p t)
    (driver-screen-dpi display screen-index)))

(defun screen-pointer-position (display)
  (within-kernel-mode (display :block-p t)
    (driver-screen-pointer-position display)))

(defun avaiable-cursor-names (display)
  (within-kernel-mode (display :block-p t)
    (driver-avaiable-cursor-names display)))
