(in-package :cldk-internals)

;;;
;;; window 
;;;
(defclass root ()
  ())

(defgeneric create-root (display))
(defgeneric destroy-root (root))
(defgeneric root-size (root &key units force-query-p))
(defgeneric root-pointer-position (root))
                   
;;;
;;; kerneled window
;;;
(defclass kerneled-root-mixin (driver-object)
  ((cached-width :initform nil)
   (cached-height :initform nil)))

(defmethod initialize-instance  :after ((root kerneled-root-mixin) &rest args)
  (declare (ignore args))
  (within-kernel-mode ((driver root) :block-p t)
    (driver-initialize-root (driver root) root)))

(defmethod destroy-root ((root kerneled-root-mixin))
  (within-kernel-mode ((driver root) :block-p t)
      (driver-destroy-root (driver root) root)))

(defmethod root-size ((root kerneled-root-mixin) &key (units :device) (force-query-p nil))
  (with-slots (cached-width cached-height) root
    (when (or force-query-p (null cached-width) (null cached-height))
      (within-kernel-mode ((driver root) :block-p t)
        (multiple-value-bind (w h)
            (driver-root-size (driver root) root units)
          (setf cached-width w
                cached-height h))))
    (values cached-width cached-height)))

(defmethod root-pointer-position ((root kerneled-root-mixin))
  (within-kernel-mode ((driver root) :block-p t)
    (driver-window-pointer-position (driver root) root)))




