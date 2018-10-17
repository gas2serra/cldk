(in-package :cldk-internals)
;;;
;;;
;;;

(defclass buffer (buffer-image-mixin image)
  ((width :initarg :width)
   (height :initarg :height)))

(defmethod image-width ((buffer buffer))
  (with-slots (width) buffer
    width))

(defmethod image-height ((buffer buffer))
  (with-slots (height) buffer
    height))

(defmethod image-rgb-get-fn ((image buffer) &key (dx 0) (dy 0))
  (driver-buffer-rgb-get-fn image dx dy))

(defmethod image-rgb-set-fn ((image buffer) &key (dx 0) (dy 0))
  (driver-buffer-rgb-set-fn image dx dy))

(defgeneric destroy-buffer (buffer))
(defgeneric update-buffer (buffer width height))
(defgeneric create-buffer (driver width height))

;;;
;;;
;;;
(defclass kerneled-buffer-mixin (driver-object)
  ())

(defmethod initialize-instance  :after ((buffer kerneled-buffer-mixin) &key width height
                                                          &allow-other-keys)
  (within-kernel-mode ((driver buffer) :block-p t)
    (driver-initialize-buffer (driver buffer) buffer width height)))

(defmethod destroy-buffer ((buffer kerneled-buffer-mixin))
  (within-kernel-mode ((driver buffer) :block-p t)
    (driver-destroy-buffer buffer)))

(defmethod update-buffer ((buffer kerneled-buffer-mixin) width height)
  (within-kernel-mode ((driver buffer) :block-p t)
    (driver-update-buffer buffer width height)))
