(in-package :cldk-kernel)

(defclass kerneled-buffer-mixin (driver-object)
  ())

(defmethod initialize-instance  :after ((buffer kerneled-buffer-mixin) &key width height
                                                          &allow-other-keys)
  (within-kernel-mode ((driver buffer) :block-p t)
    (driver-initialize-buffer (driver buffer) buffer width height)))

(defgeneric create-buffer (driver width height))

(defgeneric destroy-buffer (buffer)
  (:method ((buffer kerneled-buffer-mixin))
    (within-kernel-mode ((driver buffer) :block-p t)
      (driver-destroy-buffer (driver buffer) buffer))))

(defgeneric update-buffer (buffer width height)
  (:method ((buffer kerneled-buffer-mixin) width height)
    (within-kernel-mode ((driver buffer) :block-p t)
      (driver-update-buffer (driver buffer) buffer width height))))
