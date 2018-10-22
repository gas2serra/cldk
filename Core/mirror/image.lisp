(in-package :cldk-internals)
;;;
;;;
;;;

(defclass shared-image (basic-image)
  ((device :initarg :device :reader image-device)))

