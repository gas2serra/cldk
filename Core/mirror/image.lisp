(in-package :cldk-internals)
;;;
;;;
;;;

(defclass shared-image (basic-image)
  ((medium :initarg :medium :reader image-medium)))

