(in-package :cldk-internals)
;;;
;;;
;;;

(defclass shared-image (cldk-render-internals::basic-image)
  ((medium :initarg :medium :reader image-medium)))

