(in-package :cldk-internals)

;;;
;;; kernel mixin
;;;

(defclass server-kernel-mixin ()
  ())

(defclass kernel-object-mixin ()
  ((kernel :initform nil
           :initarg :kernel
           :reader kernel)))

(defmethod driver ((object kernel-object-mixin))
  (kernel object))

(defmethod driver ((object server-kernel-mixin))
  object)
