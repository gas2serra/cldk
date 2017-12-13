(in-package :cldk-internals)

(defclass multi-thread-kernel-mixin (kernel-mixin)
  ((main-lock :initform (bt:make-lock))))

(defmethod register-server-object :around ((kernel multi-thread-kernel-mixin)
                                           driver-object server-object)
  (with-slots (main-lock) kernel
    (bt:with-lock-held (main-lock)
      (call-next-method))))

(defmethod unregister-server-object :around ((kernel multi-thread-kernel-mixin)
                                             driver-object)
  (with-slots (main-lock) kernel
    (bt:with-lock-held (main-lock)
      (call-next-method))))
