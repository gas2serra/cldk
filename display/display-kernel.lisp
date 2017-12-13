(in-package :cldk-internals)

;;;
;;; kernel
;;;

(defclass display-kernel-mixin (single-thread-kernel-mixin event-kernel-mixin)
  ((kwindows :initform nil
             :reader kernel-kwindows)))

(defmethod kernel-loop-step ((kernel display-kernel-mixin))
  (process-next-driver-events kernel)
  (process-next-calles kernel)
  (k-refresh-windows kernel)
  (driver-force-output (driver kernel)))

(defgeneric event-handler (kernel))

