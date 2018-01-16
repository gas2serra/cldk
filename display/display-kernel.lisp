(in-package :cldk-internals)

;;;
;;; kernel
;;;

(defclass display-server-kernel-mixin (server-kernel-mixin)
  ((kwindows :initform nil
             :reader kernel-kwindows)))

(defgeneric event-handler (kernel))

(defmethod server-loop-step ((kernel display-server-kernel-mixin))
  (process-next-driver-events kernel)
  (process-next-calles kernel)
  (unless (kernel-shutdown-p kernel)
    (k-refresh-windows kernel)
    (driver-force-output (driver kernel))))
