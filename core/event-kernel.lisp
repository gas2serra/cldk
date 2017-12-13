(in-package :cldk-internals)

(defclass event-kernel-mixin (kernel-mixin)
  ())

;;;
;;; event loop
;;;

(defgeneric process-next-driver-events (kernel &key maxtime))

(defmethod process-next-driver-events ((kernel event-kernel-mixin) &key (maxtime 0.01))
  (let ((end-time (+ (get-internal-real-time) (* maxtime internal-time-units-per-second))))
    (loop with event-p = nil do
         (setq event-p (driver-process-next-event (driver kernel)
                                                  kernel
                                                  :timeout 0.01))
       while (and event-p
                  (< (get-internal-real-time) end-time)))
    (when (> (get-internal-real-time) end-time)
      (log:info "event time exceded"))))
