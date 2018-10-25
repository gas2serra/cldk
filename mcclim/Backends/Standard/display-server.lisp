(in-package :cldk-backend)

(defclass single-thread-display-server (
                                        cldki::display
                                        cldki::kerneled-display-mixin
                                        cldki::lparallel-kernel-callback-mixin
                                        cldki::callback-queued-kerneled-driver-with-thread-mixin
                                        cldki::lparallel-kernel-call-mixin
                                        cldki::driver-with-thread-mixin)
  ())

(defmethod cldki::driver-loop-step ((server single-thread-display-server))
  (check-kernel-mode)
  (driver-process-next-events server)
  (process-next-kernel-calls server :maxtime 0.03)
  (unless (or (cldki::driver-stopping-p server)
              (cldki::driver-stopped-p server))
    (refresh-windows server)
    (driver-force-output server)))


(defclass multi-thread-display-server (
                                       cldki::display
                                       cldki::kerneled-display-mixin
                                       multi-threaded-driver-mixin
                                       cldki::driver-with-thread-mixin)
  ())

(defmethod cldki::driver-loop-step ((server multi-thread-display-server))
  (driver-process-next-events server)
  (unless (or (cldki::driver-stopping-p server)
              (cldki::driver-stopped-p server))
    (refresh-windows server)
    (driver-force-output server)))

