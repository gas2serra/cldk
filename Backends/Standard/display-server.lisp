(in-package :cldk-backend)

;;;
;;;
;;;

(defclass single-thread-display-server (display-server
                                        ;;event-server-mixin
                                        callback-queue-with-thread-mixin
                                        ;;lparallel-kernel-call-mixin
                                        command-queue-mixin
                                        server-with-thread-mixin)
  ())

(defmethod server-loop-step ((server single-thread-display-server))
  (check-kernel-mode)
  (driver-process-next-events server)
  (process-next-calls server)
  (unless (server-stopping-p server)
    (k-refresh-windows server)
    (driver-force-output server)))


(defclass multi-thread-display-server (display-server
                                       event-server-mixin
                                       ;;callback-queue-with-thread-mixin
                                       command-server-mixin
                                       multi-threaded-driver-mixin
                                       server-with-thread-mixin)
  ())

(defmethod server-loop-step ((server multi-thread-display-server))
  (driver-process-next-events server)
  (unless (server-stopping-p server)
    (k-refresh-windows server)
    (driver-force-output server)))
