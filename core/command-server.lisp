(in-package :cldk-internals)

;;;
;;; command server
;;;

(defclass command-server-mixin ()
  ())

(defmethod stop-server ((server command-server-mixin))
  (call-next-method)
  (kernel-call server :stop t))

(defmethod server-force-output ((server command-server-mixin))
  #+nil (<call- server #'k-force-output server))

;;;
;;; command queue
;;;

(defclass command-queue-mixin (command-server-mixin lparallel-kernel-call-mixin)
  ())

(defgeneric process-next-calls (server &key maxtime)
  (:method ((server command-queue-mixin) &key (maxtime 0.03))
    (process-next-kernel-calls server :maxtime maxtime)))
