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

(defmethod stop-server :after ((server  command-queue-mixin))
  (empty-lparallel-queue (kernel-call-queue server)))

(defmethod kill-server :after ((server  command-queue-mixin))
  (empty-lparallel-queue (kernel-call-queue server)))

(defgeneric process-next-calls (server &key maxtime)
  (:method ((server command-queue-mixin) &key (maxtime 0.03))
    (let ((end-time (+ (get-internal-real-time) (* maxtime internal-time-units-per-second)))
          (count 0))
      (loop with res = nil do
           (setq res (exec-next-kernel-call server))
           (setq count (1+ count))
         while (and res
                    (< (get-internal-real-time) end-time)))
      (when (> (get-internal-real-time) end-time)
        (log:info "next calls time exceded after ~A steps" count)))))
