(in-package :cldk-internals)

;;;
;;; command server
;;;

(defclass command-server-mixin ()
  ())

(defgeneric call (server command &key block-p)
  (:method ((server command-server-mixin) command &key block-p)
    (declare (ignore block-p))
    (let ((*kernel-mode* t))
      (exec-command server command))))

(defmacro <call+ (server fn &rest args)
  `(call ,server (make-command ,fn (list ,@args)) :block-p t))

(defmacro <call- (server fn &rest args)
  `(call ,server (make-command ,fn (list ,@args)) :block-p nil))

(defmethod stop-server ((server command-server-mixin))
  (call-next-method)
  (call server (make-stop-command) :block-p t))

(defmethod server-force-output ((server command-server-mixin))
  (<call- server #'driver-force-output server))

;;;
;;; command queue
;;;

(defclass command-queue-mixin (command-server-mixin)
  ((call-queue :initform (lparallel.queue:make-queue))))

(defmethod stop-server :after ((server  command-queue-mixin))
  (with-slots (call-queue) server
    (empty-command-queue call-queue)))

(defmethod kill-server :after ((server  command-queue-mixin))
  (with-slots (call-queue) server
    (empty-command-queue call-queue)))

(defmethod call ((server command-queue-mixin) command &key block-p)
  (with-slots (call-queue) server
    (in-queue-command server command call-queue block-p)))

(defgeneric process-next-calls (server &key maxtime)
  (:method ((server command-queue-mixin) &key (maxtime 0.03))
    (let ((end-time (+ (get-internal-real-time) (* maxtime internal-time-units-per-second)))
          (count 0))
      (with-slots (call-queue) server
        (loop with com = nil do
             (setq com (exec-next-queued-command server call-queue :block-p nil))
             (setq count (1+ count))
           while (and com
                      (< (get-internal-real-time) end-time)
                      (not (stop-command-p com)))))
      (when (> (get-internal-real-time) end-time)
        (log:info "next calls time exceded after ~A steps" count)))))
