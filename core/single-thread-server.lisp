(in-package :cldk-internals)


(defclass command-server-mixin ()
  ())

(defun make-command (fn args)
  (list :fn fn args))

(defun make-stop-command ()
  (list :stop))

(defun exec-command (server command)
  (handler-case
      (let ((result
             (case (car command)
               (:stop
                (when *kernel-mode*
                  (with-slots (shutdown-p) server
                    (when (not shutdown-p)
                      (shutdown server)))))
               (:fn
                (apply (second command) (third command))))))
        result)
    (error (condition)
      (log:error "~A ~A ~A" (car command) (cdr command) condition))))

(defgeneric call (server command &key block-p)
  (:method ((server command-server-mixin) command &key block-p)
    (break)
    (exec-command server command)))

(defmacro <call+ (server fn &rest args)
  `(call ,server (make-command ,fn (list ,@args)) :block-p t))

(defmacro <call- (server fn &rest args)
  `(call ,server (make-command ,fn (list ,@args)) :block-p nil))


;;;
;;;
;;;

(defclass command-queue-mixin (command-server-mixin)
  ((call-queue :initform (lparallel.queue:make-queue))))

(defun %empty-server-queue (queue)
  (loop
     (if (lparallel.queue:queue-empty-p queue)
         (return)
         (let ((command-and-promise
                (lparallel.queue:pop-queue queue)))
           (when (and command-and-promise (cdr command-and-promise))
             (lparallel:fulfill (cdr command-and-promise)
               nil))))))

(defmethod stop-server :after ((server  command-queue-mixin))
  (with-slots (call-queue) server
    (%empty-server-queue call-queue)))

(defmethod kill-server :after ((server  command-queue-mixin))
  (with-slots (call-queue) server
    (%empty-server-queue call-queue)))

(defgeneric exec-call (server command))

(defun %call (server command queue block-p)
  (declare (ignore server))
  (if block-p
      (let ((p (lparallel:promise)))
        (lparallel.queue:push-queue (cons command p) queue)
        (lparallel:force p))
        (lparallel.queue:push-queue (cons command nil) queue)))

(defun %exec-call (server command)
    (log:trace "~A" command)
    (if (cdr command)
        (let ((res (exec-command server (car command))))
          (lparallel:fulfill (cdr command)
            (lparallel:chain
             (lparallel:delay res))))
        (exec-command server (car command))))

(defmethod call ((server command-queue-mixin) command &key block-p)
  (with-slots (call-queue) server
    (%call server command call-queue block-p)))

(defmethod exec-call ((server command-queue-mixin) command)
  (when (not *kernel-mode*)
    (log:warn "a thread not in kernel mode is calling the kernel: ~A" command))
  (%exec-call server command))

(defgeneric process-next-calles (server &key maxtime)
  (:method ((server command-queue-mixin) &key (maxtime 0.03))
    (let ((end-time (+ (get-internal-real-time) (* maxtime internal-time-units-per-second)))
          (count 0))
      (with-slots (call-queue) server
        (loop with command-and-promise = nil do
             (setq command-and-promise (lparallel.queue:peek-queue call-queue))
             (setq count (1+ count))
             (when command-and-promise
               (lparallel.queue:pop-queue call-queue)
               (exec-call server command-and-promise))
           while (and command-and-promise
                      (< (get-internal-real-time) end-time)
                      (not (eql (car command-and-promise) :stop)))))
      (when (> (get-internal-real-time) end-time)
        (log:info "next calls time exceded after ~A steps" count)))))

;;;
;;;
;;;


(defun check-kernel-mode ()
  (unless *kernel-mode*
    (error "a thread not in kernel mode is calling a kernel function")))

(defclass single-thread-server-mixin (command-queue-mixin
                                      server-with-thread-mixin)
  ((kernel-thread :initform nil
                  :reader server-kernel-thread)))

(defgeneric boot (kernel))

(defmethod boot ((server single-thread-server-mixin))
  (driver-start server))

(defgeneric shutdown (server))

(defmethod shutdown :before ((server single-thread-server-mixin))
  (with-slots (shutdown-p) server
    (setf shutdown-p t)))

(defmethod shutdown ((server single-thread-server-mixin))
  (driver-stop server))

(defmethod start-server ((server single-thread-server-mixin))
  (with-slots (kernel-thread) server
    (setf kernel-thread
          (bt:make-thread #'(lambda ()
                              (server-loop-fn server))
                          :name "cldk server"))))

(defmethod stop-server ((server single-thread-server-mixin))
  (with-slots (kernel-thread) server
    (call server (make-stop-command) :block-p t)))

(defmethod kill-server ((server single-thread-server-mixin))
  (with-slots (kernel-thread) server
    (bt:destroy-thread kernel-thread)
    (setf kernel-thread nil)))


(defmethod shutdown :after ((server  single-thread-server-mixin))
  (empty-call-queue server))



(defgeneric server-loop-step (kernel))

(defgeneric server-loop (kernel &key min-loop-time))

(defmethod server-loop ((server single-thread-server-mixin)  &key (min-loop-time 0.01))
  (block loop
    (loop
       (let ((end-time (+ (get-internal-real-time) (* min-loop-time internal-time-units-per-second))))
         (server-loop-step server)
         (let ((wait-time (- end-time (get-internal-real-time))))
           (when (> wait-time 0)
             (sleep (/ wait-time internal-time-units-per-second)))))
       (with-slots (shutdown-p) server
         (when shutdown-p
           (return-from loop))))))
  
(defun server-loop-fn (server)
  (boot server)
  (block loop
    (let ((*kernel-mode* t))
      (loop
         (with-simple-restart
             (restart-server-loop
              "restart cldk's server loop.")
           (server-loop server)
           (with-slots (shutdown-p) server
             (setf shutdown-p nil))
           (return-from loop))))))



;;;
;;;
;;;

(defclass multi-thread-server-mixin ()
  ())
