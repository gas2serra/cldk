(in-package :cldk-internals)

(defvar *kernel-mode* nil)

(defclass single-thread-kernel-mixin (kernel-mixin)
  ((call-queue :initform (lparallel.queue:make-queue))
   (callback-queue :initform (lparallel.queue:make-queue))))

(defmethod shutdown :after ((kernel single-thread-kernel-mixin))
  (empty-kernel-queues kernel))

(defun empty-kernel-queues (kernel)
  (labels ((empty (queue)
             (loop
                (if (lparallel.queue:queue-empty-p queue)
                    (return)
                    (let ((command-and-promise
                           (lparallel.queue:pop-queue queue)))
                      (when (and command-and-promise (cdr command-and-promise))
                        (lparallel:fulfill (cdr command-and-promise)
                                           nil)))))))
    (with-slots (call-queue callback-queue) kernel
      (empty call-queue)
      (empty callback-queue))))

(defun %call (kernel command queue block-p)
  (declare (ignore kernel))
  (if block-p
      (let ((p (lparallel:promise)))
        (lparallel.queue:push-queue (cons command p) queue)
        (lparallel:force p))
        (lparallel.queue:push-queue (cons command nil) queue)))

(defun %exec-call (kernel command)
  (labels ((exec (com)
             (handler-case
                 (let ((result
                        (case (car com)
                          (:stop
                           (with-slots (shutdown-p) kernel
                             (when (not shutdown-p)
                               (shutdown kernel))))
                          (:fn
                           (apply (second com) (third com)))
                          (:funcall
                           (apply (second com) (third com) (fourth com))))))
                   result)
               (error (condition)
                 (log:error "~A ~A" (cdr com) condition)))))
    (if (cdr command)
        (lparallel:fulfill (cdr command)
                           (lparallel:chain
                            (lparallel:delay
                             (exec (car command)))))
        (exec (car command)))))

(defmethod call ((kernel single-thread-kernel-mixin) command &key block-p)
  (with-slots (call-queue) kernel
    (when *kernel-mode*
      (log:warn "a thread in kernel mode is calling the kernel: ~A" command))
    (%call kernel command call-queue block-p)))
  
(defmethod exec-call ((kernel single-thread-kernel-mixin) command)
  (when (not *kernel-mode*)
    (log:warn "a thread not in kernel mode is calling the kernel: ~A" command))
  (%exec-call kernel command))

(defmethod callback ((kernel single-thread-kernel-mixin) command &key block-p)
  (with-slots (callback-queue) kernel
    (when (not *kernel-mode*)
      (log:warn "a thread not in kernel mode is calling the kernel: ~A" command))
    (%call kernel command callback-queue block-p)))
  
(defmethod exec-callback ((kernel single-thread-kernel-mixin) command)
  (when *kernel-mode*
    (log:warn "a thread in kernel mode is calling the kernel: ~A" command))
  (%exec-call kernel command))

;;;
;;; kernel loop
;;;

(defgeneric kernel-loop-step (kernel))

(defgeneric kernel-loop (kernel &key min-loop-time))

(defmethod kernel-loop ((server single-thread-kernel-mixin)  &key (min-loop-time 0.01))
  (block loop
    (loop
       (let ((end-time (+ (get-internal-real-time) (* min-loop-time internal-time-units-per-second))))
         (kernel-loop-step server)
         (let ((wait-time (- end-time (get-internal-real-time))))
           (when (> wait-time 0)
             (sleep (/ wait-time internal-time-units-per-second))))))
       (with-slots (shutdown-p) server
         (when shutdown-p
           (return-from loop)))))
  
(defun kernel-loop-fn (kernel)
  (boot kernel)
  (block loop
    (let ((*kernel-mode* t))
      (loop
         (with-simple-restart
             (restart-kernel-loop
              "restart cldk's kernel loop.")
           (kernel-loop kernel)
           (return-from loop))))))

(defgeneric process-next-calles (kernel &key maxtime))

(defmethod process-next-calles ((kernel single-thread-kernel-mixin) &key (maxtime 0.01))
  (let ((end-time (+ (get-internal-real-time) (* maxtime internal-time-units-per-second))))
    (with-slots (call-queue) kernel
      (loop with command-and-promise = nil do
           (setq command-and-promise (lparallel.queue:peek-queue call-queue))
           (when command-and-promise
             (unless (eql (car command-and-promise) :stop)
               (lparallel.queue:pop-queue call-queue)
               (exec-call kernel command-and-promise)))
         while (and command-and-promise
                    (< (get-internal-real-time) end-time)
                    (not (eql (car command-and-promise) :stop)))))
    (when (> (get-internal-real-time) end-time)
      (log:info "next calls time exceded"))))

;;;
;;; callback loop
;;;

(defgeneric callback-loop (kernel))

(defmethod callback-loop ((kernel single-thread-kernel-mixin))
  (with-slots (callback-queue) kernel
    (loop with com = nil do
         (setq com (lparallel.queue:pop-queue callback-queue))
         (unless (eql (car com) :stop)
           (exec-callback kernel com))
       while (not (eql (car com) :stop)))))
  
(defun callback-loop-fn (kernel)
  (block loop
    (loop
       (with-simple-restart
           (restart-callback-loop
            "restart cldk's callback loop.")
         (callback-loop kernel)
         (return-from loop)))))
