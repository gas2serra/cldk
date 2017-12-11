(in-package :cldk-internals)

;;;
;;; kernel
;;;

(defvar *kernel-mode* nil)

(defclass display-kernel-mixin (kernel-mixin)
  ((command-queue :initform (lparallel.queue:make-queue))
   (event-queue :initform (lparallel.queue:make-queue))
   (kwindows :initform nil
             :reader kernel-kwindows)))

(defgeneric shutdown (kernel)
  (:method ((kernel display-kernel-mixin))
    )
  (:method :after ((kernel display-kernel-mixin))
           (empty-kernel-queues kernel)))

(defun empty-kernel-queues (kernel)
  (with-slots (command-queue) kernel
    (loop
       (if (lparallel.queue:queue-empty-p  command-queue)
           (return)
           (let ((command-and-promise
                  (lparallel.queue:pop-queue command-queue)))
             (when (and command-and-promise (cdr command-and-promise))
               (lparallel:fulfill (cdr command-and-promise)
                 nil))))))
  (with-slots (event-queue) kernel
    (loop
         (if (lparallel.queue:queue-empty-p  event-queue)
             (return)
             (lparallel.queue:pop-queue event-queue)))))

;;;
;;; command queue
;;;

(defun push-command (kernel command &key (block-p nil))
  (with-slots (command-queue) kernel
    (when *kernel-mode*
      (log:warn "a thread in kernel mode is pushing the command: ~A" command))
    (if block-p
        (let ((p (lparallel:promise)))
          (lparallel.queue:push-queue (cons command p) command-queue)
          (lparallel:force p))
        (lparallel.queue:push-queue (cons command nil) command-queue))))

(defmacro <c+ (kernel fn &rest args)
  `(push-command ,kernel (list :fn ,fn (list ,@args)) :block-p t))

(defmacro <c- (kernel fn &rest args)
  `(push-command ,kernel (list :fn ,fn (list ,@args)) :block-p nil))

(defun exec-command (kernel command)
  (labels ((exec (com)
             (handler-case
                 (let ((result
                        (case (car com)
                          (:stop
                           (shutdown kernel))
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

(defun command-subloop (kernel &key (maxtime 0.01))
  (let ((end-time (+ (get-internal-real-time) (* maxtime internal-time-units-per-second))))
    (with-slots (command-queue) kernel
      (loop with command-and-promise = nil do
           (setq command-and-promise (lparallel.queue:peek-queue command-queue))
           (when command-and-promise
             (if (eql (car command-and-promise) :stop)
                 (shutdown kernel)
                 (progn
                   (lparallel.queue:pop-queue command-queue)
                   (exec-command kernel command-and-promise))))
         while (and command-and-promise
                    (< (get-internal-real-time) end-time)
                    (not (eql (car command-and-promise) :stop)))))
    (when (> (get-internal-real-time) end-time)
      (log:info "command loop - time"))))

;;;
;;; event queue
;;;

(defgeneric event-handler (kernel))

(defun push-event (kernel event)
  (with-slots (event-queue) kernel
    (when (and (not *kernel-mode*)
               (not (eql event :stop)))
      (log:warn "a thread not in kernel mode is pushing the event: ~A" event))
    (lparallel.queue:push-queue event event-queue)))

(defmacro <e- (kernel fn &rest args)
  `(push-event ,kernel (list :fn ,fn (list (event-handler ,kernel) ,@args))))

(defun exec-event (kernel event)
  (when *kernel-mode*
      (log:warn "a thread in kernel mode is executing the event: ~A" event))
  (handler-case
      (let ((result
             (case (car event)
               (:fn
                (apply (second event) (third event))))))
        result)
    (error (condition)
      (log:error "~A ~A" event condition))))

(defun event-loop (kernel)
  (with-slots (event-queue) kernel
    (loop with event = nil do
         ;;(log:info "!!!wait for an event")
         (setq event (lparallel.queue:pop-queue event-queue))
         ;;(log:info "!!! event ~A" event)
         (unless (eql event :stop)
           (exec-event kernel event))
       while (not (eql event :stop)))))

(defun event-loop-fn (kernel)
  (block loop
    (loop
       (with-simple-restart
           (restart-event-loop
            "restart cldk's event loop.")
         (loop
            (event-loop kernel)
            (return-from loop))))))

