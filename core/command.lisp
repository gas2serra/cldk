(in-package :cldk-internals)

;;;
;;; command
;;;

(defun make-command (fn args)
  (list :fn fn args))

(defun make-stop-command ()
  (list :stop))

(defun stop-command-p (command)
  (eq (car command) :stop))

(defun exec-command (server command)
  (handler-case
      (let ((result
             (case (car command)
               (:stop
                nil)
               (:fn
                (apply (second command) (third command))))))
        result)
    (error (condition)
      (log:error "~A ~A ~A ~A" server (car command) (cdr command) condition))))

;;;
;;;  command queue
;;;

(defun in-queue-command (server command queue block-p)
  (declare (ignore server))
  (if block-p
      (let ((p (lparallel:promise)))
        (lparallel.queue:push-queue (cons command p) queue)
        (lparallel:force p))
      (lparallel.queue:push-queue (cons command nil) queue)))

(defun %exec-queued-command (server command &key (kernel-mode-p t))
  (if kernel-mode-p 
      (check-kernel-mode)
      (check-user-mode))
  (log:trace "~A" command)
    (if (cdr command)
        (let ((res (exec-command server (car command))))
          (lparallel:fulfill (cdr command)
            (lparallel:chain
             (lparallel:delay res))))
        (exec-command server (car command))))

(defun exec-next-queued-command (server queue &key (kernel-mode-p t) (block-p t))
  (if block-p
      (let ((command-and-promise (lparallel.queue:pop-queue queue)))
        (unless (stop-command-p (car command-and-promise))
          (%exec-queued-command server command-and-promise :kernel-mode-p kernel-mode-p))
        (car command-and-promise))
      (let ((command-and-promise (lparallel.queue:peek-queue queue)))
        (when command-and-promise
          (lparallel.queue:pop-queue queue)
          (%exec-queued-command server command-and-promise))
        (car command-and-promise))))

(defun empty-command-queue (queue)
  (loop
     (if (lparallel.queue:queue-empty-p queue)
         (return)
         (let ((command-and-promise
                (lparallel.queue:pop-queue queue)))
           (when (and command-and-promise (cdr command-and-promise))
             (lparallel:fulfill (cdr command-and-promise)
               nil))))))
