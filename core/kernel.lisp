(in-package :cldk-internals)

;;;
;;; kernel mode
;;;

(defvar *kernel-mode* nil)

(defun check-kernel-mode ()
  (unless *kernel-mode*
    (error "a thread not in kernel mode is calling a kernel function")))

(defun check-user-mode ()
  (when *kernel-mode*
    (error "a thread in kernel mode is calling a non kernel function")))

;;;
;;; kernel
;;;

(defclass server-kernel (server-driver-mixin)
  ())

(defgeneric call (server command &key block-p)
  (:method ((server server-kernel) command &key block-p)
    (declare (ignore block-p))
    (let ((*kernel-mode* t))
      (exec-command server command))))

(defmacro <call+ (server fn &rest args)
  `(call ,server (make-command ,fn (list ,@args)) :block-p t))

(defmacro <call- (server fn &rest args)
  `(call ,server (make-command ,fn (list ,@args)) :block-p nil))

(defgeneric callback (server command &key block-p)
  (:method ((server server-kernel) command &key block-p)
    (declare (ignore block-p))
    (let ((*kernel-mode* nil))
      (exec-command server command))))

(defmacro <callback+ (server fn &rest args)
  `(callback ,server (make-command ,fn (list ,@args)) :block-p t))

(defmacro <callback- (server fn &rest args)
  `(callback ,server (make-command ,fn (list ,@args)) :block-p nil))

;;;
;;; Core Functions
;;;

(defun k-start (kernel)
  (check-kernel-mode)
  (driver-start kernel))

(defun k-stop (kernel)
  (check-kernel-mode)
  (driver-stop kernel))

(defun k-kill (kernel)
  (check-kernel-mode)
  (driver-kill kernel))

(defun k-ping (kernel)
  (check-kernel-mode)
  (driver-ping kernel))

(defun k-force-output (kernel)
  (check-kernel-mode)
  (driver-force-output kernel))

(defun k-process-next-event (kernel)
   (check-kernel-mode)
   (driver-process-next-event kernel))

;;;
;;;
;;;

(defgeneric k-process-next-driver-events (kernel &key maxtime)
  (:method ((kernel server-kernel) &key (maxtime 0.01))
    (check-kernel-mode)
    (let ((end-time (+ (get-internal-real-time) (* maxtime internal-time-units-per-second))))
      (loop with event-p = nil do
           (setq event-p (driver-process-next-event kernel))
         while (and event-p
                    (< (get-internal-real-time) end-time)))
      (when (> (get-internal-real-time) end-time)
        (log:info "event time exceded")))))
