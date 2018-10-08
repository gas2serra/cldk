(in-package :cldk-kernel)

;;;
;;; lparallel queue functions
;;;

(defun %exec-cont (kernel cont)
  (handler-case
      (if (eql cont :stop) nil (funcall cont))
    (error (condition)
      (log:error "~A ~A ~A" kernel cont condition))))

(defun %exec-cont-and-prom (kernel cont-and-prom &key (kernel-mode-p t))
  (if kernel-mode-p
      (check-kernel-mode)
      (check-user-mode))
  (let ((res (%exec-cont kernel (car cont-and-prom))))
    (when (cdr cont-and-prom)
      (lparallel:fulfill (cdr cont-and-prom)
                         (lparallel:chain
                          (lparallel:delay res))))
    res))

(defun %exec-next-queued-command (kernel queue
                                  &key (kernel-mode-p t) (block-p t))
  (let ((command-and-promise
         (if block-p
             (lparallel.queue:pop-queue queue)
             (lparallel.queue:peek-queue queue))))
    (when command-and-promise
      (unless block-p
        (lparallel.queue:pop-queue queue))
      (unless (eql :stop (car command-and-promise))
        (%exec-cont-and-prom kernel command-and-promise
                             :kernel-mode-p kernel-mode-p)
        (car command-and-promise)))))

(defun empty-lparallel-queue (queue)
  (loop
     (if (lparallel.queue:queue-empty-p queue)
         (return)
         (let ((command-and-promise
                (lparallel.queue:pop-queue queue)))
           (when (and command-and-promise (cdr command-and-promise))
             (lparallel:fulfill (cdr command-and-promise)
               nil))))))

(defun in-lparallel-queue (continuation queue block-p)
  (if block-p
      (let ((p (lparallel:promise)))
        (lparallel.queue:push-queue (cons continuation p) queue)
        (lparallel:force p))
      (lparallel.queue:push-queue (cons continuation nil) queue)))

;;;
;;;
;;;

(defclass lparallel-kernel-callback-mixin (kerneled-driver-mixin)
  ((callback-queue :initform (lparallel.queue:make-queue)
                   :accessor kernel-callback-queue)))

(defmethod kernel-callback ((driver lparallel-kernel-callback-mixin) continuation block-p)
  (in-lparallel-queue continuation (kernel-callback-queue driver) block-p))

(defgeneric exec-next-kernel-callback (driver))
(defmethod exec-next-kernel-callback ((driver lparallel-kernel-callback-mixin))
  (%exec-next-queued-command driver (kernel-callback-queue driver)
                             :kernel-mode-p nil))

;;;
;;;
;;;

(defclass lparallel-kernel-call-mixin (kerneled-driver-mixin)
  ((call-queue :initform (lparallel.queue:make-queue)
               :accessor kernel-call-queue)))

(defmethod kernel-call ((driver lparallel-kernel-call-mixin) continuation block-p)
  (in-lparallel-queue continuation (kernel-call-queue driver) block-p))

(defgeneric exec-next-kernel-call (driver))
(defmethod exec-next-kernel-call ((driver lparallel-kernel-call-mixin))
  (%exec-next-queued-command driver (kernel-call-queue driver)
                             :block-p nil :kernel-mode-p t))

