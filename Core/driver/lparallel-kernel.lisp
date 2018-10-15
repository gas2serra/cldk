(in-package :cldk-internals)

;;;
;;; lparallel queue
;;;

(defclass lparallel-kerneled-driver-mixin (queued-kerneled-driver-mixin)
  ())

(defmethod %next-kernel-queued-continuation ((driver queued-kerneled-driver-mixin) queue
                                             &key block-p kernel-mode-p)
  (labels ((%exec-cont (kernel cont)
             (handler-case
                 (funcall cont)
               (error (condition)
                 (log:error "~A ~A ~A" kernel cont condition))))
           (%exec-cont-and-prom (kernel cont-and-prom &key (kernel-mode-p t))
             (if kernel-mode-p
                 (check-kernel-mode)
                 (check-user-mode))
             (if (cdr cont-and-prom)
                 (lparallel:fulfill (cdr cont-and-prom) #1=(%exec-cont kernel (car cont-and-prom)))
                 #1#)))
    (let ((command-and-promise
           (if block-p
               (lparallel.queue:pop-queue queue)
               (lparallel.queue:peek-queue queue))))
      (when command-and-promise
        (unless block-p
          (lparallel.queue:pop-queue queue))
        (%exec-cont-and-prom driver command-and-promise
                             :kernel-mode-p kernel-mode-p)))))
  
(defmethod %inqueue-kernel-continuation ((driver queued-kerneled-driver-mixin) queue continuation
                                         &key block-p)
  (if block-p
      (let ((p (lparallel:promise)))
        (lparallel.queue:push-queue (cons continuation p) queue)
        (lparallel:force p))
      (lparallel.queue:push-queue (cons continuation nil) queue)))

(defmethod %empty-kernel-queue ((driver queued-kerneled-driver-mixin) queue)
  (loop
     (if (lparallel.queue:queue-empty-p queue)
         (return)
         (let ((command-and-promise
                (lparallel.queue:pop-queue queue)))
           (when (and command-and-promise (cdr command-and-promise))
             (lparallel:fulfill (cdr command-and-promise)
               nil))))))

(defclass lparallel-kernel-call-mixin (call-queued-kerneled-driver-mixin lparallel-kerneled-driver-mixin)
  ((call-queue :initform (lparallel.queue:make-queue))))

(defclass lparallel-kernel-callback-mixin (callback-queued-kerneled-driver-mixin lparallel-kerneled-driver-mixin)
  ((callback-queue :initform (lparallel.queue:make-queue))))

