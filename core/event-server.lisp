(in-package :cldk-internals)

;;;
;;; event server mixin
;;;

(defclass event-server-mixin ()
  ())

;;;
;;; callback queue
;;;

(defclass callback-queue-mixin (event-server-mixin lparallel-kernel-callback-mixin)
  ())

;;;
;;; callback queue thread
;;;

(defclass callback-queue-with-thread-mixin (callback-queue-mixin  server-with-thread-mixin)
  ((callback-thread :initform nil
                    :reader server-callback-thread)))

(defmethod start-server ((server callback-queue-with-thread-mixin))
  (call-next-method)
  (with-slots (callback-thread) server
    (setf callback-thread (bt:make-thread #'(lambda ()
                                              (callback-loop-fn server))
                                          :name (format nil "cldk callback server ~A" (driver-id server))))))

(defmethod kill-server ((server callback-queue-with-thread-mixin))
  (call-next-method)
  (with-slots (callback-thread) server
    (bt:destroy-thread callback-thread)
    (setf callback-thread nil)))

(defgeneric callback-loop (server)
  (:method ((server callback-queue-with-thread-mixin))
    (process-next-kernel-callback-loop server)))


(defun callback-loop-fn (server)
  (block loop
    (loop
       (with-simple-restart
           (restart-callback-loop
            "restart cldk's callback loop.")
         (callback-loop server)
         (log:info "Exit......!!!!!")
         (return-from loop)))))
