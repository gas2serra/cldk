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

(defmethod stop-server :after ((server callback-queue-mixin))
  (empty-lparallel-queue (kernel-callback-queue server)))

(defmethod kill-server :after ((server callback-queue-mixin))
  (empty-lparallel-queue (kernel-callback-queue server)))

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
                                          :name "cldk callback server"))))

(defmethod stop-server :before ((server callback-queue-with-thread-mixin))
  (kernel-callback server :stop t))

(defmethod kill-server ((server callback-queue-with-thread-mixin))
  (call-next-method)
  (with-slots (callback-thread) server
    (bt:destroy-thread callback-thread)
    (setf callback-thread nil)))

(defgeneric callback-loop (server)
  (:method ((server callback-queue-with-thread-mixin))
    (loop with res = nil do
         (setq res (exec-next-kernel-callback server))
       while (not (eql res :stop)))))

(defun callback-loop-fn (server)
  (block loop
    (loop
       (with-simple-restart
           (restart-callback-loop
            "restart cldk's callback loop.")
         (callback-loop server)
         (log:info "Exit......!!!!!")
         (return-from loop)))))
