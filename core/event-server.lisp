(in-package :cldk-internals)

;;;
;;; event server mixin
;;;

(defclass event-server-mixin ()
  ((driver-object-id->server-object :initform (make-hash-table))))

(defgeneric register-server-object (server server-object)
  (:method ((server event-server-mixin) server-object)
    (with-slots (driver-object-id->server-object) server
      (setf (gethash (driver-object-id server-object) driver-object-id->server-object)
            server-object))))

(defgeneric unregister-server-object (server server-object)
  (:method ((server event-server-mixin) server-object)
    (with-slots (driver-object-id->server-object) server
      (setf (gethash (driver-object-id server-object) driver-object-id->server-object)
            nil))))

(defgeneric lookup-server-object (server driver-object-id)
  (:method ((server event-server-mixin) driver-object-id)
    (with-slots (driver-object-id->server-object) server
      (gethash driver-object-id driver-object-id->server-object))))

(defgeneric process-next-driver-events (server &key maxtime)
  (:method ((server event-server-mixin) &key (maxtime 0.01))
    (let ((end-time (+ (get-internal-real-time) (* maxtime internal-time-units-per-second))))
      (loop with event-p = nil do
           (setq event-p (driver-process-next-event server))
         while (and event-p
                    (< (get-internal-real-time) end-time)))
      (when (> (get-internal-real-time) end-time)
        (log:info "event time exceded")))))

(defgeneric callback (server command &key block-p)
  (:method ((server event-server-mixin) command &key block-p)
    (exec-command server command)))

(defmacro <callback+ (server fn &rest args)
  `(callback ,server (make-command ,fn (list ,@args)) :block-p t))

(defmacro <callback- (server fn &rest args)
  `(callback ,server (make-command ,fn (list ,@args)) :block-p nil))

;;;
;;; callback queue
;;;

(defclass callback-queue-mixin (event-server-mixin)
  ((callback-queue :initform (lparallel.queue:make-queue))))

(defmethod stop-server :after ((server callback-queue-mixin))
  (with-slots (callback-queue) server
    (empty-command-queue callback-queue)))

(defmethod kill-server :after ((server callback-queue-mixin))
  (with-slots (callback-queue) server
    (empty-command-queue callback-queue)))

(defmethod callback ((server callback-queue-mixin) command &key block-p)
  (with-slots (callback-queue) server
    (in-queue-command server command callback-queue block-p)))
  
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

(defmethod stop-server ((server callback-queue-with-thread-mixin))
  (call-next-method)
  (callback server (make-stop-command) :block-p t))

(defmethod kill-server ((server callback-queue-with-thread-mixin))
  (call-next-method)
  (with-slots (callback-thread) server
    (bt:destroy-thread callback-thread)
    (setf callback-thread nil)))

(defgeneric callback-loop (server)
  (:method ((server callback-queue-with-thread-mixin))
    (with-slots (callback-queue) server
      (loop with com = nil do
           (setq com (exec-next-queued-command server callback-queue :kernel-mode-p nil))
         while (not (stop-command-p com))))))

(defun callback-loop-fn (server)
  (block loop
    (loop
       (with-simple-restart
           (restart-callback-loop
            "restart cldk's callback loop.")
         (callback-loop server)
         (return-from loop)))))
