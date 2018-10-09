(in-package :cldk-internals)

(defvar *all-servers* nil)

(defun map-over-servers (function)
  (mapc function *all-servers*))

;;;
;;; server class
;;;

(defclass server ()
  ((path :initform nil
         :initarg :path
         :reader server-path)
   (state :initform :stopped
          :type (member :starting :running :stopping :stopped)
          :reader server-state)))

(defmethod initialize-instance :after ((server server)
                                       &key)
  (setf (driver-options server) (cons :id (server-path server))))

(defun server-running-p (server)
  (eql (server-state server) :running))

(defun server-stopped-p (server)
  (eql (server-state server) :stopped))

(defun server-stopping-p (server)
  (eql (server-state server) :stopping))

(defgeneric start-server (server))
(defgeneric stop-server (server))
(defgeneric kill-server (server))
(defgeneric restart-server (server))
(defgeneric destroy-server (server))
(defgeneric server-force-output (server))

(defmethod start-server :around ((server server))
  (if (server-running-p server)
      (error "cldk server is already running")
      (call-next-method)))

(defmethod start-server :before ((server server))
  (with-slots (state) server
    (setf state :starting)))

(defmethod start-server :after ((server server))
  (with-slots (state) server
    (setf state :running)))

(defmethod stop-server :around ((server server))
  (if (not (server-running-p server))
      (error "cldk server is not running")
      (call-next-method)))

(defmethod stop-server :before ((server server))
  (with-slots (state) server
    (setf state :stopping)))

(defmethod stop-server :after ((server server))
  (with-slots (state) server
    (setf state :stopped)))

(defmethod kill-server :around ((server server))
  (if (server-stopped-p server)
      (error "cldk server is already stopped")
      (progn
        (log:warn "killing cldk server")
        (call-next-method))))

(defmethod restart-server ((server server))
  (stop-server server)
  (start-server server))

(defmethod destroy-server ((server server))
  (when (server-running-p server)
    (stop-server server)))

(defmethod destroy-server :after ((server server))
  (setf *all-servers* (remove server *all-servers*)))

;;;
;;; Find server
;;;

(defun find-server (server-path)
  (if (atom server-path)
      (setq server-path (list server-path)))
  (let ((server-path-parser-fn (get (first server-path) :server-path-parser-fn)))
    (unless server-path-parser-fn
      (error "Don't know how to make a server of type ~S"
             server-path))
    (setq server-path
          (funcall server-path-parser-fn server-path)))
  (loop for server in *all-servers*
     if (equal server-path (server-path server))
     do
       (progn
         (when (server-stopped-p server)
           (start-server server))
         (return server))
     finally (let ((server-class (get (first server-path) :server-class))
                   server)
               (if (null server-class)
                   (error "Don't know how to make a server of type ~S"
                          server-path))
               (setq server
                     (funcall 'make-instance server-class
                              :path server-path))
               (push server *all-servers*)
               (return server))))

;;;
;;; server thread
;;;

(defclass server-with-thread-mixin ()
  ((kernel-thread :initform nil
                  :reader server-kernel-thread)))

(defmethod start-server ((server server-with-thread-mixin))
  (with-slots (kernel-thread) server
    (setf kernel-thread
          (bt:make-thread #'(lambda ()
                              (server-loop-fn server))
                          :name (format nil "cldk server ~A" (driver-id server))))))

(defmethod stop-server ((server server-with-thread-mixin))
  (bt:join-thread (server-kernel-thread server)))

(defmethod stop-server :after ((server server-with-thread-mixin))
  (when (bt:thread-alive-p (server-kernel-thread server))
    (kill-server server)))

(defmethod kill-server ((server server-with-thread-mixin))
 (with-slots (kernel-thread) server
    (bt:destroy-thread kernel-thread)
    (setf kernel-thread nil)))

(defmethod destroy-server :around ((server server-with-thread-mixin))
  (unwind-protect
       (call-next-method)
    (when (bt:thread-alive-p (server-kernel-thread server))
      (kill-server server))))

(defgeneric server-loop-step (server))

(defgeneric server-loop (server &key min-loop-time))

(defmethod server-loop ((server server-with-thread-mixin)  &key (min-loop-time 0.01))
  (block loop
    (loop
       (let ((end-time (+ (get-internal-real-time) (* min-loop-time internal-time-units-per-second))))
         (server-loop-step server)
         (let ((wait-time (- end-time (get-internal-real-time))))
           (when (> wait-time 0)
             (sleep (/ wait-time internal-time-units-per-second)))))
       (when (server-stopping-p server)
         (return-from loop)))))
  
(defun server-loop-fn (server)
  (let ((*kernel-mode* t))
    (driver-start server)
    (block loop
      (loop
         (with-simple-restart
             (restart-server-loop
              "restart cldk's server loop.")
           (server-loop server)
           (driver-stop server)
           (return-from loop))))))

;;;
;;; command server
;;;

(defclass command-server-mixin ()
  ())

(defmethod stop-server ((server command-server-mixin))
  (call-next-method)
  #+nil (kernel-call server :stop t))

(defmethod server-force-output ((server command-server-mixin))
  #+nil (<call- server #'k-force-output server)
  )

;;;
;;; command queue
;;;

(defclass command-queue-mixin (command-server-mixin lparallel-kernel-call-mixin)
  ())

(defgeneric process-next-calls (server &key maxtime)
  (:method ((server command-queue-mixin) &key (maxtime 0.03))
    (process-next-kernel-calls server :maxtime maxtime)))

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
