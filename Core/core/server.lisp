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
