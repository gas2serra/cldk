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
         :reader server-path)))

(defun driver-options (server)
  (cdr (server-path server)))

(defgeneric restart-server (server)
  (:method ((server server))
    ))

(defgeneric destroy-server (server)
  (:method :after ((server server))
           (setf *all-servers* (remove server *all-servers*))))

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
     do (return server)
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

(defvar *kernel-mode* nil)

(defclass server-with-thread-mixin ()
  ((shutdown-p :initform nil
               :reader kernel-shutdown-p)))

(defun server-running-p (server)
  (server-kernel-thread server))

(defgeneric restart-server (server)
  (:method ((server server-with-thread-mixin))
    (stop-server server)
    (start-server server)))

(defgeneric start-server (server)
  (:method :around ((server server-with-thread-mixin))
           (if (server-running-p server)
               (error "cldk server is already stopped")
               (call-next-method))))

(defgeneric stop-server (server)
  (:method :around ((server server-with-thread-mixin))
           (if (not (server-running-p server))
               (error "cldk server is already stopped")
               (call-next-method))))

(defgeneric kill-server (server)
  (:method :around ((server server-with-thread-mixin))
           (if (not (server-running-p server))
               (error "cldk server is already stopped")
               (progn
                 (log:warn "killing cldk server")
                 (call-next-method)))))

(defmethod destroy-server ((server server-with-thread-mixin))
  (unwind-protect
       (when (and 
              (server-running-p server)
              (bt:thread-alive-p (server-kernel-thread server)))
         ;; destroy all cursors
         (stop-server server))
    (when (and (server-running-p server)
               (bt:thread-alive-p (server-kernel-thread server)))
      (kill-server server))))

;;;
;;; server objects
;;;

(defclass server-object (kernel-object-mixin)
  ((kernel :initform nil
           :initarg :server
           :reader server)))



