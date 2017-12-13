(in-package :cldk-internals)

(defvar *all-servers* nil)

(defun map-over-servers (function)
  (mapc function *all-servers*))

;;;
;;; server class
;;;

(defclass server (kernel-mixin)
  ((path :initform nil
         :initarg :path
         :reader server-path)))

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
;;; server objects
;;;

(defclass server-object (kernel-object-mixin)
  ((kernel :initform nil
           :initarg :server
           :reader server)))
