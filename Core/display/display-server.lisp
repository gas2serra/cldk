(in-package :cldk-internals)

(defclass display-server (server display kerneled-display-mixin)
  ()
  (:default-initargs :callback-handler
      (make-instance 'default-display-callback-handler)))

(defun server-event-handler (server)
  (event-handler server))

(defun (setf server-event-handler) (val server)
  (setf (event-handler server) val))

;;;
;;; Find server
;;;

(defvar *default-display-server-path* nil)

(defvar *display-server-path-search-order*
  '(:sdl2 :clx :null))

(defun find-default-display-server-path ()
  (loop for server in *display-server-path-search-order*
     if (get server :server-class)
     do (return-from find-default-display-server-path (list server))
     finally (error "No CLDK display servers have been loaded!")))

(defun find-display-server (&key (server-path *default-display-server-path*))
  (if (null server-path)
      (setq server-path (find-default-display-server-path)))
  (find-server server-path))

