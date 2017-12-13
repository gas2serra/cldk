(in-package :cldk-internals)

(defvar *default-event-handler*)

(defclass display-server (single-thread-server display-kernel-mixin)
  ((event-handler :initform *default-event-handler*
                  :accessor server-event-handler)
   (cursor-table :initform (make-hash-table :test #'eq)
                 :accessor server-cursor-table)))

(defmethod event-handler ((server display-server))
  (server-event-handler server))
   
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

;;;
;;;
;;;

(defmacro <d+ (server fn &rest args)
  `(<call+ ,server ,fn ,server ,@args))

(defmacro <d- (server fn &rest args)
  `(<call- ,server ,fn ,server ,@args))


(defun screen-num (server)
  (<d+ server #'k-screen-num))

(defun screen-size (server &optional (screen-index nil) (units :device))
  (<d+ server #'k-screen-size screen-index units))

(defun screen-dpi (server &optional (screen-index nil))
  (<d+ server #'k-screen-dpi screen-index))

(defun screen-pointer-position (server)
  (<d+ server #'k-screen-pointer-position))

(defun avaiable-cursor-names (server)
  (<d+ server #'k-avaiable-cursor-names))
