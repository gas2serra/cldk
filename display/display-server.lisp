(in-package :cldk-internals)

(defvar *default-event-handler*)

(defclass display-server (server display-driver)
  ((kwindows :initform nil
             :reader kernel-kwindows)
   (event-handler :initform *default-event-handler*
                  :accessor server-event-handler)
   (cursor-table :initform (make-hash-table :test #'eq)
                 :accessor server-cursor-table))
  (:default-initargs :callback-handler (make-instance 'default-display-callback-handler))) 

(defgeneric event-handler (server))

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

;;;
;;;
;;;

(defclass single-thread-display-server (display-server
                                        callback-queue-with-thread-mixin
                                        command-queue-mixin
                                        server-with-thread-mixin)
  ())

(defmethod server-loop-step ((server single-thread-display-server))
  (k-process-next-driver-events server)
  (process-next-calls server)
  (unless (server-stopping-p server)
    (k-refresh-windows server)
    (driver-force-output server)))


(defclass multi-thread-display-server (display-server
                                       event-server-mixin
                                       command-server-mixin
                                       multi-threaded-driver-mixin
                                       server-with-thread-mixin)
  ())

(defmethod server-loop-step ((server multi-thread-display-server))
  (k-process-next-driver-events server)
  (unless (server-stopping-p server)
    (k-refresh-windows server)
    (driver-force-output server)))
