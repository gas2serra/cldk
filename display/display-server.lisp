(in-package :cldk-internals)

(defvar *all-servers* nil)

(defun map-over-servers (function)
  (mapc function *all-servers*))

(defvar *default-event-handler*)

(defclass display-server (display-kernel-mixin)
  ((path :initform nil
         :initarg :path
         :reader server-path)
   (thread :initform nil
           :reader server-thread)
   (event-thread :initform nil
                 :reader server-event-thread)
   (shutdown-p :initform nil
               :reader server-shutdown-p)
   (driver :initarg :driver
           :initform nil
           :reader server-driver)
   (event-handler :initform *default-event-handler*
                  :accessor server-event-handler)
   (cursor-table :initform (make-hash-table :test #'eq)
                 :accessor server-cursor-table)))

(defmethod event-handler ((server display-server))
  (server-event-handler server))

;;; commands

(defmethod shutdown ((server display-server))
  (driver-stop (server-driver server))
  (with-slots (shutdown-p) server
    (setf shutdown-p t)))

;;; start/stop

(defun server-running-p (server)
  (server-thread server))

(defun start-server (server)
  (when (server-running-p server)
    (error "cldk server is already running"))
  (with-slots (thread event-thread) server
    (setf thread
          (bt:make-thread #'(lambda ()
                              (server-loop-fn server))
                          :name "cldk-server"))
    (setf event-thread (bt:make-thread #'(lambda ()
                                           (event-loop-fn server))
                                       :name "cldk event server"))))
  
(defun stop-server (server)
  (unless (server-running-p server)
    (error "cldk server is already stopped"))
  (push-event server :stop)
  (push-command server :stop :block-p t))

(defun kill-server (server)
  (unless (server-running-p server)
    (error "server is not running"))
  (bt:destroy-thread (server-thread server))
  (bt:destroy-thread (server-event-thread server))
  (with-slots (thread event-thread) server
    (setf thread nil
          event-thread nil)))

(defgeneric restart-server (server)
  (:method ((server display-server))
    (stop-server server)
    (start-server server)))

(defgeneric destroy-server (server)
  (:method ((server display-server))
    (unwind-protect
         (when (and 
                (server-running-p server)
                (bt:thread-alive-p (server-thread server)))
           ;; destroy all cursors
           (stop-server server))
      (when (and (server-running-p server)
                 (bt:thread-alive-p (server-thread server)))
        (kill-server server))
      (setf *all-servers* (remove server *all-servers*)))))

;;; server loop

(defun process-next-driver-events (server &key (maxtime 0.01))
  (let ((end-time (+ (get-internal-real-time) (* maxtime internal-time-units-per-second))))
    (loop with event-p = nil do
       (setq event-p (driver-process-next-event (server-driver server)
                                                server
                                                :timeout 0.01))
       while (and event-p
                  (< (get-internal-real-time) end-time)))
    (when (> (get-internal-real-time) end-time)
      (log:info "event time exceded"))))

(defun server-loop-step (server &key (min-loop-time 0.01))
  (let ((end-time (+ (get-internal-real-time) (* min-loop-time internal-time-units-per-second))))
    (process-next-driver-events server)
    (command-subloop server)
    (refresh-kwindows server)
    (driver-force-output (server-driver server))
    (let ((wait-time (- end-time (get-internal-real-time))))
      (when (> wait-time 0)
        (sleep (/ wait-time internal-time-units-per-second))))))

(defun server-loop-fn (server)
  (driver-start (server-driver server))  
  (block loop
    (let ((*kernel-mode* t))
      (loop
         (with-simple-restart
             (restart-event-loop
              "restart clim's kernel loop.")
           (loop
              (server-loop-step server)
              (with-slots (shutdown-p) server
                (when shutdown-p
                  (return-from loop)))))))
    (with-slots (thread shutdown-p) server
      (setf thread nil
            shutdown-p nil))))


  
;;;
;;; Find server
;;;

(defvar *default-server-path* nil)

(defvar *server-path-search-order*
  '(:sdl2 :clx :null))

(defun find-default-server-path ()
  (loop for server in *server-path-search-order*
     if (get server :server-class)
     do (return-from find-default-server-path (list server))
     finally (error "No CLDK servers have been loaded!")))

(defun find-server (&key (server-path *default-server-path*))
  (if (null server-path)
      (setq server-path (find-default-server-path)))
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


(defmacro <d+ (server fn &rest args)
  `(push-command ,server (list :fn ,fn (list (server-driver ,server) ,@args)) :block-p t))

(defmacro <d- (server fn &rest args)
  `(push-command ,server (list :fn ,fn (list (server-driver ,server) ,@args)) :block-p nil))


(defun screen-num (server)
  (<d+ server #'driver-screen-num))

(defun screen-size (server &optional (screen-index nil) (units :device))
  (<d+ server #'driver-screen-size screen-index units))

(defun screen-dpi (server &optional (screen-index nil))
  (<d+ server #'driver-screen-dpi screen-index))

(defun screen-pointer-position (server)
  (<d+ server #'driver-screen-pointer-position))

(defun avaiable-cursor-names (server)
  (<d+ server #'driver-avaiable-cursor-names))
