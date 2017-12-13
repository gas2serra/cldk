(in-package :cldk-internals)

(defclass single-thread-server (server)
  ((kernel-thread :initform nil
           :reader server-kernel-thread)
   (callback-thread :initform nil
                 :reader server-callback-thread)))

;;; start/stop

(defun server-running-p (server)
  (server-kernel-thread server))

(defun start-server (server)
  (when (server-running-p server)
    (error "cldk server is already running"))
  (with-slots (kernel-thread callback-thread) server
    (setf kernel-thread
          (bt:make-thread #'(lambda ()
                              (kernel-loop-fn server))
                          :name "cldk server"))
    (setf callback-thread (bt:make-thread #'(lambda ()
                                           (callback-loop-fn server))
                                       :name "cldk callback server"))))

(defun stop-server (server)
  (unless (server-running-p server)
    (error "cldk server is already stopped"))
  (call server (make-stop-command) :block-p t)
  (callback server (make-stop-command) :block-p t))

(defun kill-server (server)
  (unless (server-running-p server)
    (error "server is not running"))
  (bt:destroy-thread (server-kernel-thread server))
  (bt:destroy-thread (server-callback-thread server))
  (with-slots (kernel-thread callback-thread) server
    (setf kernel-thread nil
          callback-thread nil)))

(defgeneric restart-server (server)
  (:method ((server single-thread-server))
    (stop-server server)
    (start-server server)))

(defgeneric destroy-server (server)
  (:method ((server single-thread-server))
    (unwind-protect
         (when (and 
                (server-running-p server)
                (bt:thread-alive-p (server-kernel-thread server)))
           ;; destroy all cursors
           (stop-server server))
      (when (and (server-running-p server)
                 (bt:thread-alive-p (server-kernel-thread server)))
        (kill-server server))
      (setf *all-servers* (remove server *all-servers*)))))
