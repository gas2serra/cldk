(in-package :cldk-backend)

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
         (return-from loop)))))
