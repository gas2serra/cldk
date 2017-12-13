(in-package :cldk-internals)

;;;
;;; kernel mixin
;;;

(defclass kernel-mixin ()
  ((driver :initarg :driver
           :reader driver)
   (driver-object-id->server-object :initform (make-hash-table))
   (shutdown-p :initform nil
               :reader kernel-shutdown-p)))

(defgeneric register-server-object (kernel driver-object server-object)
  (:method ((kernel kernel-mixin) driver-object server-object)
    (with-slots (driver-object-id->server-object) kernel
      (setf (gethash (driver-object-id driver-object) driver-object-id->server-object)
            server-object))))

(defgeneric unregister-server-object (kernel driver-object)
  (:method ((kernel kernel-mixin) driver-object)
    (with-slots (driver-object-id->server-object) kernel
      (setf (gethash (driver-object-id driver-object) driver-object-id->server-object)
            nil))))

(defgeneric lookup-server-object (kernel driver-object-id)
  (:method ((kernel kernel-mixin) driver-object-id)
    (with-slots (driver-object-id->server-object) kernel
      (gethash driver-object-id driver-object-id->server-object))))

;;;
;;; call and callback
;;;

(defgeneric call (kernel command &key block-p))
(defgeneric exec-call (kernel command))
(defgeneric callback (kernel command &key block-p))
(defgeneric exec-callback (kernel command))

(defun make-command (fn args)
  (list :fn fn args))

(defun make-stop-command ()
  (list :stop))

(defmacro <call+ (kernel fn &rest args)
  `(call ,kernel (make-command ,fn (list ,@args)) :block-p t))

(defmacro <call- (kernel fn &rest args)
  `(call ,kernel (make-command ,fn (list ,@args)) :block-p nil))

(defmacro <callback+ (kernel fn &rest args)
  `(callback ,kernel (make-command ,fn (list ,@args)) :block-p t))

(defmacro <callback- (kernel fn &rest args)
  `(callback ,kernel (make-command ,fn (list ,@args)) :block-p nil))

;;;
;;; boot/shutdown
;;;

(defgeneric boot (kernel))

(defmethod boot ((kernel kernel-mixin))
  (driver-start (driver kernel)))

(defgeneric shutdown (kernel))

(defmethod shutdown :before ((kernel kernel-mixin))
  (with-slots (shutdown-p) server
    (setf shutdown-p t)))

(defmethod shutdown ((kernel kernel-mixin))
  (driver-stop (driver kernel)))

(defmethod shutdown :after ((kernel kernel-mixin))
  (with-slots (shutdown-p) server
    (setf shutdown-p nil)))

;;;
;;; kernel object mixin
;;;

(defclass kernel-object-mixin ()
  ((kernel :initform nil
           :initarg :kernel
           :reader kernel)))

(defmethod driver ((object kernel-object-mixin))
  (driver (kernel object)))


