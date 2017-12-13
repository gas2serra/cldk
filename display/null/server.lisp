(in-package :cldk-null)

(defclass null-server (display-server)
  ())

(defmethod initialize-instance :after ((server null-server) &rest args)
  (declare (ignore args))
  (with-slots (driver) server
    (setf driver (make-instance 'null-driver :options (server-path server))))
  (start-server server))

;;;
;;; server-path parser
;;;

(defun parse-null-server-path (path)
  path)

(setf (get :null :server-class) 'null-server)
(setf (get :null :server-path-parser-fn) 'parse-null-server-path)
