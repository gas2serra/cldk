(in-package :cldk-backend)

(defclass cldk-port-mixin (standard-port)
  ())

(defgeneric port-display-driver (port))
(defgeneric port-display-mirror (port))

(defmethod port-display-driver ((port cldk-port-mixin))
  port)

(defmethod port-display-mirror ((port cldk-port-mixin))
  port)

(defmethod initialize-instance :after ((port cldk-port-mixin) &rest args)
  (declare (ignore args))
  (setf (driver-options (port-display-driver port))
        (cons :id (clim:port-server-path port)))
  (cldki::start-driver port)
  (setf (cldk:event-handler port)
        (make-instance 'cldk-event-handler :port port)))

(defgeneric port-graft-class (port))

(defmethod clim-backend:make-graft ((port cldk-port-mixin)
                                    &key (orientation :default) (units :device))
  (let ((graft (make-instance (port-graft-class port)
                              :driver (port-display-driver port)
                              :port port
                              :mirror nil
                              :orientation orientation
                              :units units)))
    (push graft (port-grafts port))
    graft))

(defmethod clim:destroy-port ((port cldk-port-mixin))
  (cldki::destroy-driver (port-display-driver port)))

(defmethod port-force-output :after ((port cldk-port-mixin))
  (cldki::driver-force-output (port-display-driver port)))
