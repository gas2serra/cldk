(in-package :cldk-backend)

(defclass cldk-port-mixin (standard-port)
  ())

(defgeneric port-display-driver (port))
(defgeneric port-display-mirror (port))

(defgeneric port-graft-mirror-class (port))
(defgeneric port-graft-class (port))

(defmethod clim-backend:make-graft ((port cldk-port-mixin)
                                    &key (orientation :default) (units :device))
  (let ((graft (make-instance (port-graft-class port)
                              :driver (port-display-driver port)
                              :port port
                              :mirror nil
                              :orientation orientation
                              :units units)))
    (multiple-value-bind (w h)
        (cldk:screen-size (port-display-driver port) units)
      (climi::%%set-sheet-region 
       (clim:make-bounding-rectangle 0 0 w h)
       graft))
    (push graft (port-grafts port))
    graft))
