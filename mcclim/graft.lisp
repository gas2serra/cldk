(in-package :clim-fb)

(defclass fb-graft (standard-graft) ())

(defmethod graft-width ((graft fb-graft) &key (units :device))
  (multiple-value-bind (w h)
      (cldk:screen-size (fb-port-server (port graft)) 0 units)
    w))

(defmethod graft-height ((graft fb-graft) &key (units :device))
  (multiple-value-bind (w h)
      (cldk:screen-size (fb-port-server (port graft)) 0 units)
    h))

