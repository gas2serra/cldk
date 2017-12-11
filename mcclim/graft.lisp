(in-package :clim-fb)

(defclass fb-graft (standard-graft) ())

(defmethod graft-width ((graft fb-graft) &key (units :device))
  (first (cldk:screen-size (fb-port-server (port graft)) 0 units)))

(defmethod graft-height ((graft fb-graft) &key (units :device))
  (second (cldk:screen-size (fb-port-server (port graft)) 0 units)))

