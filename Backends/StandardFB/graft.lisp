(in-package :clim-fb)

(defclass fb-graft (cldk-graft-mixin
                    standard-graft
                    )
                    
  ())

#|
(defmethod graft-width ((graft fb-graft) &key (units :device))
  (multiple-value-bind (w h)
      (cldk:screen-size (fb-port-server (port graft)) units)
    w))

(defmethod graft-height ((graft fb-graft) &key (units :device))
  (multiple-value-bind (w h)
      (cldk:screen-size (fb-port-server (port graft)) units)
    h))

|#
