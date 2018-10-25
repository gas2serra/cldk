(in-package :cldk-backend)

(defclass cldk-mirror-mixin (window)
  ())

(defmethod destroy-mirror ((port cldk-port-mixin) (sheet cldk-mirrored-sheet-mixin))
  (cldk:destroy-window (clim:sheet-direct-mirror sheet))
  (when (port-lookup-mirror port sheet)
    (port-unregister-mirror port sheet (clim:sheet-direct-mirror sheet))))
    

(defmethod raise-mirror ((port cldk-port-mixin) (sheet cldk-mirrored-sheet-mixin))
  (cldk:raise-window (clim:sheet-direct-mirror sheet)))

(defmethod bury-mirror ((port cldk-port-mixin) (sheet cldk-mirrored-sheet-mixin))
  (cldk:bury-window (clim:sheet-direct-mirror sheet)))

(defmethod mirror-transformation ((port cldk-port-mixin) (mirror cldk-mirrored-sheet-mixin))
  (break)
  (multiple-value-bind (x y)
      (cldk:window-position mirror)
    (clim:make-translation-transformation x y)))

(defmethod port-enable-sheet ((port cldk-port-mixin) (sheet cldk-mirrored-sheet-mixin))
  (cldk:show-window (clim:sheet-direct-mirror sheet))
  (clim:repaint-sheet sheet clim:+everywhere+))

(defmethod port-disable-sheet ((port cldk-port-mixin) (sheet cldk-mirrored-sheet-mixin))
  (cldk:hide-window (clim:sheet-direct-mirror sheet)))
