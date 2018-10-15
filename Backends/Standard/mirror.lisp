(in-package :cldk-backend)

(defclass cldk-mirror-mixin ()
  ())
#|
(defmethod clim:destroy-mirror ((port cldk-port-mixin) (sheet cldk-mirrored-sheet-mixin))
  (when (climi::port-lookup-mirror port sheet)
    (climi::port-unregister-mirror port sheet (clim:sheet-direct-mirror sheet))))

(defmethod climi::raise-mirror ((port cldk-port-mixin) (sheet cldk-mirrored-sheet-mixin))
  (let ((mirror (clim:sheet-direct-mirror sheet)))
    (within-kernel-mode ((driver mirror) :block-p nil)
      (driver-raise-window (driver mirror) mirror))))

(defmethod climi::bury-mirror ((port cldk-port-mixin) (sheet cldk-mirrored-sheet-mixin))
  (let ((mirror (clim:sheet-direct-mirror sheet)))
    (within-kernel-mode ((driver mirror) :block-p nil)
      (driver-bury-window (driver mirror) mirror))))

(defmethod clim-backend:port-enable-sheet ((port cldk-port-mixin) (sheet cldk-mirrored-sheet-mixin))
  (let ((mirror (clim:sheet-direct-mirror sheet)))
    (within-kernel-mode ((driver mirror) :block-p nil)
      (driver-show-window (driver mirror) mirror))
    (clim:repaint-sheet sheet clim:+everywhere+)))

(defmethod clim-backend:port-disable-sheet ((port cldk-port-mixin) (sheet cldk-mirrored-sheet-mixin))
  (let ((mirror (clim:sheet-direct-mirror sheet)))
    (within-kernel-mode ((driver mirror) :block-p nil)
      (driver-hide-window (driver mirror) mirror))))

(defmethod clim-backend:mirror-transformation ((port cldk-port-mixin) (mirror cldk-mirror-mixin))
  (multiple-value-bind (x y)
      (cldk:window-position mirror)
    (clim:make-translation-transformation x y)))
|#
