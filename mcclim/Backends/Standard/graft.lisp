(in-package :cldk-backend)

(defclass cldk-graft-mixin (standard-graft)
  ())

(defmethod initialize-instance :after ((graft cldk-graft-mixin) &rest args)
  (climi::%%set-sheet-region 
   (clim:make-bounding-rectangle 0 0 (clim:graft-width graft) (clim:graft-height graft))
   graft))

(defmethod clim:pointer-position ((graft cldk-graft-mixin))
  (let ((display-driver (port-display-driver (clim:port graft))))
    (within-kernel-mode (display-driver :block-p t)
      (driver-screen-pointer-position display-driver))))

(defmethod clim:graft-width ((graft cldk-graft-mixin) &key (units :device))
  (let ((mirror graft))
    (multiple-value-bind (w h)
        (cldk:root-size mirror :units units)
      (declare (ignore h))
      w)))

(defmethod clim:graft-height ((graft cldk-graft-mixin) &key (units :device))
  (let ((mirror graft))
    (multiple-value-bind (w h)
        (cldk:root-size mirror :units units)
      (declare (ignore w))
      h)))

(defmethod clim:dispatch-event ((graft cldk-graft-mixin) event)
  )
