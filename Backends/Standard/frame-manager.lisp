(in-package :cldk-backend)

(defclass cldk-frame-manager-mixin ()
  ())

#|
(defmethod clim:disown-frame :after ((fm cldk-frame-manager-mixin)
                                     (frame clim:application-frame))
  (let ((thread (climi::frame-process frame)))
    (when thread
      (bt:destroy-thread thread))))

|#
