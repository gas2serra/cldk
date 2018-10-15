(in-package :clim-fb)

(defclass fb-mirror (image-mirror-mixin cldk:buffered-window)
  ((width :initform 0)
   (height :initform 0)
   (cldk-mcclim-render-internals::image-medium :initform :two-dim-array)
   (img :initform nil)))

(defmethod destroy-mirror ((port fb-port) (sheet mirrored-sheet-mixin))
  (when (port-lookup-mirror port sheet)
    (port-unregister-mirror port sheet (sheet-mirror sheet))))

(defmethod raise-mirror ((port fb-port) (sheet basic-sheet))
  (cldk:raise-window (sheet-mirror sheet)))

(defmethod bury-mirror ((port fb-port) (sheet basic-sheet))
  (cldk:bury-window (sheet-mirror sheet)))

(defmethod mirror-transformation ((port fb-port) mirror)
  (multiple-value-bind (x y)
      (cldk:window-position mirror)
    (make-translation-transformation x y)))

(defmethod port-enable-sheet ((port fb-port) (mirror mirrored-sheet-mixin))
  (cldk:show-window (sheet-direct-mirror mirror))
  (clim:repaint-sheet mirror clim:+everywhere+))

(defmethod port-disable-sheet ((port fb-port) (mirror mirrored-sheet-mixin))
  (cldk:hide-window (sheet-direct-mirror mirror)))

#|
(defmethod port-mirror-width ((port fb-port) sheet)
  (multiple-value-bind (w h)
      (cldk:window-size (sheet-direct-mirror sheet))
    w))

(defmethod port-mirror-height ((port fb-port) sheet)
  (multiple-value-bind (w h)
      (cldk:window-size (sheet-direct-mirror sheet))
    h))
|#
;;;
;;;
;;;

;;; for port
(defmethod cldk-mcclim-render-internals::%create-mirror-image :after ((sheet fb-mirror) w h)
  (with-slots (cldk-mcclim-render-internals::dirty-region) sheet
    (setf cldk-mcclim-render-internals::dirty-region nil))
    (with-slots (width height img) sheet
      (setf width w
	    height h
            img (make-instance 'cldk:rgb-image
                                :pixels (image-pixels (image-mirror-image sheet))
                                :width w
                                :height h))))

(defun image-mirror-pre-put (width height mirror dirty-r)
  (let ((pixels (image-pixels (image-mirror-image mirror))))
    (declare (type cldk-mcclim-render-internals::rgb-image-pixels pixels))
    (let ((rs  nil)
          (cldki::*epsilon* 2))
      (map-over-region-set-regions
       #'(lambda (region)
           (clim:with-bounding-rectangle* (min-x min-y max-x max-y)
               (region-intersection region
                                    (make-rectangle* 0 0 (1- width) (1- height)))
             (setf rs (cldki::rectangle-set-union
                       rs
                       (cldki::rectangle->rectangle-set
                        (max 0 min-x)
                        (max 0 min-y)
                        (max 0 max-x)
                        (max 0 max-y))))))
       dirty-r)
      (with-slots (img) mirror
        (let ((dimg (cldki::window-obuffer mirror)))
          (when dimg
            (cldki::copy-image* img rs dimg 0 0)))
        #+nil (let ((dimg (cldk:buffered-window-image mirror)))
          (when dimg
            (cldki::copy-image* img rs (cldk:buffered-window-image mirror) 0 0)))))))

(defmethod cldk-mcclim-render-internals::%mirror-force-output ((mirror fb-mirror))
  (with-slots (cldk-mcclim-render-internals::image-lock
               cldk-mcclim-render-internals::dirty-region
               dirty-xr width height)
      mirror
    (when cldk-mcclim-render-internals::dirty-region
      (climi::with-lock-held (cldk-mcclim-render-internals::image-lock)
        (when cldk-mcclim-render-internals::dirty-region
          (image-mirror-pre-put width height mirror cldk-mcclim-render-internals::dirty-region)
          (setf cldk-mcclim-render-internals::dirty-region nil))))))
