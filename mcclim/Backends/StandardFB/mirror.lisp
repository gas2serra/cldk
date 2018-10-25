(in-package :clim-fb)

(defclass fb-mirror (image-mirror-mixin  cldk-buffered-mirror-mixin)
  ((width :initform 0)
   (height :initform 0)
   (cldk-render-internals::image-medium :initform :two-dim-array)))

;;;
;;;
;;;

;;; for port
(defmethod cldk-render-internals::%create-mirror-image :after ((sheet fb-mirror) w h)
  (with-slots (cldk-render-internals::dirty-region) sheet
    (setf cldk-render-internals::dirty-region nil))
  (with-slots (width height) sheet
      (setf width w
	    height h)))

(defun image-mirror-pre-put (width height mirror dirty-r)
  (let ((pixels (image-pixels (image-mirror-image mirror))))
    (declare (type cldk-render-internals::rgb-image-pixels pixels))
    (let ((rs  nil)
          (cldki::*epsilon* 2))
      (map-over-region-set-regions
       #'(lambda (region)
           (clim:with-bounding-rectangle* (min-x min-y max-x max-y)
               (region-intersection region
                                    (make-rectangle* 0 0 (- (min width) 2) (- (min height) 2)))
             (setf rs (cldki::rectangle-set-union
                       rs
                       (cldki::rectangle->rectangle-set
                        (max 0 min-x)
                        (max 0 min-y)
                        (max 0 max-x)
                        (max 0 max-y))))))
       dirty-r)
      (cldki::with-buffered-window-locked (mirror)
        (cldki::copy-image-to-buffered-window (image-mirror-image mirror) rs
                                       mirror 0 0)))))

(defmethod cldk-render-internals::%mirror-force-output ((mirror fb-mirror))
  (with-slots (cldk-render-internals::image-lock
               cldk-render-internals::dirty-region
               dirty-xr width height)
      mirror
    (when cldk-render-internals::dirty-region
      (climi::with-lock-held (cldk-render-internals::image-lock)
        (when cldk-render-internals::dirty-region
          (when (image-mirror-pre-put width height mirror cldk-render-internals::dirty-region)
            (setf cldk-render-internals::dirty-region nil)))))))
