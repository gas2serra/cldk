(in-package :clim-fb)

(defclass fb-mirror (image-mirror-mixin cldk:buffered-window)
  ((width :initform 0)
   (height :initform 0)
   (xlib-image :initform nil)
   (buffer :initarg buffer)
   (dirty-xr :initform +nowhere+)
   (skip-count :initform 0)))

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

(defmethod port-mirror-width ((port fb-port) sheet)
  (multiple-value-bind (w h)
      (cldk:window-size (sheet-direct-mirror sheet))
    w))

(defmethod port-mirror-height ((port fb-port) sheet)
  (multiple-value-bind (w h)
      (cldk:window-size (sheet-direct-mirror sheet))
    h))

;;;
;;;
;;;

;;; for port
(defmethod mcclim-render-internals::%create-mirror-image :after ((sheet fb-mirror) w h)
  (with-slots (mcclim-render-internals::dirty-region) sheet
    (setf mcclim-render-internals::dirty-region nil))
  ;;(let ((data (mcclim-render::image-pixels (image-mirror-image sheet))))
    (with-slots (width height xlib-image buffer) sheet
      (setf width w
	    height h)
      (setf xlib-image (make-array (list height width)
                                   :element-type '(unsigned-byte 32)
                                   :initial-element #x00FFFFFF))
      ))

(defgeneric image-mirror-to-x (sheet))

(defmethod image-mirror-to-x ((sheet image-mirror-mixin))
  )

(defun image-mirror-put (width height window xlib-image dirty-r)
  #+nil (map-over-region-set-regions #'(lambda (region)
				   (clim:with-bounding-rectangle* (min-x min-y max-x max-y)
				     (region-intersection region
							  (make-rectangle* 0 0 width height))
				     (let ((w (round (- max-x min-x)))
					   (h (round (- max-y min-y))))
				       (when (and window xlib-image)
					 (cldk:copy-image-to-buffered-window
                                          window
                                          (make-instance 'cldk:image
                                                         :pixels xlib-image
                                                         :width width
                                                         :height height)
                                          (round (max min-x 0))
                                          (round (max min-y 0))
                                          w 
                                          h 
                                          (round (max min-x 0))
                                          (round (max min-y 0))
                                          )))))

                               dirty-r)
  #+nil (when dirty-r
    (cldk:flush-buffered-window window)))

(declaim (inline xlib-image-data-set-pixel))
(defun xlib-image-data-set-pixel (data x y red green blue)
  (setf (aref data y x)
	(dpb blue (byte 8 0)
	     (dpb green (byte 8 8)
		  (dpb red (byte 8 16) 0)))))

(defun image-mirror-pre-put (width height window sheet xlib-image dirty-r)
  (let ((pixels (image-pixels (image-mirror-image sheet))))
    (declare (type opticl-rgb-image-pixels pixels))
    (let ((rs  nil))
      (map-over-region-set-regions
       #'(lambda (region)
           (clim:with-bounding-rectangle* (min-x min-y max-x max-y)
                                          (region-intersection region (make-rectangle* 0 0 (1- width) (1- height)))
                                          (setf rs (cldki::rectangle-set-union
                                                    rs
                                                    (cldki::rectangle->rectangle-set
                                                     (max 0 min-x)
                                                     (max 0 min-y)
                                                     (max 0 max-x)
                                                     (max 0 max-y))))
                                          #+nil (log:info "pre: " min-x min-y max-x max-y window)
                                          (when (and window)
                                            (opticl:do-region-pixels (y x min-y min-x max-y max-x)
                                                pixels
                                              (multiple-value-bind (red green blue)
                                                  (opticl:pixel pixels y x)
                                                (xlib-image-data-set-pixel xlib-image x y red green blue))))))
       dirty-r)
      (cldki::copy-image-to-buffered-window* window (make-instance 'cldk:image
                                                           :pixels xlib-image
                                                           :width width
                                                           :height height)
                                           rs))))
    #+nil (map-over-region-set-regions #'(lambda (region)
                                     (clim:with-bounding-rectangle* (min-x min-y max-x max-y)
                                       (region-intersection region
                                                            (make-rectangle* 0 0 width height))
                                       (let ((w (round (- max-x min-x)))
                                             (h (round (- max-y min-y))))
                                         (when (and window xlib-image)
                                           (cldk:copy-image-to-buffered-window
                                            window
                                            (make-instance 'cldk:image
                                                           :pixels xlib-image
                                                           :width width
                                                           :height height)
                                            (round (max min-x 0))
                                            (round (max min-y 0))
                                            w 
                                            h 
                                            (round (max min-x 0))
                                            (round (max min-y 0))
                                            )))))
                                 dirty-r)

(defmethod image-mirror-to-x ((sheet fb-mirror))
  (declare (optimize speed))
  (with-slots (xlib-image mcclim-render-internals::image-lock
		       mcclim-render-internals::dirty-region  mcclim-render-internals::finished-output MCCLIM-RENDER-INTERNALS::updating-p
		       width height dirty-xr skip-count)
      sheet
    (when (not (region-equal dirty-xr +nowhere+))
      (let ((reg))
        (climi::with-lock-held (mcclim-render-internals::image-lock)
          (setf reg dirty-xr)
          (setf dirty-xr +nowhere+))
        (image-mirror-put width height sheet xlib-image reg)))))




(defmethod mcclim-render-internals::%mirror-force-output ((mirror fb-mirror))
  (with-slots (mcclim-render-internals::image-lock mcclim-render-internals::dirty-region dirty-xr width height 
                          xlib-image)
      mirror
    (when mcclim-render-internals::dirty-region
      (climi::with-lock-held (mcclim-render-internals::image-lock)
        (when mcclim-render-internals::dirty-region
          (setf dirty-xr (region-union dirty-xr mcclim-render-internals::dirty-region))
          (image-mirror-pre-put width height mirror mirror xlib-image dirty-xr)
          (setf mcclim-render-internals::dirty-region nil))))))
