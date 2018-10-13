(in-package :cldk-mcclim-render-internals)

(defmethod medium-draw-image*
    ((medium sheet-with-medium-mixin) design x y)
  (medium-draw-image* (sheet-medium medium) design x y))

(defmethod medium-draw-image*
    ((medium clim-clx::clx-medium) (image image-mixin) x y)
  (draw-using-pixmap medium image x y nil :partial))

(defmethod medium-draw-image*
    ((medium clim-clx::clx-medium) (image rgba-image-mixin) x y)
  (draw-using-pixmap medium image x y t :partial))

(defmethod medium-draw-image*
    ((medium clim-clx::clx-medium) (image clx-image) x y)
  (draw-using-pixmap medium image x y nil :full))

(defmethod medium-draw-image*
    ((medium clim-clx::clx-medium) (image clx-rgba-image) x y)
  (draw-using-pixmap medium image x y t :full))

(defun draw-using-pixmap (medium image x y mask-p mode)
  (let* ((da (clim-clx::sheet-xmirror (medium-sheet medium)))
	 (width (image-width image))
	 (height (image-height image))
         (region
          (region-intersection
           (climi::medium-device-region medium)
           (transform-region (sheet-device-transformation (medium-sheet medium))
                             (make-rectangle* x y (+ x width) (+ y height))))))
    (flet ((compute-pixmap (x y w h mode)
             (let ((im
                    (if (eql mode :full)
                        (coerce-image image :auto medium)
                        (crop-image image x y w h :rgb medium))))
               (if (eql mode :full)
                   (clx-image->pixmap im x y w h)
                   (clx-image->pixmap im 0 0 w h)))))
      (multiple-value-bind (x1 y1)
          (transform-position
           (sheet-device-transformation (medium-sheet medium))
           x y)
        (clim:with-bounding-rectangle*
         (min-x min-y max-x max-y)
         region
         (let ((x0 (max 0 (round (+ 0 (- min-x x1)))))
               (y0 (max 0 (round (+ 0 (- min-y y1)))))
               (w (round (- max-x min-x)))
               (h (round (- max-y min-y))))
           (when (and (> w 0) (> h 0))
             (let ((pixmap (compute-pixmap x0 y0 w h mode))
                   (gcontext (xlib:create-gcontext :drawable da))
                   (mask nil))
               (when mask-p
                 (setf mask (rgba-image->pixmap-mask medium image x0 y0 w h))
                 (setf (xlib:gcontext-clip-mask gcontext) mask
                       (xlib:gcontext-clip-x gcontext) (round min-x)
                       (xlib:gcontext-clip-y gcontext) (round min-y)))
               (xlib:copy-area pixmap gcontext
                               0 0 w h
                               da
                               (round min-x) (round min-y))
               (xlib:free-gcontext gcontext)
               (when mask
                 (xlib:free-pixmap mask))
               (xlib:free-pixmap pixmap)))))))))
