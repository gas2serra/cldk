(in-package :cldk-render-internals)

(defclass image-render ()
  ((transformation :accessor render-transformation)
   (clip-region :accessor render-clip-region)
   (pixeled-design-ink :accessor render-pixeled-design-ink)
   (line-style :accessor render-line-style)
   (text-style :initform nil :accessor render-text-style)
   (text-font :accessor render-text-font)))

(defgeneric render-fill-image (render image design stencil
                               &key x y width height stencil-dx stencil-dy))
(defgeneric render-draw-image (render image src-img x y width height to-x to-y))

(defgeneric render-draw-line* (render image x1 y1 x2 y2))
(defgeneric render-draw-point* (render image x y))
(defgeneric render-draw-circle* (render image center-x center-y radius start-angle end-angle
				 filled))
(defgeneric render-draw-poligon* (render image coord-seq closed filled))
(defgeneric render-draw-ellipse* (render image center-x center-y
				 radius-1-dx radius-1-dy
				 radius-2-dx radius-2-dy
				  start-angle end-angle filled))
(defgeneric render-draw-rectangle-using-ink* (render image left top right bottom filled))
(defgeneric render-draw-text* (render image string x y
                               start end
                               align-x align-y
                               toward-x toward-y transform-glyphs
                               transformation))
(defgeneric render-draw-image* (render image src-image
                                to-x to-y))
(defgeneric render-copy-area (render image to-x to-y width height
                              img-src
                              from-x from-y))
(defgeneric render-draw-bezier-design (render image design filled))

(defmethod render-fill-image ((render image-render) image design stencil
                              &key x y width height (stencil-dx 0) (stencil-dy 0))
  #+nil (format *debug-io* "render: fill image (stencil ~A) ~A ~%~A~%" (not (null stencil))
          (render-transformation render)
          (render-clip-region render))
  (fill-image image design stencil
              :x x :y y :width width :height height :stencil-dx stencil-dx :stencil-dy stencil-dy))

(defmethod render-draw-image ((render image-render) image (src-img rgba-image-mixin)
                              x y width height to-x to-y)
  ;;(format *debug-io* "render: draw image (blend)~%")
  (blend-image src-img x y width height
               image
               to-x to-y :alpha 255))

(defmethod render-draw-image ((render image-render) image (src-img image-mixin)
                              x y width height to-x to-y)
  ;;(format *debug-io* "render: draw image (copy)~%")
  (copy-image src-img x y width height
               image
               to-x to-y))

(defmethod render-draw-image* ((render image-render) image src-img
                               to-x to-y)
  (render-draw-image* render image (coerce-image src-img :auto (image-medium image))
                      to-x to-y))

(defmethod render-draw-image* ((render image-render) image (src-img image-mixin)
                               to-x to-y)
  (let* ((width (image-width src-img))
	 (height (image-height src-img))
         (region
          (region-intersection
           (render-clip-region render)
           (transform-region (render-transformation render)
                             (make-rectangle* to-x to-y (+ to-x width) (+ to-y height))))))
    (clim:with-bounding-rectangle* (min-x min-y max-x max-y)
        region
      (if (clim:rectanglep region)
          (multiple-value-bind (x1 y1)
              (transform-position
               (render-transformation render)
               to-x to-y)
            (render-draw-image render image
                               (if (typep src-img 'image)
                                   src-img
                                   (coerce-image src-img :rgb :two-dim-array))
                               (+ 0 (- min-x x1))
                               (+ 0 (- min-y y1))
                               (- max-x min-x)
                               (- max-y min-y)
                               min-x min-y))
          (progn
            (setf (render-pixeled-design-ink render)
                  (make-pixeled-design 
                   (climi::transform-region
                    (make-translation-transformation
                     to-x to-y)
                    (make-image-design src-img))))
            (render-draw-rectangle-using-ink* render image
                                              to-x to-y
                                              (+ to-x width) (+ to-y height)
                                              t))))))

