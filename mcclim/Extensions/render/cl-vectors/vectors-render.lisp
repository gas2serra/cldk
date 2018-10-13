(in-package :cldk-mcclim-render-internals)

(defclass vectors-image-render (image-render)
  ((state :initform (aa:make-state))))

(defgeneric render-fill-paths (render image paths))
(defgeneric render-stroke-paths (render image paths))

(defmethod render-fill-paths ((render vectors-image-render) image paths)
  (format *debug-io* "render: fill paths (aa) ~A~%" (render-pixeled-design-ink render))
  (with-slots (state) render
    (aa-fill-paths image
                   (render-pixeled-design-ink render)
                   paths
                   state
                   (render-transformation render)
                   (render-clip-region render))))

(defmethod render-stroke-paths ((render vectors-image-render) image paths)
  ;;(format *debug-io* "render: stroke paths (aa) ~A~%" image)
  (with-slots (state) render
    (aa-stroke-paths image
                     (render-pixeled-design-ink render)
                     paths
                     (render-line-style render)
                     state
                     (render-transformation render)
                     (render-clip-region render))))

(defmethod render-draw-line* ((render vectors-image-render)
                              image x1 y1 x2 y2)
  ;;(format *debug-io* "render: draw line (aa)~%")
  (let ((path (make-path x1 y1)))
    (line-to path x2 y2)
    (render-stroke-paths render image
                         (list path))))

(defmethod render-draw-point* ((render vectors-image-render)
                               image x y)
  ;;(format *debug-io* "render: draw point (aa)~%")
  (let ((path (arc x y
		   (max 1 (/ (line-style-thickness (render-line-style render)) 2))
		   pi
		   (+ pi (* 2 pi)))))
    (render-fill-paths render image
                       (list path))))

(defmethod render-draw-circle* ((render vectors-image-render) image
                                center-x center-y radius start-angle end-angle
				filled)
  (format *debug-io* "!!!! render: draw circle (aa)~%")
  (let ((path (arc center-x center-y radius (+ pi start-angle) (+ pi end-angle))))
    (if filled
	(render-fill-paths render image
                           (list path))
	(render-stroke-paths render image
                             (list path)))))

(defmethod render-draw-poligon* ((render vectors-image-render) image coord-seq closed filled)
  (format *debug-io* "render: draw poligon (aa) ~A ~A~%" coord-seq filled)
  (let ((x (elt coord-seq 0))
	(y (elt coord-seq 1)))	  
    (let ((path (make-path x y)))
      (do ((v 2 (+ 2 v)))
	  ((>= v (length coord-seq)))
	(let ((x (elt coord-seq v))
	      (y (elt coord-seq (1+ v))))
	  (line-to path x y)))
      (when closed
	(close-path path))
      (if filled
          (render-fill-paths render image (list path))
	  (render-stroke-paths render image (list path))))))
  
(defmethod render-draw-ellipse* ((render vectors-image-render) image center-x center-y
				 radius-1-dx radius-1-dy
				 radius-2-dx radius-2-dy
				 start-angle end-angle filled
                                 &aux (el (make-ellipse*
                                           center-x center-y
                                           radius-1-dx radius-1-dy
                                           radius-2-dx radius-2-dy
                                           :start-angle start-angle
                                           :end-angle end-angle)))
  ;;(format *debug-io* "render: draw ellipse (aa)~%")
  (multiple-value-bind (cx cy hx hy theta) (climi::ellipse-simplified-representation el)
    (declare (ignorable cx cy))
    (let* ((sa (- (* 2 pi) end-angle theta))
           (dalpha (- end-angle start-angle))
           (path (ellipse-arc center-x center-y hx hy theta
                              sa (+ sa dalpha))))
      (when filled
        (line-to path center-x center-y))
      (if filled
          (render-fill-paths render image (list path))
	  (render-stroke-paths render image (list path))))))

(defmethod render-draw-rectangle-using-ink* ((render vectors-image-render)
                                             image left top right bottom filled)
  ;;(format *debug-io* "render: draw rectangles (aa)~%")
  (if (< right left)
      (rotatef left right))
  (if (< bottom top)
      (rotatef top bottom))
  (let* ((region
	  (region-intersection
	   (render-clip-region render)
	   (transform-region (render-transformation render)
			     (make-rectangle* left top right bottom)))))
    (clim:with-bounding-rectangle* (min-x min-y max-x max-y)
	region
      (if (and
	   filled
	   (not (typep (render-pixeled-design-ink render) 'pixeled-flipping-design))
	   (clim:rectanglep region))
	  (progn
	    (render-fill-image render image
                               (render-pixeled-design-ink render)
                               nil
                               :x min-x :y min-y :width (- max-x min-x) :height (- max-y min-y)))
	  (let ((path (make-path left top)))
	    (line-to path right top)
	    (line-to path right bottom)
	    (line-to path left bottom)
	    (close-path path)
            (if filled
                (render-fill-paths render image (list path))
	        (render-stroke-paths render image (list path))))))))
  
(defmethod render-draw-text* ((render vectors-image-render) image string x y
                              start end
                              align-x align-y
                              toward-x toward-y transform-glyphs
                              transformation)
  ;;(format *debug-io* "render: draw text (aa)~%")
  (let ((reg clim:+nowhere+))
    (multiple-value-bind (x y)
        (transform-position transformation x y)
      (flet ((draw-font-glypse (paths opacity-image dx dy transformation)
               (declare (ignore paths))
	       (multiple-value-bind (x1 y1)
		   (transform-position
		    (clim:compose-transformations transformation
						  (render-transformation render))
                    
		    (+ dx ) (-  dy))
	         (clim:with-bounding-rectangle* (min-x min-y max-x max-y)
		     (region-intersection
		      (render-clip-region render)
		      (make-rectangle* x1 y1 (+ -1 x1
                                                (image-width opacity-image))
                                       (+ -1 y1 (image-height opacity-image))))
		   (setf reg (clim:region-union
                              reg
                              (render-fill-image
		               render
                               image
                               (render-pixeled-design-ink render)
		               opacity-image
		               :x (round min-x) :y (round min-y)
		               :width (- max-x min-x) :height (- max-y min-y)
		               :stencil-dx (- (round x1)) :stencil-dy (- (round y1)))))))))
        (let ((xfont (render-text-font render)))
          (let ((size (text-style-size (render-text-style render))))
            (setf size   (or size :normal)
	          size (getf *text-sizes* size size))
            (when (characterp string)
	      (setq string (make-string 1 :initial-element string)))
            (when (null end) (setq end (length string)))
            (multiple-value-bind (text-width text-height x-cursor y-cursor baseline)
	        (render-text-size render string
                                  (render-text-style render)
                                  (render-text-font render)
                                  :start start :end end)
	      (declare (ignore x-cursor y-cursor))
	      (unless (and (eq align-x :left) (eq align-y :baseline))
	        (setq x (- x (ecase align-x
			       (:left 0)
			       (:center (round text-width 2))
			       (:right text-width))))
	        (setq y (ecase align-y
		          (:top (+ y (- baseline text-height)
			           (+ text-height)))
		          (:center (+ y (- baseline text-height)
				      (+ (floor text-height 2))))
		          (:baseline y)
		          (:bottom (+ y (- baseline text-height))))))
	      (string-primitive-paths x y string xfont size #'draw-font-glypse)))))
      reg)))

(defmethod render-draw-bezier-design ((render image-render) image design filled)
  ;;(format *debug-io* "render: draw bezier (aa)~%")
  (let ((segments (mcclim-bezier:segments design)))
    (let ((p0 (slot-value (elt segments 0) 'mcclim-bezier:p0)))
      (let ((path (make-path (point-x p0) (point-y p0))))
        (map nil (lambda (segment)
                   (with-slots (mcclim-bezier:p1 mcclim-bezier:p2 mcclim-bezier:p3) segment
                     (curve-to path
                               (point-x mcclim-bezier:p1) (point-y mcclim-bezier:p1)
                               (point-x mcclim-bezier:p2) (point-y mcclim-bezier:p2)
                               (point-x mcclim-bezier:p3) (point-y mcclim-bezier:p3))))
             segments)
        (if filled
                (render-fill-paths render image (list path))
	        (render-stroke-paths render image (list path)))))))
