(in-package :cldk-render-internals)

(defclass render-medium-mixin (basic-medium)
  ((render :initform (make-instance 'vectors-image-render) :reader medium-render)))

(defmethod initialize-instance :after ((medium render-medium-mixin)
				       &rest initargs)
  (declare (ignore initargs))
  nil)

#+nil (defmethod (setf medium-text-style) :before (text-style (medium render-medium-mixin))
  (let ((render (medium-render medium)))
    (let ((old-text-style (render-text-style render)))
      (unless (eql old-text-style text-style)
        (setf (render-text-font render)
              (text-style-to-font
               (port medium) (medium-text-style medium)))))))

(defun medium-text-font (medium)
  (render-text-font (medium-render medium)))

;;;
;;; protocols
;;;

(defgeneric %medium-pixeled-design (medium))
(defgeneric %medium-update-render (medium))

(defmethod %medium-pixeled-design ((medium render-medium-mixin))
  (make-pixeled-design (transform-region (sheet-native-transformation
                                          (medium-sheet medium))
				         (medium-ink medium))
                       :background (medium-background medium)
                       :foreground (medium-foreground medium)))

(defmethod %medium-update-render ((medium render-medium-mixin))
  (let* ((msheet (sheet-mirrored-ancestor (medium-sheet medium)))
         (render (medium-render medium)))
    (setf (render-transformation render) (sheet-native-transformation (medium-sheet medium))
          (render-clip-region render) (climi::medium-device-region medium)
          (render-pixeled-design-ink render)(%medium-pixeled-design medium)
          (render-line-style render) (medium-line-style medium)
          (render-text-style render) (medium-text-style medium)
          (render-text-font render) (text-style-to-font
                                     (port medium) (medium-text-style medium)))))

(defmacro %medium-with-render (medium &body code)
  `(progn (%medium-update-render ,medium)
          (let* ((msheet (sheet-mirrored-ancestor (medium-sheet ,medium)))
                 (mirror (and msheet (sheet-mirror msheet)))
                 (render (medium-render ,medium))
                 (image (and mirror (image-mirror-image mirror))))
            (when (and msheet mirror)
              (mirror-rendering mirror
                ,@code)))))
          
;;;
;;; standard medium protocol
;;;

(defgeneric medium-draw-rectangle-using-ink*
    (medium ink left top right bottom filled))

(defmethod medium-draw-rectangle* ((medium render-medium-mixin) left top right bottom filled)
  (medium-draw-rectangle-using-ink* medium (medium-ink medium)
                                    left top right bottom filled))

(defmethod medium-draw-rectangle-using-ink* ((medium render-medium-mixin) (ink t) left top right bottom filled)
  (%medium-with-render medium
    (render-draw-rectangle-using-ink*
     render
     image
     left top right bottom filled)))

(defmethod medium-draw-rectangles* ((medium render-medium-mixin) position-seq filled)
  (assert (evenp (length position-seq)))
  (do ((v 0 (+ 4 v)))
      ((>= v (length position-seq)))
    (let ((x1 (elt position-seq (+ 0 v)))
	  (y1 (elt position-seq (+ 1 v)))
	  (x2 (elt position-seq (+ 2 v)))
	  (y2 (elt position-seq (+ 3 v))))
      (medium-draw-rectangle* medium x1 y1 x2 y2 filled))))
  
(defmethod medium-draw-polygon* ((medium render-medium-mixin) coord-seq closed filled)
   (%medium-with-render medium
    (render-draw-poligon*
     render
     image
     coord-seq closed filled)))

(defmethod medium-draw-line* ((medium render-medium-mixin) x1 y1 x2 y2)
  (%medium-with-render medium
    (render-draw-line*
     render
     image
     x1 y1 x2 y2)))

(defmethod medium-draw-point* ((medium render-medium-mixin) x y)
   (%medium-with-render medium
    (render-draw-point*
     render
     image
     x y)))

(defmethod medium-draw-circle* ((medium render-medium-mixin)
				center-x center-y radius start-angle end-angle
				filled)
  ;;; TO FIX
  (%medium-with-render medium
    (render-draw-circle*
     render
     image
     center-x center-y radius start-angle end-angle
     filled)))
  
(defmethod medium-draw-ellipse* ((medium render-medium-mixin) center-x center-y
				 radius-1-dx radius-1-dy
				 radius-2-dx radius-2-dy
				 start-angle end-angle filled)
  (%medium-with-render medium
    (render-draw-ellipse*
     render
     image
     center-x center-y
     radius-1-dx radius-1-dy
     radius-2-dx radius-2-dy
     start-angle end-angle filled)))

(defmethod medium-draw-text* ((medium render-medium-mixin) string x y
                              start end
                              align-x align-y
                              toward-x toward-y transform-glyphs)
  ;;transformation)
  (let ((transformation (clim:make-translation-transformation 0 0)))
   (%medium-with-render medium
    (render-draw-text*
     render
     image
     string x y
     start end
     align-x align-y
     toward-x toward-y transform-glyphs
     transformation))))

(defun %medium-copy-area (dst-medium to-x to-y
                          width height
                          src-medium
                          from-x from-y)
  (medium-force-output src-medium)
  (multiple-value-bind (w2 h2)
      (untransform-distance (medium-transformation src-medium)
                            width height)
    (multiple-value-bind (w h)
        (transform-distance (sheet-transformation (medium-sheet dst-medium))
                            w2 h2)
      (clim:with-bounding-rectangle* (min-x min-y max-x max-y)
        (region-intersection
         (climi::medium-device-region dst-medium)
         (transform-region
          (sheet-native-transformation (medium-sheet dst-medium))
          (make-rectangle* to-x to-y (+ to-x w) (+ to-y h))))
        (multiple-value-bind (x1 y1)
            (transform-position
             (sheet-native-transformation (medium-sheet dst-medium))
             to-x to-y)
          (multiple-value-bind (x2 y2)
              (transform-position
               (sheet-native-transformation (medium-sheet src-medium))
               from-x from-y)
            (%medium-with-render dst-medium
              (render-draw-image
               render
               image
               (image-mirror-image (sheet-mirror (medium-sheet src-medium)))
               (round (+ x2 (- min-x x1)))
               (round (+ y2 (- min-y y1)))
               (round (- max-x min-x))
               (round (- max-y min-y))
               (round min-x)
               (round min-y)))))))))
 
(defmethod medium-copy-area ((from-drawable render-medium-mixin) from-x from-y width height
                             (to-drawable render-medium-mixin) to-x to-y)
  (%medium-copy-area to-drawable
                     to-x to-y
                     width height
                     from-drawable
                     from-x from-y))

(defmethod medium-copy-area ((from-drawable render-medium-mixin) from-x from-y width height
                             (to-drawable image-sheet-mixin) to-x to-y)
  (let* ((msheet (sheet-mirrored-ancestor to-drawable)))
    (when (and msheet (sheet-mirror msheet))
      (climi::with-pixmap-medium (to-medium to-drawable)
        (%medium-copy-area (sheet-medium to-drawable)
                           to-x to-y
                           width height
                           from-drawable
                           from-x from-y)))))
      
(defmethod medium-copy-area ((from-drawable image-sheet-mixin) from-x from-y width height
                             (to-drawable render-medium-mixin) to-x to-y)
  (let ((msheet (sheet-mirrored-ancestor (medium-sheet to-drawable))))
    (when (and msheet (sheet-mirror msheet))
      (%medium-copy-area to-drawable
                         to-x to-y
                         width height
                         (sheet-medium from-drawable)
                         from-x from-y))))

#|
(defmethod mcclim-image::medium-draw-image-design* ((medium render-medium-mixin)
                                                    (design mcclim-image::rgb-image-design) to-x to-y)
  (let* ((image (slot-value design 'mcclim-image::image)))
    (medium-draw-image* medium image to-x to-y)))
|#
(defmethod medium-draw-image* ((medium render-medium-mixin)
                               src-img to-x to-y)
   (%medium-with-render medium
    (render-draw-image*
     render
     image
     src-img
     to-x
     to-y)))

(defmethod medium-finish-output ((medium render-medium-mixin))
  (when (sheet-mirror (medium-sheet medium))
    (%mirror-force-output (sheet-mirror (medium-sheet medium)))))

(defmethod medium-force-output ((medium render-medium-mixin))
  (when (sheet-mirror (medium-sheet medium))
    (%mirror-force-output (sheet-mirror (medium-sheet medium)))))
#|
(defmethod mcclim-image:medium-free-image-design
    ((medium render-medium-mixin) (design mcclim-image:rgb-image-design))
  ())
|#

;;;
;;; Bezier
;;;

(defvar *bezier-draw-control-lines* nil)
(defvar *bezier-draw-location-labels* nil)

(defgeneric %medium-draw-bezier-design (medium design filled
                                        &key bezier-draw-control-lines
                                          bezier-draw-location-labels))

(defmethod %medium-draw-bezier-design ((medium render-medium-mixin) design filled
                                       &key (bezier-draw-control-lines *bezier-draw-control-lines*)
                                         (bezier-draw-location-labels *bezier-draw-location-labels*))
  (%medium-with-render medium
    (render-draw-bezier-design
     render
     image
     design
     filled))
  (when (or bezier-draw-control-lines
            bezier-draw-location-labels)
    (let ((segments (mcclim-bezier:segments design)))
      (let ((i 0))
        (map nil (lambda (segment)
                   (incf i)
                   (with-slots ((p0 mcclim-bezier:p0)
                                (p1 mcclim-bezier:p1)
                                (p2 mcclim-bezier:p2)
                                (p3 mcclim-bezier:p3))
                       segment
                     (when bezier-draw-control-lines
                       (draw-point medium p0 :ink +blue+ :line-thickness 6)
                       (draw-point medium p1 :ink +red+ :line-thickness 6)
                       (draw-line  medium p0 p1 :ink +green+ :line-thickness 2)
                       (draw-point medium p2 :ink +red+ :line-thickness 6)
                       (draw-line  medium p1 p2 :ink +green+ :line-thickness 2)
                       (draw-point medium p3 :ink +blue+ :line-thickness 6)
                       (draw-line  medium p2 p3 :ink +green+ :line-thickness 2))
                     (when bezier-draw-location-labels
                       (draw-text medium (format nil "P~D ~D ~D" i (point-x p0) (point-y p0)) p0)
                       (draw-text medium (format nil "C~D ~D ~D" i (point-x p1) (point-y p1)) p1)
                       (draw-text medium (format nil "C~D ~D ~D" (1+ i) (point-x p2) (point-y p2)) p2)
                       (draw-text medium (format nil "P~D ~D ~D" (1+ i) (point-x p3) (point-y p3)) p3))))
             segments)))))

(defmethod mcclim-bezier:medium-draw-bezier-design* ((medium render-medium-mixin)
                                       (design mcclim-bezier:bezier-curve))
  (%medium-draw-bezier-design medium design nil))

(defmethod mcclim-bezier:medium-draw-bezier-design* ((medium render-medium-mixin)
                                                     (design mcclim-bezier:bezier-area))
  (%medium-draw-bezier-design medium design t))

(defmethod mcclim-bezier:medium-draw-bezier-design* ((medium render-medium-mixin)
                                                     (design mcclim-bezier:bezier-union))
  (dolist (area (mcclim-bezier:areas design))
    (%medium-draw-bezier-design medium area t)))

(defmethod mcclim-bezier:medium-draw-bezier-design* ((medium render-medium-mixin)
                                                     (design mcclim-bezier:bezier-difference))
  (dolist (area (mcclim-bezier:positive-areas design))
    (%medium-draw-bezier-design medium area t))
  (dolist (area (mcclim-bezier:negative-areas design))
    (with-drawing-options (medium :ink +background-ink+)
      (%medium-draw-bezier-design medium area t))))
