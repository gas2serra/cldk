(in-package :cldk-render-internals)

;;;
;;; path utility
;;;
(defun make-path (x y)
  (let ((path (paths:create-path :open-polyline)))
    (paths:path-reset path (paths:make-point x y))
    path))

(defun line-to (path x y)
  (paths:path-extend path (paths:make-straight-line)
                     (paths:make-point x y)))

(defun close-path (path )
  (setf (paths::path-type path) :closed-polyline))

(defun stroke-path (path line-style)
  (let ((dashes (climi::line-style-dashes line-style)))
    (when dashes
        (setf path (paths:dash-path path
                                    (ctypecase dashes
                                      (simple-array
                                       dashes)
                                      (cons
                                       (map 'vector #'(lambda (x) x)
                                            dashes))
                                      (t
                                       #(2 1)))))))
  (setf path (paths:stroke-path path
				(max 1 (line-style-thickness line-style))
				:joint (funcall #'(lambda (c)
						    (if (eq c :bevel)
							:none
							c))
						  (line-style-joint-shape line-style))
				:caps (funcall #'(lambda (c)
						   (if (eq c :no-end-point)
						       :butt
						       c))
					       (line-style-cap-shape line-style))))
  path)

;;;
;;; state utility
;;;
(defgeneric aa-stroke-paths (image pixeled-design paths line-style state transformation clip-region))
(defgeneric aa-fill-paths (image pixeled-design paths state transformation clip-region))
(defgeneric aa-fill-alpha-paths (image pixeled-design paths state transformation clip-region))

(declaim (inline aa-line-f))
(defun aa-line-f (state mxx mxy myx myy tx ty x1 y1 x2 y2)
  (declare (type coordinate mxx mxy myx myy tx ty))
  (let ((x1 (+ (* mxx x1) (* mxy y1) tx))
	(y1 (+ (* myx x1) (* myy y1) ty))
	(x2 (+ (* mxx x2) (* mxy y2) tx))
	(y2 (+ (* myx x2) (* myy y2) ty)))
    (aa::line-f state x1 y1 x2 y2)))

(defun aa-update-state (state paths transformation)
  (multiple-value-bind (mxx mxy myx myy tx ty)
      (climi::get-transformation transformation)
    (if (listp paths)
	(dolist (path paths)
	  (%aa-update-state state path mxx mxy myx myy tx ty))
	(%aa-update-state state paths mxx mxy myx myy tx ty))))

(defun %aa-update-state (state paths mxx mxy myx myy tx ty)
  (let ((iterator (vectors::path-iterator-segmented paths)))
    (multiple-value-bind (i1 k1 e1) (vectors::path-iterator-next iterator)
      (declare (ignore i1))
      (when (and k1 (not e1))
	;; at least 2 knots
	(let ((first-knot k1))
	  (loop
	     (multiple-value-bind (i2 k2 e2) (vectors::path-iterator-next iterator)
	       (declare (ignore i2))
	       (aa-line-f state mxx mxy myx myy tx ty
			      (vectors::point-x k1) (vectors::point-y k1)
			      (vectors::point-x k2) (vectors::point-y k2))
	       (setf k1 k2)
	       (when e2
		 (return))))
	  (aa-line-f state mxx mxy myx myy tx ty
			 (vectors::point-x k1) (vectors::point-y k1)
			 (vectors::point-x first-knot) (vectors::point-y first-knot)))))
    state))

(defmethod aa-stroke-paths (image pixeled-design paths line-style state transformation clip-region)
  (vectors::state-reset state)
  (let ((paths (car (mapcar (lambda (path)
                              (stroke-path path line-style))
                            paths))))
    (aa-update-state state paths transformation)
    (aa-cells-sweep/rectangle image
                              pixeled-design
                              state
                              clip-region)))

(defmethod aa-fill-paths (image pixeled-design paths state transformation clip-region)
  (vectors::state-reset state)
  (dolist (path paths)
    (setf (paths::path-type path) :closed-polyline))
  (aa-update-state state paths transformation)
  (aa-cells-sweep/rectangle image
                            pixeled-design
                            state
                            clip-region))

(defmethod aa-fill-alpha-paths (image pixeled-design paths state transformation clip-region)
  (vectors::state-reset state)
  (dolist (path paths)
    (setf (paths::path-type path) :closed-polyline))
  (aa-update-state state paths transformation)
  (aa-cells-alpha-sweep/rectangle image
                                  pixeled-design
                                  state
                                  clip-region))
