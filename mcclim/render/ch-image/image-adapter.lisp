(in-package :cldk-render-internals)

(defclass ch-image-image-adapter (image-adapter-mixin rgb-image-mixin)
  ())

(defmethod image-width ((image ch-image-image-adapter))
  (with-slots (img) image
    (ch-image:image-width img)))

(defmethod image-height ((image ch-image-image-adapter))
  (with-slots (img) image
    (ch-image:image-width img)))

(defmethod image-rgb-get-fn ((image ch-image-image-adapter) &key (dx 0) (dy 0) (region nil))
  (with-slots (img) image
    (declare (type fixnum dx dy))
    (lambda (x y)
      (declare (type fixnum x y))
      (if (or (not region) (clim:region-contains-position-p region x y))
          (multiple-value-bind (a r g b)
              (ch-image:get-argb-values img (+ y dy) (+ x dx))
            (values r g b))
          (values 0 0 0)))))

(defun make-ch-image-adapter (ch-image)
  (make-instance 'ch-image-image-adapter :image ch-image))
