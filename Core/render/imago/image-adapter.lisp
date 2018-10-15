(in-package :cldk-mcclim-render-internals)

(defclass imago-image-adapter (image-adapter-mixin rgb-image-mixin)
  ())

(defmethod image-width ((image imago-image-adapter))
  (with-slots (img) image
    (imago:image-width img)))

(defmethod image-height ((image imago-image-adapter))
  (with-slots (img) image
    (imago:image-width img)))

(defmethod image-rgb-get-fn ((image imago-image-adapter) &key (dx 0) (dy 0) (region nil))
  (with-slots (img) image
    (declare (type fixnum dx dy))
    (etypecase img
      (imago:rgb-image
       (lambda (x y)
         (declare (type fixnum x y))
         (if (or (not region) (clim:region-contains-position-p region x y))
             (let ((p (imago:image-pixel img (+ x dx) (+ y dy))))
               (values (ldb (byte 8 16) p)
                       (ldb (byte 8 8) p)
                       (ldb (byte 8 0) p)))
             (values 0 0 0)))))))

(defun make-imago-image-adapter (imago)
  (make-instance 'imago-image-adapter :image imago))
