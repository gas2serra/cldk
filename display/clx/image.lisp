(in-package :cldk-internals)

(defclass clx-rgb-image (rgb-image buffer-image-mixin)
  ())

(defmethod image-rgb-get-fn ((image clx-rgb-image) &key (dx 0) (dy 0))
  (let ((pixels (image-pixels image)))
    (declare (type rgb-image-pixels pixels))
    (lambda (x y)
      (let ((p (aref pixels (+ dy y) (+ dx x))))
        (values (ldb (byte 8 16) p)
                (ldb (byte 8 8) p)
                (ldb (byte 8 0) p)
                (ldb (byte 8 24) p))))))

(defmethod image-rgb-set-fn ((image clx-rgb-image) &key (dx 0) (dy 0))
  (let ((pixels (image-pixels image)))
    (declare (type rgb-image-pixels pixels))
    (lambda (x y r g b)
      (setf (aref pixels (+ dy y) (+ dx x))
            (dpb r (byte 8 16)
                 (dpb g (byte 8 8)
                      (dpb b (byte 8 0)
                           (dpb 255 (byte 8 24) 0))))))))
