(in-package :cldk-internals)

;;;
;;; Two dimensional array of pixels
;;;
(defclass sdl2-image (basic-image buffer-image-mixin)
  ())

;;;
;;; RGBA
;;;
(deftype sdl2-rgb-image-pixels () 'cffi-sys:foreign-pointer)

(defclass sdl2-rgb-image (sdl2-image rgb-image-mixin)
  ((pixels :type sdl2-rgb-image-pixels)))

(defmethod image-rgb-get-fn ((image sdl2-rgb-image) &key (dx 0) (dy 0))
  (let ((pixels (image-pixels image))
        (width (image-width image)))
    (declare (type sdl2-rgb-image-pixels pixels))
    (lambda (x y)
      (cffi-sys:%mem-ref
       pixels :UNSIGNED-INT (* 4
                               (+
                                (* (+ y dy) width)
                                (+ x dx)))))))

(defmethod image-rgb-set-fn ((image sdl2-rgb-image) &key (dx 0) (dy 0))
  (let ((pixels (image-pixels image))
        (width (image-width image)))
    (declare (type sdl2-rgb-image-pixels pixels))
    (lambda (x y r g b)
      (cffi-sys:%mem-set
       (dpb r (byte 8 0)
            (dpb g (byte 8 8)
                 (dpb b (byte 8 16)
                      (dpb 255 (byte 8 24) 0))))
       pixels :UNSIGNED-INT (* 4
                               (+
                                (* (+ y dy) width)
                                (+ x dx)))))))

