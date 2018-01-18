(in-package :cldk-sdl2)

(defclass sdl2-buffer (buffer sdl2-driver-buffer)
  ())

(defmethod create-buffer ((server sdl2-server) width height)
  (make-instance 'sdl2-buffer :server server :width width :height height))

(deftype sdl2-rgb-image-pixels () 'cffi-sys:foreign-pointer)

(defmethod image-width ((buffer sdl2-buffer))
  (let ((db (cldki::buffer-driver-buffer buffer)))
    (with-slots (surface) db
      (sdl2:surface-width surface))))

(defmethod image-height ((buffer sdl2-buffer))
  (let ((db (cldki::buffer-driver-buffer buffer)))
    (with-slots (surface) db
      (sdl2:surface-height surface))))

(defmethod image-pixels ((buffer sdl2-buffer))
  (let ((db (cldki::buffer-driver-buffer buffer)))
    (with-slots (surface) db
      (sdl2:surface-pixels surface))))

(defmethod image-rgb-get-fn ((image sdl2-buffer) &key (dx 0) (dy 0))
  (let ((pixels (image-pixels image))
        (width (image-width image)))
    (declare (type sdl2-rgb-image-pixels pixels))
    (lambda (x y)
      (cffi-sys:%mem-ref
       pixels :UNSIGNED-INT (* 4
                               (+
                                (* (+ y dy) width)
                                (+ x dx)))))))

(defmethod image-rgb-set-fn ((image sdl2-buffer) &key (dx 0) (dy 0))
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

