(in-package :cldk-sdl2)

(defclass sdl2-buffer (buffer sdl2-driver-buffer kerneled-buffer-mixin)
  ())

(defmethod create-buffer ((server sdl2-server) width height)
  (make-instance 'sdl2-buffer :driver server :width width :height height))


(defmethod image-width ((buffer sdl2-buffer))
  (let ((db buffer))
    (with-slots (surface) db
      (sdl2:surface-width surface))))

(defmethod image-height ((buffer sdl2-buffer))
  (let ((db buffer))
    (with-slots (surface) db
      (sdl2:surface-height surface))))

(defmethod image-pixels ((buffer sdl2-buffer))
  (let ((db buffer))
    (with-slots (surface) db
      (sdl2:surface-pixels surface))))

(defmethod image-rgb-get-fn ((image sdl2-buffer) &key (dx 0) (dy 0))
  (driver-buffer-rgb-get-fn (cldki::driver image) image dx dy))

(defmethod image-rgb-set-fn ((image sdl2-buffer) &key (dx 0) (dy 0))
  (driver-buffer-rgb-set-fn (cldki::driver image) image dx dy))
