(in-package :cldk-clx-backend)

(defclass clx-buffer (buffer clx-driver-buffer kerneled-buffer-mixin)
  ())



(defmethod image-width ((buffer clx-buffer))
  (let ((db buffer))
    (with-slots (ximage) db
      (xlib:image-width ximage))))

(defmethod image-height ((buffer clx-buffer))
  (let ((db buffer))
    (with-slots (ximage) db
      (xlib:image-height ximage))))

(defmethod image-pixels ((buffer clx-buffer))
  (let ((db buffer))
    (with-slots (xpixels) db
      xpixels)))

(defmethod image-rgb-get-fn ((image clx-buffer) &key (dx 0) (dy 0))
  (driver-buffer-rgb-get-fn (cldki::driver image) image dx dy))

(defmethod image-rgb-set-fn ((image clx-buffer) &key (dx 0) (dy 0))
  (driver-buffer-rgb-set-fn (cldki::driver image) image dx dy))
