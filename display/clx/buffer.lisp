(in-package :cldk-clx)

(defclass clx-buffer (buffer clx-driver-buffer)
  ())

(defmethod create-buffer ((server clx-server) width height)
  (make-instance 'clx-buffer :server server :width width :height height))

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
    (with-slots (pixels) db
      pixels)))

(defmethod image-rgb-get-fn ((image clx-buffer) &key (dx 0) (dy 0))
  (driver-buffer-rgb-get-fn (cldki::server image) image dx dy))

(defmethod image-rgb-set-fn ((image clx-buffer) &key (dx 0) (dy 0))
  (driver-buffer-rgb-set-fn (cldki::server image) image dx dy))
