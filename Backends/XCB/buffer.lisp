(in-package :cldk-xcb)

(defclass xcb-buffer (buffer xcb-driver-buffer kerneled-buffer-mixin)
  ())

(defmethod create-buffer ((server xcb-server) width height)
  (make-instance 'xcb-buffer :driver server :width width :height height))

(defmethod image-width ((buffer xcb-buffer))
  (let ((db buffer))
    (with-slots (cldk-driver-xcb::w) db
      cldk-driver-xcb::w)))

(defmethod image-height ((buffer xcb-buffer))
  (let ((db buffer))
    (with-slots (cldk-driver-xcb::h) db
      cldk-driver-xcb::h)))

(defmethod image-pixels ((buffer xcb-buffer))
  (let ((db buffer))
    (with-slots (xpixels) db
      xpixels)))

(defmethod image-rgb-get-fn ((image xcb-buffer) &key (dx 0) (dy 0))
  (driver-buffer-rgb-get-fn (cldki::driver image) image dx dy))

(defmethod image-rgb-set-fn ((image xcb-buffer) &key (dx 0) (dy 0))
  (driver-buffer-rgb-set-fn (cldki::driver image) image dx dy))
