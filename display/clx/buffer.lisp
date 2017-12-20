(in-package :cldk-clx)

(defclass clx-buffer (buffer)
  ())

(defmethod create-buffer ((server clx-server) width height)
  (make-instance 'clx-buffer :server server :width width :height height))

(deftype clx-buffer-pixels () '(simple-array (unsigned-byte 32) (* *)))

(defmethod image-width ((buffer clx-buffer))
  (let ((db (cldki::buffer-driver-buffer buffer)))
    (with-slots (ximage) db
      (xlib:image-width ximage))))

(defmethod image-height ((buffer clx-buffer))
  (let ((db (cldki::buffer-driver-buffer buffer)))
    (with-slots (ximage) db
      (xlib:image-height ximage))))

(defmethod image-pixels ((buffer clx-buffer))
  (let ((db (cldki::buffer-driver-buffer buffer)))
    (with-slots (pixels) db
      pixels)))

(defmethod image-rgb-get-fn ((image clx-buffer) &key (dx 0) (dy 0))
  (let ((pixels (image-pixels image)))
    (declare (type clx-buffer-pixels pixels))
    (lambda (x y)
      (let ((p (aref pixels (+ dy y) (+ dx x))))
        (values (ldb (byte 8 16) p)
                (ldb (byte 8 8) p)
                (ldb (byte 8 0) p)
                (ldb (byte 8 24) p))))))

(defmethod image-rgb-set-fn ((image clx-buffer) &key (dx 0) (dy 0))
  (let ((pixels (image-pixels image)))
    (declare (type clx-buffer-pixels pixels))
    (lambda (x y r g b)
      (setf (aref pixels (+ dy y) (+ dx x))
            (dpb r (byte 8 16)
                 (dpb g (byte 8 8)
                      (dpb b (byte 8 0)
                           (dpb 255 (byte 8 24) 0))))))))
