(in-package :cldk-driver-sdl2)

(deftype octet ()
  '(unsigned-byte 8))

(deftype sdl2-basic-image-pixels () 'cffi-sys:foreign-pointer)

(defclass sdl2-image (cldki::shared-image)
  ())

(defclass sdl2-basic-image (sdl2-image)
  ((svector)
   (pixels :type sdl2-basic-image-pixels)))

(defmethod initialize-instance :after ((image sdl2-basic-image)
                                       &key)
  (let ((width (image-width image))
        (height (image-height image)))
    (when (and width height (not (slot-boundp image 'pixels)))
      (with-slots (svector pixels) image
        (setf svector (static-vectors:make-static-vector (* width height 4)
                                                         :element-type '(unsigned-byte 8)))
        (setf pixels (static-vectors:static-vector-pointer svector))))))

(defclass sdl2-rgb-image (sdl2-basic-image rgb-image-mixin)
  ())

(defmethod image-rgb-get-fn ((image sdl2-rgb-image) &key (dx 0) (dy 0) (region nil))
  #+nil (let ((pixels (image-pixels image))
        (translator (pixel->sdl2-translator (clx-image-colormap image))))
    (declare (type clx-basic-image-pixels pixels)
             (type fixnum dx dy)
             (type (function (fixnum) (values octet octet octet octet)) translator))
    (lambda (x y)
      (declare (type fixnum x y))
      (if (or (not region) (clim:region-contains-position-p region x y))
          (let ((p (aref pixels (+ y dy) (+ x dx))))
            (multiple-value-bind (r g b a)
                (funcall translator p)
              (sdl2a->sdl2 r g b a)))
          (values 0 0 0)))))

(defmethod image-rgb-set-fn ((image sdl2-rgb-image) &key (dx 0) (dy 0))
  (let ((pixels (image-pixels image))
        (width (image-width image)))
    (declare (type sdl2-basic-image-pixels pixels)
             (type fixnum dx dy))
    (lambda (x y red green blue)
      (declare (type fixnum x y)
               (type octet red green blue))
      (multiple-value-bind (r g b a)
          (values red green blue 255)
        (cffi-sys:%mem-set
         (dpb a (byte 8 0)
              (dpb b (byte 8 8)
                   (dpb g (byte 8 16)
                        (dpb r (byte 8 24) 0))))
         pixels :UNSIGNED-INT (* 4
                                 (+
                                  (* (+ y dy) width)
                                  (+ x dx))))))))

(defun sdl2-image->sdl2-surface (image)
  (sdl2:create-rgb-surface-with-format-from
   (image-pixels image)
   (image-width image)
   (image-height image)
   32
   (* 4 (image-width image))
   :format sdl2:+pixelformat-rgba8888+))

(defmethod create-image ((window cldk-driver-sdl2::sdl2-driver-window) (type (eql :rgb)) width height)
  (make-instance 'sdl2-rgb-image :width width :height height
                 :device window))
