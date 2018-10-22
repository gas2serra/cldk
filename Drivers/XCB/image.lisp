(in-package :cldk-driver-xcb)

(deftype octet ()
  '(unsigned-byte 8))

(deftype xcb-basic-image-pixels () 'cffi-sys:foreign-pointer)

(defclass xcb-image (cldki::shared-image)
  ())

(defclass xcb-basic-image (xcb-image)
  ((svector)
   (pixels :type xcb-basic-image-pixels)))

(defmethod initialize-instance :after ((image xcb-basic-image)
                                       &key)
  (let ((width (image-width image))
        (height (image-height image)))
    (when (and width height (not (slot-boundp image 'pixels)))
      (with-slots (svector pixels) image
        (setf svector (static-vectors:make-static-vector (* width height 4)
                                                         :element-type '(unsigned-byte 8)))
        (setf pixels (static-vectors:static-vector-pointer svector))))))

(defclass xcb-rgb-image (xcb-basic-image rgb-image-mixin)
  ())

(defmethod image-rgb-get-fn ((image xcb-rgb-image) &key (dx 0) (dy 0) (region nil))
  #+nil (let ((pixels (image-pixels image))
        (translator (pixel->xcb-translator (clx-image-colormap image))))
    (declare (type clx-basic-image-pixels pixels)
             (type fixnum dx dy)
             (type (function (fixnum) (values octet octet octet octet)) translator))
    (lambda (x y)
      (declare (type fixnum x y))
      (if (or (not region) (clim:region-contains-position-p region x y))
          (let ((p (aref pixels (+ y dy) (+ x dx))))
            (multiple-value-bind (r g b a)
                (funcall translator p)
              (xcba->xcb r g b a)))
          (values 0 0 0)))))

(defmethod image-rgb-set-fn ((image xcb-rgb-image) &key (dx 0) (dy 0))
  (let ((pixels (image-pixels image))
        (width (image-width image)))
    (declare (type xcb-basic-image-pixels pixels)
             (type fixnum dx dy))
    (lambda (x y red green blue)
      (declare (type fixnum x y)
               (type octet red green blue))
      (multiple-value-bind (r g b a)
          (values red green blue 255)
        (cffi-sys:%mem-set
         (dpb b (byte 8 0)
              (dpb g (byte 8 8)
                   (dpb r (byte 8 16)
                        (dpb a (byte 8 24) 0))))
         pixels :UNSIGNED-INT (* 4
                                 (+
                                  (* (+ y dy) width)
                                  (+ x dx))))))))


(defmethod create-image ((window cldk-driver-xcb::xcb-driver-window) (type (eql :rgb)) width height)
  (make-instance 'xcb-rgb-image :width width :height height
                 :device window))

(defmethod cldki::copy-image* (src-image rectangle-set
                               (dst-image xcb-rgb-image) dx dy)
  (cldk-render:copy-image src-image 0 0 (cldk-render:image-width dst-image)
                          (cldk-render:image-height dst-image)
                               dst-image (+ dx 0) (+ dy 0)))
  
