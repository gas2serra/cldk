(in-package :cldk-driver-clx)

#+nil (declaim (optimize speed))

;;;
;;; Pixel - RGB translators
;;;
(deftype octet ()
  '(unsigned-byte 8))

(defun mask->byte (mask)
  (let ((h (integer-length mask)))
    (let ((l (integer-length (logxor mask (1- (ash 1 h))))))
      (byte (- h l) l))))

(defvar *translator-cache-lock* (bt:make-lock "translator cache lock"))
(defparameter *rgb->pixel-translator-cache* (make-hash-table :test #'equal))
(defparameter *pixel->rgb-translator-cache* (make-hash-table :test #'equal))

(defun rgb->pixel-translator (colormap)
  (unless (eq (xlib:visual-info-class (xlib:colormap-visual-info colormap))
	      :true-color)
    (error "sorry, cannot draw rgb image for non-true-color drawable yet"))
  (let* ((info (xlib:colormap-visual-info colormap))
	 (rbyte (mask->byte (xlib:visual-info-red-mask info)))
	 (gbyte (mask->byte (xlib:visual-info-green-mask info)))
	 (bbyte (mask->byte (xlib:visual-info-blue-mask info)))
         (abyte (mask->byte (- #xFFFFFFFF
                               (+ (xlib:visual-info-red-mask info)
                                  (xlib:visual-info-green-mask info)
                                  (xlib:visual-info-blue-mask info)))))
         (key (list rbyte gbyte bbyte)))
    (bt:with-lock-held (*translator-cache-lock*)
      (or (gethash key *rgb->pixel-translator-cache*)
          (setf (gethash key *rgb->pixel-translator-cache*)
                (compile nil
                         `(lambda (r g b a)
                            (dpb r ',rbyte
                                 (dpb g ',gbyte
                                      (dpb b ',bbyte
                                           (dpb a ',abyte
                                                0)))))))))))

(defun pixel->rgb-translator (colormap)
  (unless (eq (xlib:visual-info-class (xlib:colormap-visual-info colormap))
	      :true-color)
    (error "sorry, cannot draw rgb image for non-true-color drawable yet"))
  (let* ((info (xlib:colormap-visual-info colormap))
	 (rbyte (mask->byte (xlib:visual-info-red-mask info)))
	 (gbyte (mask->byte (xlib:visual-info-green-mask info)))
	 (bbyte (mask->byte (xlib:visual-info-blue-mask info)))
         (abyte (mask->byte (- #xFFFFFFFF
                               (+ (xlib:visual-info-red-mask info)
                                  (xlib:visual-info-green-mask info)
                                  (xlib:visual-info-blue-mask info)))))
         (key (list rbyte gbyte bbyte)))
    (bt:with-lock-held (*translator-cache-lock*)
      (or (gethash key *pixel->rgb-translator-cache*)
          (setf (gethash key *pixel->rgb-translator-cache*)
                (compile nil
                         `(lambda (p)
                            (values (ldb ',rbyte p)
                                    (ldb ',gbyte p)
                                    (ldb ',bbyte p)
                                    (ldb ',abyte p)))))))))

;;;
;;; Clx images
;;;
(defclass clx-image (cldki::shared-image)
  ())

(defun clx-image-colormap (image)
  (xlib:window-colormap (cldki::driver-object-id (cldki::image-device image))))

;;;
;;; RGB
;;;
(deftype clx-basic-image-pixels () '(simple-array (unsigned-byte 32) (* *)))

(defclass clx-basic-image (clx-image)
  ((pixels :type clx-basic-image-pixels)))

(defmethod initialize-instance :after ((image clx-basic-image)
                                       &key)
  (let ((width (image-width image))
        (height (image-height image)))
    (when (and width height (not (slot-boundp image 'pixels)))
      (setf (slot-value image 'pixels)
            (make-array (list height width)
                        :element-type '(unsigned-byte 32)
                        :initial-element #xFFFFFFFF)))))

;;;
;;; RGBA
;;;
(defclass clx-rgba-image (clx-basic-image rgba-image-mixin)
  ())

#|
(defmethod image-rgba-get-fn ((image clx-rgba-image) &key (dx 0) (dy 0) (region nil))
  (let ((pixels (image-pixels image))
        (translator (pixel->rgb-translator (clx-image-colormap image))))
    (declare (type clx-basic-image-pixels pixels)
             (type fixnum dx dy)
             (type (function (fixnum) (values octet octet octet octet)) translator))
    (lambda (x y)
      (declare (type fixnum x y))
      (if (or (not region) (clim:region-contains-position-p region x y))
          (let ((p (aref pixels (+ y dy) (+ x dx))))
            (multiple-value-bind (r g b a)
                (funcall translator p)
              (values r g b a)))
          (values 0 0 0 0)))))
|#

(defmethod image-rgba-set-fn ((image clx-rgba-image) &key (dx 0) (dy 0))
  (let ((pixels (image-pixels image))
        (translator (rgb->pixel-translator (clx-image-colormap image))))
    (declare (type clx-basic-image-pixels pixels)
             (type fixnum dx dy)
             (type (function (octet octet octet octet) fixnum) translator))
    (lambda (x y red green blue alpha)
      (declare (type fixnum x y)
               (type octet red green blue))
      (setf (aref pixels (+ y dy) (+ x dx))
            (funcall translator red green blue alpha)))))

;;;
;;; RGB
;;;
(defclass clx-rgb-image (clx-basic-image rgb-image-mixin)
  ())

(defmethod image-rgb-get-fn ((image clx-rgb-image) &key (dx 0) (dy 0) (region nil))
  (let ((pixels (image-pixels image))
        (translator (pixel->rgb-translator (clx-image-colormap image))))
    (declare (type clx-basic-image-pixels pixels)
             (type fixnum dx dy)
             (type (function (fixnum) (values octet octet octet octet)) translator))
    (lambda (x y)
      (declare (type fixnum x y))
      (let ((p (aref pixels (+ y dy) (+ x dx))))
        (multiple-value-bind (r g b a)
            (funcall translator p)
          #+nil(rgba->rgb r g b a)
          (values r g b)
          ))
      (values 0 0 0))))

(defmethod image-rgb-set-fn ((image clx-rgb-image) &key (dx 0) (dy 0))
  (let ((pixels (image-pixels image))
        (translator (rgb->pixel-translator (clx-image-colormap image))))
    (declare (type clx-basic-image-pixels pixels)
             (type fixnum dx dy)
             (type (function (octet octet octet octet) fixnum) translator))
    (lambda (x y red green blue)
      (declare (type fixnum x y)
               (type octet red green blue))
      (multiple-value-bind (r g b a)
          #+nil(rgb->rgba red green blue)
          (values red green blue 255)
        (setf (aref pixels (+ y dy) (+ x dx))
              (funcall translator r g b a))))))

;;;
;;; Gray
;;;
(defclass clx-gray-image (clx-basic-image gray-image-mixin)
  ())
#|
(defmethod image-gray-get-fn ((image clx-gray-image) &key (dx 0) (dy 0) (region nil))
  (let ((pixels (image-pixels image))
        (translator (pixel->rgb-translator (clx-image-colormap image))))
    (declare (type clx-basic-image-pixels pixels)
             (type fixnum dx dy)
             (type (function (fixnum) (values octet octet octet octet)) translator))
    (lambda (x y)
      (declare (type fixnum x y))
      (if (or (not region) (clim:region-contains-position-p region x y))
          (let ((p (aref pixels (+ y dy) (+ x dx))))
            (multiple-value-bind (r g b a)
                (funcall translator p)
              (rgba->gray r g b a)))
          0))))
|#
(defmethod image-gray-set-fn ((image clx-gray-image) &key (dx 0) (dy 0))
  (let ((pixels (image-pixels image))
        (translator (rgb->pixel-translator (clx-image-colormap image))))
    (declare (type clx-basic-image-pixels pixels)
             (type fixnum dx dy)
             (type (function (octet octet octet octet) fixnum) translator))
    (lambda (x y gray)
      (declare (type fixnum x y)
               (type octet gray))
      (multiple-value-bind (r g b a)
          (gray->rgba gray)
        (setf (aref pixels (+ y dy) (+ x dx))
              (funcall translator r g b a))))))

;;;
;;; making
;;;
(defmethod create-image ((window cldk-driver-clx::clx-driver-window) (type (eql :rgba)) width height)
  (make-instance 'clx-rgba-image :width width :height height
                 :device window))

(defmethod create-image ((window cldk-driver-clx::clx-driver-window) (type (eql :rgb)) width height)
  (make-instance 'clx-rgb-image :width width :height height
                 :device window))

(defmethod create-image ((window cldk-driver-clx::clx-driver-window) (type (eql :gray)) width height)
  (make-instance 'clx-gray-image :width width :height height
                 :device window))


;;;
;;; To xlib image/pixmap
;;;

(defun clx-image->xlib-image (image)
  (let ((depth (xlib:drawable-depth
                (cldki::driver-object-id (image-device image)))))
    (xlib:create-image :width (image-width image)
                       :height (image-height image)
                       :depth depth
		       :bits-per-pixel 32
                       :data (image-pixels image)
                       :format :z-pixmap)))

(defun clx-image->pixmap (image &optional (x 0) (y 0)
                                  (w (image-width image))
                                  (h (image-height image)))
  (let* ((drawable (image-device image))
         (xlib-image (clx-image->xlib-image image))
         (pixmap (xlib:create-pixmap :drawable drawable
				     :width w
				     :height h
				     :depth (xlib:drawable-depth drawable)))
         (gc (xlib:create-gcontext :drawable pixmap)))
    (unless (or (>= w 2048) (>= h 2048))
      (xlib:put-image pixmap gc xlib-image
		      :src-x x :src-y y
		      :x 0 :y 0
		      :width w :height h))
    (xlib:free-gcontext gc)
    pixmap))

(defun rgba-image->xlib-image-mask (image &optional (x0 0) (y0 0)
                                            (w (image-width image))
                                            (h (image-height image)))
  (let ((xdata (make-array (list h w)
			   :element-type '(unsigned-byte 1)))
        (fn (image-rgba-get-fn image)))
    (dotimes (y h)
      (dotimes (x w)
        (multiple-value-bind (r g b a)
            (funcall fn (+ x0 x) (+ y0 y))
          (if (> a 128)
              (setf (aref xdata y x) 1)
	      (setf (aref xdata y x) 0)))))
    (xlib:create-image :width (image-width image)
                       :height (image-height image)
                       :depth 1
                       :data xdata)))

(defun clx-rgba-image->pixmap-mask (image &optional (x 0) (y 0)
                                            (w (image-width image))
                                            (h (image-height image)))
  (rgba-image->pixmap-mask (image-device image) image x y w h))
 
#|
(defun rgba-image->pixmap-mask (medium image &optional (x 0) (y 0)
                                               (w (image-width image))
                                               (h (image-height image)))
  (let* ((drawable (clim-clx::sheet-xmirror (medium-sheet medium)))
         (xlib-image (rgba-image->xlib-image-mask image x y w h))
         (pixmap (xlib:create-pixmap :drawable drawable
				     :width w
				     :height h
				     :depth 1))
         (gc (xlib:create-gcontext :drawable pixmap
                                   :foreground 1
				   :background 0)))
    (unless (or (>= w 2048) (>= h 2048))
      (xlib:put-image pixmap gc xlib-image
		      :src-x x :src-y y
		      :x 0 :y 0
		      :width w :height h))
    (xlib:free-gcontext gc)
    pixmap))
|#
