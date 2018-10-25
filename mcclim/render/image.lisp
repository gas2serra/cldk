(in-package :cldk-render-internals)

;;;
;;; conversion
;;;
(declaim (inline color-value->octet)
	 (ftype (function (real) octet) color-value->octet))
(defun color-value->octet (v)
  (round (* 255 v)))

(declaim (inline color-octet->value)
	 (ftype (function (octet) real) color-octet->value))
(defun color-octet->value (o)
  (/ o 255))

(defgeneric color->octets (color)
  (:method ((color standard-color))
    (multiple-value-bind (r g b)
        (climi::color-rgb color)
      (values (color-value->octet r)
              (color-value->octet g)
              (color-value->octet b)))))

(defun octets->color (red green blue)
  (make-rgb-color
   (color-octet->value red)
   (color-octet->value green)
   (color-octet->value blue)))

#+nil (declaim (optimize speed))

(defgeneric image-medium (image))

(defmethod image-device (image)
  (image-medium image))

(defmethod image-medium (image)
  (image-device image))

;;;
;;; Image mixins
;;;
;;(defclass image-adapter-mixin (image-mixin)
;;  ((img :initarg :image)))


;;;
;;; Drawable Image
;;;
(defun draw-image* (medium image x y
                    &rest args
                    &key clipping-region transformation)
  (declare (ignorable clipping-region transformation args))
  (climi::with-medium-options (medium args)
    (medium-draw-image* medium image x y)))

(clim-internals::def-graphic-op draw-image* (image x y))

;;;
;;; Image Design
;;;
(defclass image-design (design)
  ((image :reader image
          :initarg :image)))

(defun make-image-design (image)
  (make-instance 'image-design :image image))

(defmethod clim:draw-design
    (medium (design image-design) &rest options
     &key (x 0) (y 0) &allow-other-keys)
  (climi::with-medium-options (medium options)
    (medium-draw-image* medium (slot-value design 'image) x y)))

;;;
;;; Image Pattern
;;;
(defclass image-pattern (pattern image-design)
  ())

(defmethod pattern-width ((pattern image-pattern))
  (image-width (image pattern)))

(defmethod pattern-height ((pattern image-pattern))
  (image-height (image pattern)))

(defmethod climi::medium-draw-pattern* (medium (pattern image-pattern) x y)
  (medium-draw-image* medium (image pattern) x y))





