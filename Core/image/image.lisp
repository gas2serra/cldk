(in-package :cldk-internals)

;;;
;;; Image
;;;
(defclass image ()
  ())

(defgeneric image-width (image))
(defgeneric image-height (image))

(defgeneric image-device (image))

;;;
;;; Image mixins
;;;
(defclass image-mixin ()
  ())

(defclass rgba-image-mixin (image-mixin)
  ())

(defclass rgb-image-mixin (image-mixin)
  ())

(defclass gray-image-mixin (image-mixin)
  ())

(defgeneric image-type (image)
  (:method ((image rgba-image-mixin))
    :rgba)
  (:method ((image rgb-image-mixin))
    :rgb)
  (:method ((image gray-image-mixin))
    :gray))

;;;
;;; Image primitives
;;;
(deftype image-rgba-get-fn () '(function (fixnum fixnum) (values octet octet octet octet)))
(defgeneric image-rgba-get-fn (image &key dx dy region))
(deftype image-rgba-set-fn () '(function (fixnum fixnum octet octet octet octet)))
(defgeneric image-rgba-set-fn (image &key dx dy))
(deftype image-rgb-get-fn () '(function (fixnum fixnum) (values octet octet octet)))
(defgeneric image-rgb-get-fn (image &key dx dy region))
(deftype image-rgb-set-fn () '(function (fixnum fixnum octet octet octet)))
(defgeneric image-rgb-set-fn (image &key dx dy))
(deftype image-gray-get-fn () '(function (fixnum fixnum) octet))
(defgeneric image-gray-get-fn (image &key dx dy region))
(deftype image-gray-set-fn () '(function (fixnum fixnum octet)))
(defgeneric image-gray-set-fn (image &key dx dy))

;;;
;;; Basic Image
;;;
(defclass basic-image (image)
  ((width :initform 0 :initarg :width :reader image-width :type fixnum)
   (height :initform 0 :initarg :height :reader image-height :type fixnum)
   (pixels :initarg :pixels
           :accessor image-pixels)))

;;;
;;; Image operations
;;;
(defgeneric create-image (device type width height))
(defgeneric copy-image (src-image sx sy width height dst-image x y))
