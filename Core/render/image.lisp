(in-package :cldk-mcclim-render-internals)

#+nil (declaim (optimize speed))

;;;
;;; Image
;;;
(defclass image ()
  ())

(defgeneric image-with (image))
(defgeneric image-height (image))

(defgeneric image-medium (image))

;;;
;;; Image mixins
;;;
(defclass image-mixin ()
  ())

(defclass image-adapter-mixin (image-mixin)
  ((img :initarg :image)))

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

;;;
;;; Basic Image
;;;
(defclass basic-image (image)
  ((width :initform 0 :initarg :width :reader image-width :type fixnum)
   (height :initform 0 :initarg :height :reader image-height :type fixnum)
   (pixels :initarg :pixels
           :accessor image-pixels)))

;;;
;;; image I/O
;;;
(defgeneric read-image (source &key format type medium))
(defgeneric write-image (image destination &key format quality))

(defvar *image-file-readers* (make-hash-table :test 'equalp)
  "A hash table mapping keyword symbols naming image
formats to a function that can read an image of that format. The
functions will be called with one argument, the pathname of the
file to be read.")

(defvar *image-file-writer* (make-hash-table :test 'equalp)
  "A hash table mapping keyword symbols naming image
formats to a function that can write an image of that format. The
functions will be called with two arguments, the image and the pathname of the
file to be read.")

(defmacro define-image-file-reader (format (&rest args) &body body)
  `(setf (gethash ,format *image-file-readers*)
         #'(lambda (,@args)
             ,@body)))

(defun image-format-read-supported-p (format)
  "Return true if FORMAT is supported by `read-image'."
  (not (null (gethash format *image-file-readers*))))

(defmacro define-image-file-writer (format (&rest args) &body body)
  `(setf (gethash ,format *image-file-writer*)
         #'(lambda (,@args)
             ,@body)))

(defun image-format-write-supported-p (format)
  "Return true if FORMAT is supported by `write-image'."
  (not (null (gethash format *image-file-writer*))))

;;;
;;; Image extended primitives
;;;

;; blend
(deftype image-rgba-blend-fn () '(function (fixnum fixnum octet octet octet octet)))
(defgeneric image-rgba-blend-fn (image &key dx dy))
(defmethod image-rgba-blend-fn ((image rgba-image-mixin) &key (dx 0) (dy 0))
  #+nil(warn "using un-optimized function: image-rgba-blend-fn")
  (let ((sfn (image-rgba-set-fn image :dx dx :dy dy))
        (gfn (image-rgba-get-fn image :dx dx :dy dy)))
    (declare (type image-rgba-get-fn gfn)
             (type image-rgba-set-fn sfn))
    (lambda (x y red green blue alpha)
      (declare (type fixnum x y red green blue alpha))
      (multiple-value-bind (r g b a) (funcall gfn x y)
        (multiple-value-bind (nr ng nb na)
            (octet-rgba-blend-function red green blue alpha r g b a)
          (funcall sfn x y nr ng nb na))))))
(deftype image-rgb-blend-fn () '(function (fixnum fixnum octet octet octet octet)))
(defgeneric image-rgb-blend-fn (image &key dx dy))
(defmethod image-rgb-blend-fn ((image rgb-image-mixin) &key (dx 0) (dy 0))
  #+nil(warn "using un-optimized function: image-rgb-blend-fn")
  (let ((sfn (image-rgb-set-fn image :dx dx :dy dy))
        (gfn (image-rgb-get-fn image :dx dx :dy dy)))
    (declare (type image-rgb-get-fn gfn)
             (type image-rgb-set-fn sfn))
    (lambda (x y red green blue alpha)
      (declare (type fixnum x y red green blue alpha))
      (multiple-value-bind (r g b) (funcall gfn x y)
        (multiple-value-bind (nr ng nb)
            (octet-rgb-blend-function red green blue alpha r g b)
          (funcall sfn x y nr ng nb))))))
(deftype image-gray-blend-fn () '(function (fixnum fixnum octet octet)))
(defgeneric image-gray-blend-fn (image &key dx dy))
(defmethod image-gray-blend-fn ((image gray-image-mixin) &key (dx 0) (dy 0))
  #+nil(warn "using un-optimized function: image-gray-blend-fn")
  (let ((sfn (image-gray-set-fn image :dx dx :dy dy))
        (gfn (image-gray-get-fn image :dx dx :dy dy)))
    (declare (type image-gray-get-fn gfn)
             (type image-gray-set-fn sfn))
    (lambda (x y gray alpha)
      (declare (type fixnum x y gray alpha))
      (let ((g (funcall gfn x y)))
        (funcall sfn x y (octet-gray-blend-function gray alpha g))))))
;; xor
(deftype image-rgba-xor-blend-fn () '(function (fixnum fixnum octet octet octet octet)))
(defgeneric image-rgba-xor-blend-fn (image &key dx dy))
(defmethod image-rgba-xor-blend-fn ((image rgba-image-mixin) &key (dx 0) (dy 0))
  #+nil(warn "using unoptimized function: image-rgba-xor-blend-fn")
  (let ((sfn (image-rgba-set-fn image :dx dx :dy dy))
        (gfn (image-rgba-get-fn image :dx dx :dy dy)))
    (declare (type image-rgba-get-fn gfn)
             (type image-rgba-set-fn sfn))
    (lambda (x y red green blue alpha)
      (declare (type fixnum x y red green blue alpha))
      (multiple-value-bind (r g b a) (funcall gfn x y)
        (multiple-value-bind (nr ng nb na)
            (octet-rgba-blend-function (color-octet-xor r red) (color-octet-xor g green)
                                       (color-octet-xor b blue) alpha r g b a)
          (funcall sfn x y nr ng nb na))))))
(deftype image-rgb-xor-blend-fn () '(function (fixnum fixnum octet octet octet octet)))
(defgeneric image-rgb-xor-blend-fn (image &key dx dy))
(defmethod image-rgb-xor-blend-fn ((image rgb-image-mixin) &key (dx 0) (dy 0))
  #+nil(warn "using unoptimized function: image-rgb-xor-blend-fn")
  (let ((sfn (image-rgb-set-fn image :dx dx :dy dy))
        (gfn (image-rgb-get-fn image :dx dx :dy dy)))
    (declare (type image-rgb-get-fn gfn)
             (type image-rgb-set-fn sfn))
    (lambda (x y red green blue alpha)
      (declare (type fixnum x y red green blue alpha))
      (multiple-value-bind (r g b) (funcall gfn x y)
        (multiple-value-bind (nr ng nb)
            (octet-rgb-blend-function (color-octet-xor r red) (color-octet-xor g green)
                                      (color-octet-xor b blue) alpha r g b)
          (funcall sfn x y nr ng nb))))))
(deftype image-gray-xor-blend-fn () '(function (fixnum fixnum octet octet)))
(defgeneric image-gray-xor-blend-fn (image &key dx dy))
(defmethod image-gray-xor-blend-fn ((image gray-image-mixin) &key (dx 0) (dy 0))
  #+nil(warn "using unoptimized function: image-rgba-xor-blend-fn")
  (let ((sfn (image-gray-set-fn image :dx dx :dy dy))
        (gfn (image-gray-get-fn image :dx dx :dy dy)))
    (declare (type image-gray-get-fn gfn)
             (type image-gray-set-fn sfn))
    (lambda (x y gray alpha)
      (declare (type fixnum x y gray alpha))
      (multiple-value-bind (g) (funcall gfn x y)
        (multiple-value-bind (ng)
            (octet-gray-blend-function (color-octet-xor g gray) alpha g)
          (funcall sfn x y ng))))))
;; alpha
(deftype image-alpha-get-fn () '(function (fixnum fixnum) octet))
(defgeneric image-alpha-get-fn (image &key dx dy region))
(defmethod image-alpha-get-fn ((image gray-image-mixin) &key (dx 0) (dy 0) (region nil))
  (image-gray-get-fn image :dx dx :dy dy :region region))
(defmethod image-alpha-get-fn ((image rgba-image-mixin) &key (dx 0) (dy 0) (region nil))
  #+nil(warn "using unoptimized function: image-alpha-get-fn")
  (let ((fn (image-rgba-get-fn image :dx dx :dy dy :region region)))
    (declare (type image-rgba-get-fn fn))
    (lambda (x y)
      (declare (type fixnum x y))
      (multiple-value-bind (r g b a)
          (funcall fn x y)
        (rgba->alpha r g b a)))))
(deftype image-alpha-set-fn () '(function (fixnum fixnum octet )))
(defgeneric image-alpha-set-fn (image &key dx dy))
(defmethod image-alpha-set-fn ((image gray-image-mixin) &key (dx 0) (dy 0))
  (image-gray-set-fn image :dx dx :dy dy))
(defmethod image-alpha-set-fn ((image rgba-image-mixin) &key (dx 0) (dy 0))
  #+nil(warn "using unoptimized function: image-alpha-set-fn")
  (let ((sfn (image-rgba-set-fn image :dx dx :dy dy))
        (gfn (image-rgba-get-fn image :dx dx :dy dy)))
    (declare (type image-rgba-get-fn gfn)
             (type image-rgba-set-fn sfn))
    (lambda (x y alpha)
      (declare (type fixnum x y alpha))
      (multiple-value-bind (r g b a) (funcall gfn x y)
        (declare (ignore a))
        (funcall sfn x y r g b alpha)))))

;;; span -set
(deftype image-rgba-set-span-fn () '(function (fixnum fixnum fixnum fixnum
                                               octet octet octet octet)))
(defgeneric image-rgba-set-span-fn (image &key dx dy))
(defmethod image-rgba-set-span-fn ((image rgba-image-mixin) &key (dx 0) (dy 0))
  #+nil(warn "using unoptimized function: image-rgba-set-span-fn")
  (let ((fn (image-rgba-set-fn image :dx dx :dy dy)))
    (declare (type image-rgba-set-fn fn))
    (lambda (x1 y1 x2 y2 red green blue alpha)
      (declare (type fixnum x1 y1 x2 y2)
               (type octet red green blue alpha))
      (loop for y from y1 to y2 do
           (loop for x from x1 to x2 do
                (funcall fn x y red green blue alpha))))))
(deftype image-rgb-set-span-fn () '(function (fixnum fixnum fixnum fixnum
                                               octet octet octet)))
(defgeneric image-rgb-set-span-fn (image &key dx dy))
(defmethod image-rgb-set-span-fn ((image rgb-image-mixin) &key (dx 0) (dy 0))
  #+nil(warn "using unoptimized function: image-rgb-set-span-fn")
  (let ((fn (image-rgb-set-fn image :dx dx :dy dy)))
    (declare (type image-rgb-set-fn fn))
    (lambda (x1 y1 x2 y2 red green blue)
      (declare (type fixnum x1 y1 x2 y2)
               (type octet red green blue))
      (loop for y from y1 to y2 do
           (loop for x from x1 to x2 do
                (funcall fn x y red green blue))))))
(deftype image-gray-set-span-fn () '(function (fixnum fixnum fixnum fixnum
                                               octet)))
(defgeneric image-gray-set-span-fn (image &key dx dy))
(defmethod image-gray-set-span-fn ((image gray-image-mixin) &key (dx 0) (dy 0))
  #+nil(warn "using unoptimized function: image-gray-set-span-fn")
  (let ((fn (image-gray-set-fn image :dx dx :dy dy)))
    (declare (type image-gray-set-fn fn))
    (lambda (x1 y1 x2 y2 gray)
      (declare (type fixnum x1 y1 x2 y2)
               (type octet gray))
      (loop for y from y1 to y2 do
           (loop for x from x1 to x2 do
                (funcall fn x y gray))))))

;;; span - blend
(deftype image-rgba-blend-span-fn () '(function (fixnum fixnum fixnum fixnum
                                                 octet octet octet octet)))
(defgeneric image-rgba-blend-span-fn (image &key dx dy))
(defmethod image-rgba-blend-span-fn ((image rgba-image-mixin) &key (dx 0) (dy 0))
  #+nil (warn "using unoptimized function: image-rgba-blend-span-fn")
  (let ((fn (image-rgba-blend-fn image :dx dx :dy dy)))
    (declare (type image-rgba-blend-fn fn))
    (lambda (x1 y1 x2 y2 red green blue alpha)
      (declare (type fixnum x1 y1 x2 y2)
               (type octet red green blue alpha))
      (loop for y from y1 to y2 do
           (loop for x from x1 to x2 do
                (funcall fn x y red green blue alpha))))))
(deftype image-rgb-blend-span-fn () '(function (fixnum fixnum fixnum fixnum
                                               octet octet octet octet)))
(defgeneric image-rgb-blend-span-fn (image &key dx dy))
(defmethod image-rgb-blend-span-fn ((image rgb-image-mixin) &key (dx 0) (dy 0))
  #+nil (warn "using unoptimized function: image-rgb-blend-span-fn")
  (let ((fn (image-rgb-blend-fn image :dx dx :dy dy)))
    (declare (type image-rgb-blend-fn fn))
    (lambda (x1 y1 x2 y2 red green blue alpha)
      (declare (type fixnum x1 y1 x2 y2)
               (type octet red green blue alpha))
      (loop for y from y1 to y2 do
           (loop for x from x1 to x2 do
                (funcall fn x y red green blue alpha))))))
(deftype image-gray-blend-span-fn () '(function (fixnum fixnum fixnum fixnum
                                               octet octet)))
(defgeneric image-gray-blend-span-fn (image &key dx dy))
(defmethod image-gray-blend-span-fn ((image gray-image-mixin) &key (dx 0) (dy 0))
  #+nil (warn "using unoptimized function: image-gray-blend-span-fn")
  (let ((fn (image-gray-blend-fn image :dx dx :dy dy)))
    (declare (type image-gray-blend-fn fn))
    (lambda (x1 y1 x2 y2 gray alpha)
      (declare (type fixnum x1 y1 x2 y2)
               (type octet gray alpha))
      (loop for y from y1 to y2 do
           (loop for x from x1 to x2 do
                (funcall fn x y gray alpha))))))
;;; span - xor
(deftype image-rgba-xor-blend-span-fn () '(function (fixnum fixnum fixnum fixnum
                                                 octet octet octet octet)))
(defgeneric image-rgba-xor-blend-span-fn (image &key dx dy))
(defmethod image-rgba-xor-blend-span-fn ((image rgba-image-mixin) &key (dx 0) (dy 0))
  #+nil (warn "using unoptimized function: image-rgba-xor-blend-span-fn")
  (let ((fn (image-rgba-xor-blend-fn image :dx dx :dy dy)))
    (declare (type image-rgba-xor-blend-fn fn))
    (lambda (x1 y1 x2 y2 red green blue alpha)
      (declare (type fixnum x1 y1 x2 y2)
               (type octet red green blue alpha))
      (loop for y from y1 to y2 do
           (loop for x from x1 to x2 do
                (funcall fn x y red green blue alpha))))))
(deftype image-rgb-xor-blend-span-fn () '(function (fixnum fixnum fixnum fixnum
                                                 octet octet octet octet)))
(defgeneric image-rgb-xor-blend-span-fn (image &key dx dy))
(defmethod image-rgb-xor-blend-span-fn ((image rgb-image-mixin) &key (dx 0) (dy 0))
  #+nil (warn "using unoptimized function: image-rgb-xor-blend-span-fn")
  (let ((fn (image-rgb-xor-blend-fn image :dx dx :dy dy)))
    (declare (type image-rgb-xor-blend-fn fn))
    (lambda (x1 y1 x2 y2 red green blue alpha)
      (declare (type fixnum x1 y1 x2 y2)
               (type octet red green blue alpha))
      (loop for y from y1 to y2 do
           (loop for x from x1 to x2 do
                (funcall fn x y red green blue alpha))))))
(deftype image-gray-xor-blend-span-fn () '(function (fixnum fixnum fixnum fixnum
                                                 octet octet)))
(defgeneric image-gray-xor-blend-span-fn (image &key dx dy))
(defmethod image-gray-xor-blend-span-fn ((image gray-image-mixin) &key (dx 0) (dy 0))
  #+nil (warn "using unoptimized function: image-gray-xor-blend-span-fn")
  (let ((fn (image-gray-xor-blend-fn image :dx dx :dy dy)))
    (declare (type image-gray-xor-blend-fn fn))
    (lambda (x1 y1 x2 y2 gray alpha)
      (declare (type fixnum x1 y1 x2 y2)
               (type octet gray alpha))
      (loop for y from y1 to y2 do
           (loop for x from x1 to x2 do
                (funcall fn x y gray alpha))))))
;;;
;;; Image operations
;;;
(defgeneric make-image (medium type width height))
(defgeneric coerce-image (image type &optional medium))
(defgeneric clone-image (image type &optional medium))
(defgeneric copy-image (src-image sx sy width height dst-image x y))
(defgeneric blend-image (src-image sx sy width height dst-image x y &key alpha))
(defgeneric crop-image (image sx sy width height &optional type medium))
(defgeneric coerce-alpha-channel (image &optional type medium))
(defgeneric clone-alpha-channel (image &optional type medium))
(defgeneric copy-alpha-channel (src-image sx sy width height dst-image x y))
(defgeneric set-image-color (image red green blue &key alpha x y width height))
(defgeneric set-image (image design &key x y width height))
(defgeneric fill-image-color (image red green blue stencil
                              &key alpha x y width height stencil-dx stencil-dy))
(defgeneric fill-image (image design stencil &key x y width height stencil-dx stencil-dy))
