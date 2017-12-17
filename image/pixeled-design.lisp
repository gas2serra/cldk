(in-package :cldk-image-internals)

(deftype pixeled-design-fn () '(function (fixnum fixnum) (values octet octet octet octet)))

;;;
;;; Pixeled Design
;;;
(defclass pixeled-design ()
  ((region :initarg :region :initform t :type region
           :accessor pixeled-design-region)))

(defgeneric pixeled-rgba-fn (design))
(defgeneric pixeled-rgba-unsafe-fn (design))

(defmethod pixeled-rgba-fn :around (design)
  (with-slots (region)
      design
    (if (region-equal region t)
        (pixeled-rgba-unsafe-fn design)
        (call-next-method))))

;;;
;;; Uniform Design
;;;
(defclass pixeled-uniform-design (pixeled-design)
  ((red :initarg :red :type octet :initform 0
        :accessor pixeled-uniform-design-red)
   (green :initarg :green :type octet :initform 0
          :accessor pixeled-uniform-design-green)
   (blue :initarg :blue :type octet :initform 0
         :accessor pixeled-uniform-design-blue)
   (alpha :initarg :alpha :type octet :initform 0
          :accessor pixeled-uniform-design-alpha)))

(defun make-pixeled-uniform-design (&key (red 0) (green 0) (blue 0) (alpha 255))
  (make-instance 'pixeled-uniform-design :red red :green green :blue blue :alpha alpha))

(defmethod pixeled-rgba-fn ((design pixeled-uniform-design))
  (with-slots (red green blue alpha region)
      design
    (lambda (x y)
      (if (region-contains-position-p region x y)
          (values red green blue alpha)
          (values 0 0 0 0)))))

(defmethod pixeled-rgba-unsafe-fn ((design pixeled-uniform-design))
  (with-slots (red green blue alpha region)
      design
    (lambda (x y)
      (declare (ignore x y))
      (values red green blue alpha))))

;;;
;;; Functiona Design
;;;
(defclass pixeled-functional-design (pixeled-design)
  ((color-fn :initarg :color-fn :type pixeled-design-fn)))

(defun make-pixeled-functional-design (&key color-fn (region t))
  (make-instance 'pixeled-functional-design :color-fn color-fn :region region))

(defmethod pixeled-rgba-fn ((design pixeled-functional-design))
  (with-slots (color-fn region)
      design
    (declare (type pixeled-design-fn color-fn))
    (lambda (x y)
      (declare (type fixnum x y))
      (if (region-contains-position-p region x y)
          (funcall color-fn x y)
          (values 0 0 0 0)))))

(defmethod pixeled-rgba-unsafe-fn ((design pixeled-functional-design))
  (with-slots (color-fn region)
      design
    (declare (type pixeled-design-fn color-fn))
    color-fn))

;;;
;;; Flippend Design
;;;
(defclass pixeled-flipping-design (pixeled-functional-design)
  ())

(defun make-pixeled-flipping-design (&key color-fn (region t))
  (make-instance 'pixeled-flipping-design :color-fn color-fn :region region))

;;;
;;; Image Design
;;;
(defclass pixeled-image-design (pixeled-design)
  ((image :initarg :image :initform nil
          :accessor pixeled-image-design-image)
   (dx :initarg :dx :initform 0 :type fixnum
       :accessor pixeled-image-design-dx)
   (dy :initarg :dy :initform 0 :type fixnum
       :accessor pixeled-image-design-dy)))

(defun make-pixeled-image-design (&key (image nil))
  (make-instance 'pixeled-image-design
                 :image image
                 :region (make-rectangle* 0 0 (1- (image-width image)) (1- (image-height image)))))

(defmethod  pixeled-rgba-fn ((design pixeled-image-design))
  (with-slots (image dx dy region)
      design
    (image-rgba-get-fn image :dx dx :dy dy :region region)))

(defmethod  pixeled-rgba-unsafe-fn ((design pixeled-image-design))
  (with-slots (image dx dy region)
      design
    (image-rgba-get-fn image :dx dx :dy dy :region nil)))

;;;
;;; Make a pixeled design
;;;
(defgeneric %make-pixeled-design (design))

(defgeneric make-pixeled-design (design &key foreground background)
  (:method (design &key foreground background)
    (let ((*pixeled-foreground-design* (or foreground *pixeled-foreground-design*))
          (*pixeled-background-design* (or background *pixeled-background-design*)))
      (%make-pixeled-design design))))

(defmethod %make-pixeled-design (ink)
  (error "unknow how to make an rgba design of the ~A" ink))

(defmethod %make-pixeled-design ((ink cl-colors:rgb))
  (multiple-value-bind (red green blue)
      (color->octets ink)
    (make-pixeled-uniform-design
     :red red
     :green green
     :blue blue
     :alpha 255)))


