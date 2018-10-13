(in-package :cldk-mcclim-render-internals)

(defclass opticl-image-adapter (image-adapter-mixin)
  ((width :initform 0 :initarg :width :reader image-width :type fixnum)
   (height :initform 0 :initarg :height :reader image-height :type fixnum)))

(defclass opticl-rgb-image-adapter (opticl-image-adapter rgb-image-mixin)
  ())

(defmethod image-rgb-get-fn ((image opticl-rgb-image-adapter) &key (dx 0) (dy 0) (region nil))
  (with-slots (img) image
    (declare (type fixnum dx dy))
    (etypecase img
      (opticl:8-bit-rgb-image      
       (lambda (x y)
         (declare (type fixnum x y))
         (if (or (not region) (clim:region-contains-position-p region x y))
             (opticl:pixel img (+ y dy) (+ x dx))
             (values 0 0 0))))
      (opticl:16-bit-rgb-image      
       (lambda (x y)
         (declare (type fixnum x y))
         (if (or (not region) (clim:region-contains-position-p region x y))
             (multiple-value-bind (r g b)
                 (opticl:pixel img (+ y dy) (+ x dx))
               (values (ash r -8) (ash g -8) (ash b -8)))
             (values 0 0 0))))
      (t
       (error "unknown opticl image type (~A)" (type-of img))))))

;;; rgba
(defclass opticl-rgba-image-adapter (opticl-image-adapter rgba-image-mixin) 
  ())

(defmethod image-rgba-get-fn ((image opticl-rgba-image-adapter) &key (dx 0) (dy 0) (region nil))
  (with-slots (img) image
    (declare (type fixnum dx dy))
    (etypecase img
      (opticl:8-bit-rgba-image      
       (lambda (x y)
         (declare (type fixnum x y))
         (if (or (not region) (clim:region-contains-position-p region x y))
             (opticl:pixel img (+ y dy) (+ x dx))
             (values 0 0 0 0))))
      (opticl:16-bit-rgba-image      
       (lambda (x y)
         (declare (type fixnum x y))
         (if (or (not region) (clim:region-contains-position-p region x y))
             (multiple-value-bind (r g b a)
                 (opticl:pixel img (+ y dy) (+ x dx))
               (values (ash r -8) (ash g -8) (ash b -8) (ash a -8)))
             (values 0 0 0 0))))
      (t
       (error "unknown opticl image type (~A)" (type-of img))))))

;;; gray
(defclass opticl-gray-image-adapter (opticl-image-adapter gray-image-mixin) 
  ())

(defmethod image-gray-get-fn ((image opticl-gray-image-adapter) &key (dx 0) (dy 0) (region nil))
  (with-slots (img) image
    (declare (type fixnum dx dy))
    (etypecase img
      (opticl:8-bit-gray-image
       (lambda (x y)
         (declare (type fixnum x y))
         (if (or (not region) (clim:region-contains-position-p region x y))
             (opticl:pixel img (+ y dy) (+ x dx))
             0)))
      (opticl:1-bit-gray-image
       (lambda (x y)
         (declare (type fixnum x y))
         (if (or (not region) (clim:region-contains-position-p region x y))
             (* 255 (opticl:pixel img (+ y dy) (+ x dx)))
             0)))
      (t
       (error "unknown opticl image type (~A)" (type-of img))))))

(defun make-opticl-image-adapter (opticl-pixels)
  (opticl:with-image-bounds (height width channels) opticl-pixels
    (cond ((or (not channels)
               (= channels 1))
           (make-instance 'opticl-gray-image-adapter
                          :width width :height height :image opticl-pixels))
          ((= channels 4)
           (make-instance 'opticl-rgba-image-adapter
                          :width width :height height :image opticl-pixels))
          ((= channels 3)
           (make-instance 'opticl-rgb-image-adapter
                          :width width :height height :image opticl-pixels))
          (t
           (error "unknown opticl image type (~A) of ~A channels"
                  (type-of opticl-pixels) channels)))))
