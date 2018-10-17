(in-package :cldk-render-internals)

#+nil (declaim (optimize speed))

;;;
;;; Two dimensional array of pixels
;;;
(defclass two-dim-array-image (basic-image)
  ())

(defmethod image-medium ((image two-dim-array-image))
  :two-dim-array)

;;;
;;; RGBA
;;;
(deftype rgba-image-pixels () '(simple-array (unsigned-byte 32) (* *)))

(defclass rgba-image (two-dim-array-image rgba-image-mixin)
  ((pixels :type rgba-image-pixels)))

(defmethod initialize-instance :after ((image rgba-image)
                                       &key)
  (let ((width (image-width image))
        (height (image-height image)))
    (when (and width height (not (slot-boundp image 'pixels)))
      (setf (slot-value image 'pixels)
            (make-array (list height width)
                        :element-type '(unsigned-byte 32)
                        :initial-element #xFFFFFFFF)))))

(defmethod image-rgba-get-fn ((image rgba-image) &key (dx 0) (dy 0) (region nil))
  (let ((pixels (image-pixels image)))
    (declare (type rgba-image-pixels pixels)
             (type fixnum dx dy))
    (lambda (x y)
      (declare (type fixnum x y))
      (if (or (not region) (clim:region-contains-position-p region x y))
          (let ((p (aref pixels (+ y dy) (+ x dx))))
            (values (ldb (byte 8 0) p)
                    (ldb (byte 8 8) p)
                    (ldb (byte 8 16) p)
                    (ldb (byte 8 24) p)))
          (values 0 0 0 0)))))

(defmethod image-rgba-set-fn ((image rgba-image) &key (dx 0) (dy 0))
  (let ((pixels (image-pixels image)))
    (declare (type rgba-image-pixels pixels)
             (type fixnum dx dy))
    (lambda (x y red green blue alpha)
      (declare (type fixnum x y)
               (type octet red green blue alpha))
      (setf (aref pixels (+ y dy) (+ x dx))
            (dpb red (byte 8 0)
                 (dpb green (byte 8 8)
                      (dpb blue (byte 8 16)
                           (dpb alpha (byte 8 24) 0))))))))

;;;
;;; RGB
;;;
(deftype rgb-image-pixels () '(simple-array (unsigned-byte 32) (* *)))

(defclass rgb-image (two-dim-array-image rgb-image-mixin)
  ((pixels :type rgb-image-pixels)))

(defmethod initialize-instance :after ((image rgb-image)
                                       &key)
  (let ((width (image-width image))
        (height (image-height image)))
    (when (and width height (not (slot-boundp image 'pixels)))
      (setf (slot-value image 'pixels)
            (make-array (list height width)
                        :element-type '(unsigned-byte 32)
                        :initial-element #xFFFFFFFF)))))

(defmethod image-rgb-get-fn ((image rgb-image) &key (dx 0) (dy 0) (region nil))
  (let ((pixels (image-pixels image)))
    (declare (type rgb-image-pixels pixels)
             (type fixnum dx dy))
    (lambda (x y)
      (declare (type fixnum x y))
      (if (or (not region) (clim:region-contains-position-p region x y))
          (let ((p (aref pixels (+ y dy) (+ x dx))))
            (values (ldb (byte 8 0) p)
                    (ldb (byte 8 8) p)
                    (ldb (byte 8 16) p)))
          (values 0 0 0)))))

(defmethod image-rgb-set-fn ((image rgb-image) &key (dx 0) (dy 0))
  (let ((pixels (image-pixels image)))
    (declare (type rgb-image-pixels pixels)
             (type fixnum dx dy))
    (lambda (x y red green blue)
      (declare (type fixnum x y)
               (type octet red green blue))
      (setf (aref pixels (+ y dy) (+ x dx))
            (dpb red (byte 8 0)
                 (dpb green (byte 8 8)
                      (dpb blue (byte 8 16)
                           (dpb 255 (byte 8 24) 0))))))))

;;;
;;; Gray
;;;
(deftype gray-image-pixels () '(simple-array (unsigned-byte 8) (* *)))

(defclass gray-image (two-dim-array-image gray-image-mixin)
  ((pixels :type gray-image-pixels)))

(defmethod initialize-instance :after ((image gray-image)
                                       &key)
  (let ((width (image-width image))
        (height (image-height image)))
    (when (and width height (not (slot-boundp image 'pixels)))
      (setf (slot-value image 'pixels)
            (make-array (list height width)
                        :element-type '(unsigned-byte 8)
                        :initial-element #x00)))))

(defmethod image-gray-get-fn ((image gray-image) &key (dx 0) (dy 0) (region nil))
  (let ((pixels (image-pixels image)))
    (declare (type gray-image-pixels pixels)
             (type fixnum dx dy))
    (lambda (x y)
      (declare (type fixnum x y))
      (if (or (not region) (clim:region-contains-position-p region x y))
          (aref pixels (+ y dy) (+ x dx))
          0))))

(defmethod image-gray-set-fn ((image gray-image) &key (dx 0) (dy 0))
  (let ((pixels (image-pixels image)))
    (declare (type opticl-gray-image-pixels pixels)
             (type fixnum dx dy))
    (lambda (x y gray)
      (declare (type fixnum x y)
               (type octet gray))
      (setf (aref pixels (+ y dy) (+ x dx))
            gray))))

;;;
;;; making
;;;
(defmethod make-image ((medium (eql :two-dim-array)) (type (eql :rgba)) width height)
  (make-instance 'rgba-image :width width :height height))

(defmethod make-image ((medium (eql :two-dim-array)) (type (eql :rgb)) width height)
  (make-instance 'rgb-image :width width :height height))

(defmethod make-image ((medium (eql :two-dim-array)) (type (eql :gray)) width height)
  (make-instance 'gray-image :width width :height height))

(defmethod make-image ((medium (eql :two-dim-array)) (type (eql :auto)) width height)
  (make-instance 'rgba-image :width width :height height))
