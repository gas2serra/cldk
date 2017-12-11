(in-package :cldk-internals)

(deftype rgba-image-pixels () '(simple-array (unsigned-byte 32) (* *)))

(defclass image ()
  ((width :initform 0 :initarg :width :accessor image-width :type fixnum)
   (height :initform 0 :initarg :height :accessor image-height :type fixnum)
   (pixels :initarg :pixels
           :accessor image-pixels
           :type rgba-image-pixels)))

(defmethod initialize-instance :after ((image image)
                                       &key)
  (let ((width (image-width image))
        (height (image-height image)))
    (when (and width height (not (slot-boundp image 'pixels)))
      (setf (slot-value image 'pixels)
            (make-array (list height width)
                        :element-type '(unsigned-byte 32)
                        :initial-element #xFFFFFFFF)))))
