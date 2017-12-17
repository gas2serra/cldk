(in-package :cldk-image-internals)

(defgeneric region-contains-position-p (region x y))

(defclass rectangle ()
  ((x1 :initarg :x1)
   (y1 :initarg :y1)
   (x2 :initarg :x2)
   (y2 :initarg :y2)))
   
(defun make-rectangle* (x1 y1 x2 y2)
  (make-instance 'rectangle :x1 x1 :y1 y1 :x2 x2 :y2 y2))

(defgeneric region-equal (r1 r2))
