(in-package :cldk-internals)
;;;
;;;
;;;

(defclass shared-image (cldk-render-internals::basic-image)
  ((device :initarg :device :reader image-device)
   (updated-region-set :initform nil)
   (pixels-lock :initform (bt:make-lock "pixels"))))

(defgeneric update-image (image width height))

(defmethod cldk-render:copy-image :around (src-image sx sy width height (dst-image shared-image) x y)
  ;;(with-slots (pixels-lock) dst-image
    ;;(bt:with-lock-held (pixels-lock)
      (call-next-method src-image sx sy
                        width height
                        dst-image x y))

(defmethod cldk-render:copy-image :after (src-image sx sy width height (dst-image shared-image) x y)
  (with-slots (updated-region-set) dst-image
    (setf updated-region-set (rectangle-set-union
                              updated-region-set
                              (rectangle->rectangle-set x y
                                                        (+ x width) (+ y height))))))

(defmethod copy-image* (src-image rectangle-set (dst-image shared-image) dx dy)
  (map-over-rectangle-set-regions
   #'(lambda (x1 y1 x2 y2)
       (cldk-render:copy-image src-image x1 y1 (- x2 x1) (- y2 y1)
                               dst-image (+ dx x1) (+ dy y1)))
   rectangle-set))

(defmethod copy-image* :around (src-img rectangle-set
                                (dst-img shared-image) dx dy)
  (with-slots (pixels-lock) dst-img
    (bt:with-lock-held (pixels-lock)
      (call-next-method src-img rectangle-set
                        dst-img dx dy))))
