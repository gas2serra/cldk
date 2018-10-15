(in-package :cldk-internals)

;;;
;;; Image
;;;
(defclass image ()
  ())

;;;
;;; Image mixins
;;;
(defclass image-mixin ()
  ())

(defclass rgb-image-mixin (image-mixin)
  ())

(defclass buffer-image-mixin (image-mixin)
  ((updated-region-set :initform nil)
   (pixels-lock :initform (bt:make-lock "pixels"))))

;;;
;;; Basic Image
;;;
(defclass basic-image (image)
  ((width :initform 0 :initarg :width :accessor image-width :type fixnum)
   (height :initform 0 :initarg :height :accessor image-height :type fixnum)
   (pixels :initarg :pixels
           :accessor image-pixels)))


;;;
;;; prim
;;;
(deftype octet ()
  '(unsigned-byte 8))


(deftype image-rgb-get-fn () '(function (fixnum fixnum) (values octet octet octet)))
(deftype image-rgb-set-fn () '(function (fixnum fixnum octet octet octet)))

(defgeneric image-rgb-get-fn (image &key dx dy))
(defgeneric image-rgb-set-fn (image &key dx dy))

;;;
;;; Image operations
;;;
(defgeneric %copy-image (src-image sx sy width height dst-image x y))
(defgeneric copy-image (src-image sx sy width height dst-image x y))
(defgeneric copy-image* (src-image rectangle-set dst-image dx dy))
(defmethod copy-image (src-image sx sy width height dst-image x y)
  (%copy-image src-image sx sy width height dst-image x y))

(defmethod copy-image* (src-image rectangle-set dst-image dx dy)
  (map-over-rectangle-set-regions
   #'(lambda (x1 y1 x2 y2)
       (%copy-image src-image x1 y1 (- x2 x1) (- y2 y1) dst-image (+ dx x1) (+ dy y1)))
   rectangle-set))

;;;
;;; Two dimensional array of pixels
;;;
(defclass two-dim-array-image (basic-image)
  ())

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

(defmethod image-rgb-get-fn ((image rgb-image) &key (dx 0) (dy 0))
  (let ((pixels (image-pixels image)))
    (declare (type rgb-image-pixels pixels))
    (lambda (x y)
      (let ((p (aref pixels (+ dy y) (+ dx x))))
        (values (ldb (byte 8 0) p)
                (ldb (byte 8 8) p)
                (ldb (byte 8 16) p)
                (ldb (byte 8 24) p))))))

(defmethod image-rgb-set-fn ((image rgb-image) &key (dx 0) (dy 0))
  (let ((pixels (image-pixels image)))
    (declare (type rgb-image-pixels pixels))
    (lambda (x y r g b)
      (setf (aref pixels (+ dy y) (+ dx x))
            (dpb r (byte 8 0)
                 (dpb g (byte 8 8)
                      (dpb b (byte 8 16)
                           (dpb 255 (byte 8 24) 0))))))))

;;;
;;; copy functions
;;;
(defmacro do-copy-image (src-img sx sy width height dst-img x y (i-var j-var) &body code)
  `(progn
     (let ((max-y (+ ,y ,height -1))
           (max-x (+ ,x ,width -1)))
       (declare (type fixnum max-x max-y))
       (flet ((copy-ff ()
                (loop for ,j-var from y to max-y do
                     (loop for ,i-var from x to max-x do
                          ,@code)))
              (copy-bf ()
                  (loop for ,j-var from y to max-y do
                       (loop for ,i-var from max-x downto x do
                            ,@code)))
              (copy-fb ()
                  (loop for ,j-var from max-y downto y do
                       (loop for ,i-var from x to max-x do
                            ,@code)))
                (copy-bb ()
                  (loop for ,j-var from max-y downto y do
                       (loop for ,i-var from max-x downto x do
                            ,@code))))
         (when (and (> ,width 0) (> ,height 0))
           (if (eq ,src-img ,dst-img)
               (cond
                 ((and (<= ,sx ,x) (<= ,sy ,y))
                  (copy-bb))
                 ((and (<= ,sx ,x) (> ,sy ,y))
                  (copy-bf))
                 ((and (> ,sx ,x) (<= ,sy ,y))
                  (copy-fb))
                 ((and (> ,sx ,x) (> ,sy ,y))
                  (copy-ff)))
               (copy-ff)))))))

(defmacro do-copy-image-fn (src-img src-fn sx sy width height dst-img dst-fn x y (i-var j-var)
                            &body code)
  `(progn
     (let ((dy (- ,sy ,y))
           (dx (- ,sx ,x)))
       (declare (type fixnum ,sx ,sy))
       (let ((src-get-fn (,src-fn src-img :dx dx :dy dy))
             (dst-set-fn (,dst-fn dst-img)))
         (declare (type ,src-fn src-get-fn)
                  (type ,dst-fn dst-set-fn))
         (do-copy-image ,src-img ,sx ,sy ,width ,height ,dst-img ,x ,y (,i-var ,j-var)
                        ,@code)))))

(defmethod %copy-image :around ((src-img image-mixin) sx sy width height
                               (dst-img image-mixin) x y)
  ;; TO FIX: check image bounds
  (call-next-method src-img (max (round sx) 0) (max (round sy) 0)
                    (max 0 (min (round width) (- (image-width dst-img) (max (round x) 0))))
                    (max 0 (min (round height) (- (image-height dst-img) (max (round y) 0))))
                    dst-img (max (round x) 0) (max (round y) 0)))

(defmethod copy-image :around (src-img sx sy width height
                               (dst-img buffer-image-mixin) x y)
  (with-slots (pixels-lock) dst-img
    (bt:with-lock-held (pixels-lock)
      (call-next-method src-img sx sy
                        width
                        height
                        dst-img x y))))

(defmethod copy-image* :around (src-img rectangle-set
                                (dst-img buffer-image-mixin) dx dy)
  (with-slots (pixels-lock) dst-img
    (bt:with-lock-held (pixels-lock)
      (call-next-method src-img rectangle-set
                        dst-img dx dy))))


(defmethod %copy-image :after (src-imgage sx sy width height
                               (dst-img buffer-image-mixin) x y)
  (with-slots (updated-region-set) dst-img
    (setf updated-region-set (rectangle-set-union
                              updated-region-set
                              (rectangle->rectangle-set x y
                                                        (+ x width) (+ y height))))))

(defmacro def-copy-image (src-image-class dst-image-class src-fn dst-fn)
  `(defmethod %copy-image ((src-img ,src-image-class) sx sy width height
                          (dst-img ,dst-image-class) x y)
     (declare (type fixnum width height x y))
     (do-copy-image-fn src-img ,src-fn sx sy width height dst-img ,dst-fn x y (i j)
                       (multiple-value-bind (red green blue)
                           (funcall src-get-fn i j)
                         (funcall dst-set-fn i j red green blue)))))

(def-copy-image image-mixin image-mixin image-rgb-get-fn image-rgb-set-fn)