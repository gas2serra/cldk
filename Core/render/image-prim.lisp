(in-package :cldk-render-internals)

#+nil (declaim (optimize speed))

;;;
;;; Utilities
;;;
(defparameter *alpha-epsilon* 10)

(defmacro with-make-image-bounds ((width height) &body code)
  `(let ((,width (round ,width))
         (,height (round ,height)))
     ,@code))

(defmacro with-set-image-bounds ((src-img x y width height) &body code)
  ;; TO FIX ROUNDING
  `(let ((,x (round ,x))
         (,y (round ,y))
         (,width (round ,width))
         (,height (round ,height)))
     (assert (and (>= ,x 0) (>= ,y 0) (>= ,width 0) (>= ,height 0)))
     (assert (<= (+ ,x ,width) (image-width ,src-img)))
     (assert (<= (+ ,y ,height) (image-height ,src-img)))
     ,@code))

(defmacro with-copy-image-bounds ((src-img sx sy width height dst-img x y) &body code)
  ;; TO FIX ROUNDING
  `(let ((,sx (round ,sx))
         (,sy (round ,sy))
         (,width (round ,width))
         (,height (round ,height))
         (,x (round ,x))
         (,y (round ,y)))
     (assert (and (>= ,sx 0) (>= ,sy 0) (>= ,x 0) (>= ,y 0) (>= ,width 0) (>= ,height 0)))
     (assert (<= (+ ,sx ,width) (image-width ,src-img)))
     (assert (<= (+ ,sy ,height) (image-height src-img)))
     (assert (<= (+ ,x ,width) (image-width ,dst-img)))
     (assert (<= (+ ,y ,height) (image-height ,dst-img)))
     ,@code))

(defmacro with-fill-image-bounds ((image x y width height stencil stencil-dx stencil-dy) &body code)
  ;; TO FIX ROUNDING
  (declare (ignore image stencil))
  `(let ((,x (round ,x))
         (,y (round ,y))
         (,width (round ,width))
         (,height (round ,height))
         (,stencil-dx (round ,stencil-dx))
         (,stencil-dy (round ,stencil-dy)))
     ,@code))

(defmacro do-image (src-img x y width height (i-var j-var) &body code)
  (declare (ignore src-img))
  `(progn
     (when (and (> ,width 0) (> ,height 0))
       (let ((max-y (+ ,y ,height -1))
             (max-x (+ ,x ,width -1)))
         (declare (type fixnum max-x max-y))
         (loop for ,j-var fixnum from ,y to max-y do
              (loop for ,i-var fixnum from ,x to max-x do
                   ,@code))))
     (make-rectangle* ,x ,y (+ ,x ,width) (+ ,y ,height))))

(defmacro do-copy-image (src-img sx sy width height dst-img x y (i-var j-var) &body code)
  `(progn
     (let ((max-y (+ ,y ,height -1))
           (max-x (+ ,x ,width -1)))
       (declare (type fixnum max-x max-y))
       (flet ((copy-ff ()
                (loop for ,j-var fixnum from ,y to max-y do
                     (loop for ,i-var fixnum from ,x to max-x do
                          ,@code)))
              (copy-bf ()
                  (loop for ,j-var fixnum from ,y to max-y do
                       (loop for ,i-var fixnum from max-x downto ,x do
                            ,@code)))
              (copy-fb ()
                  (loop for ,j-var fixnum from max-y downto ,y do
                       (loop for ,i-var fixnum from ,x to max-x do
                            ,@code)))
                (copy-bb ()
                  (loop for ,j-var fixnum from max-y downto ,y do
                       (loop for ,i-var fixnum from max-x downto ,x do
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
               (copy-ff)))))
     (make-rectangle* ,x ,y (+ ,x ,width) (+ ,y ,height))))

;;;
;;; generic primitives
;;;

;; -fn
(declaim (inline %image-get-fn %image-set-fn %image-set-span-fn %image-blend-fn))
(defun %image-get-fn (image &key (dx 0) (dy 0) region)
  (case (image-type image)
    (:rgba
     (image-rgba-get-fn image :dx dx :dy dy :region region))
    (:rgb
     (image-rgb-get-fn image :dx dx :dy dy :region region))
    (:gray
     (image-gray-get-fn image :dx dx :dy dy :region region))))
(defun %image-set-fn (image &key (dx 0) (dy 0))
  (case (image-type image)
    (:rgba
     (image-rgba-set-fn image :dx dx :dy dy))
    (:rgb
     (image-rgb-set-fn image :dx dx :dy dy))
    (:gray
     (image-gray-set-fn image :dx dx :dy dy))))
(defun %image-set-span-fn (image &key (dx 0) (dy 0))
  (case (image-type image)
    (:rgba
     (image-rgba-set-span-fn image :dx dx :dy dy))
    (:rgb
     (image-rgb-set-span-fn image :dx dx :dy dy))
    (:gray
     (image-gray-set-span-fn image :dx dx :dy dy))))
(defun %image-blend-fn (image &key (dx 0) (dy 0))
  (case (image-type image)
    (:rgba
     (image-rgba-blend-fn image :dx dx :dy dy))
    (:rgb
     (image-rgb-blend-fn image :dx dx :dy dy))
    (:gray
     (image-gray-blend-fn image :dx dx :dy dy))))
(defun %image-xor-blend-fn (image &key (dx 0) (dy 0))
  (case (image-type image)
    (:rgba
     (image-rgba-xor-blend-fn image :dx dx :dy dy))
    (:rgb
     (image-rgb-xor-blend-fn image :dx dx :dy dy))
    (:gray
     (image-gray-xor-blend-fn image :dx dx :dy dy))))
(defun %image-blend-span-fn (image &key (dx 0) (dy 0))
  (case (image-type image)
    (:rgba
     (image-rgba-blend-span-fn image :dx dx :dy dy))
    (:rgb
     (image-rgb-blend-span-fn image :dx dx :dy dy))
    (:gray
     (image-gray-blend-span-fn image :dx dx :dy dy))))
(defun %image-xor-blend-span-fn (image &key (dx 0) (dy 0))
  (case (image-type image)
    (:rgba
     (image-rgba-xor-blend-span-fn image :dx dx :dy dy))
    (:rgb
     (image-rgb-xor-blend-span-fn image :dx dx :dy dy))
    (:gray
     (image-gray-xor-blend-span-fn image :dx dx :dy dy))))
;; call-
(declaim (inline %call-image-rgba-get-fn))
(defun %call-image-rgba-get-fn (image-type get-fn x y)
  (declare (type fixnum x y))
  (multiple-value-bind (red green blue alpha)
      (case image-type
        (:rgba
         (let ()
           (declare (type image-rgba-get-fn get-fn))
           (funcall get-fn x y)))
        (:rgb
         (let ()
           (declare (type image-rgb-get-fn get-fn))
           (multiple-value-bind (r g b)
               (funcall get-fn x y)
             (rgb->rgba r g b))))
        (:gray
         (let ()
           (declare (type image-gray-get-fn get-fn))
           (multiple-value-bind (g)
               (funcall get-fn x y)
             (gray->rgba g)))))
    (values red green blue alpha)))
(declaim (inline %call-image-rgb-get-fn))
(defun %call-image-rgb-get-fn (image-type get-fn x y)
  (declare (type fixnum x y))
  (multiple-value-bind (red green blue)
      (case image-type
        (:rgba
         (let ()
           (declare (type image-rgba-get-fn get-fn))
           (funcall get-fn x y)))
        (:rgb
         (let ()
           (declare (type image-rgb-get-fn get-fn))
           (funcall get-fn x y)))
        (:gray
         (let ()
           (declare (type image-gray-get-fn get-fn))
           (multiple-value-bind (g)
               (funcall get-fn x y)
             (gray->rgb g)))))
    (values red green blue)))
(declaim (inline %call-image-gray-get-fn))
(defun %call-image-gray-get-fn (image-type get-fn x y)
  (declare (type fixnum x y))
  (multiple-value-bind (gray)
      (case image-type
        (:rgba
         (let ()
           (declare (type image-rgba-get-fn get-fn))
           (multiple-value-bind (red green blue alpha)
               (funcall get-fn x y)
             (rgba->gray red green blue alpha))))
        (:rgb
         (let ()
           (declare (type image-rgb-get-fn get-fn))
           (multiple-value-bind (red green blue)
               (funcall get-fn x y)
             (rgb->gray red green blue))))
        (:gray
         (let ()
           (declare (type image-gray-get-fn get-fn))
           (funcall get-fn x y))))
    (values gray)))

;; call- set
(declaim (inline %set-image-fn))
(defun %call-set-image-fn (image-type set-fn x y red green blue alpha)
  (declare (type octet red green blue alpha *alpha-epsilon*))
  (case image-type
    (:rgba
     (let ()
       (declare (type image-rgba-set-fn set-fn))
       (funcall set-fn x y red green blue alpha)))
    (:rgb
     (let ()
       (declare (type image-rgb-set-fn set-fn))
       (multiple-value-bind (r g b)
           (rgba->rgb red green blue alpha)
         (funcall set-fn x y r g b))))
    (:gray
     (let ()
       (declare (type image-gray-set-fn set-fn))
       (funcall set-fn x y (rgba->gray red green blue alpha))))))

(declaim (inline %call-set-image-span-fn))
(defun %call-set-image-span-fn (image-type set-fn x1 y1 x2 y2 red green blue alpha)
  (declare (type octet red green blue alpha *alpha-epsilon*)
           (type fixnum x1 x2 y1 y2))
  (case image-type
    (:rgba
     (let ()
       (declare (type image-rgba-set-span-fn set-fn))
       (funcall set-fn x1 y1 x2 y2 red green blue alpha)))
    (:rgb
     (let ()
       (declare (type image-rgb-set-span-fn set-fn))
       (multiple-value-bind (r g b)
           (rgba->rgb red green blue alpha)
         (funcall set-fn x1 y1 x2 y2 r g b))))
    (:gray
     (let ()
       (declare (type image-gray-set-span-fn set-fn))
       (funcall set-fn x1 y1 x2 y2 (rgba->gray red green blue alpha))))))

;; call blend
(declaim (inline %call-blend-image-fn))
(defun %call-blend-image-fn (image-type set-fn x y red green blue alpha)
  (declare (type octet red green blue alpha *alpha-epsilon*))
  (case image-type
    (:rgba
     (let ()
       (declare (type image-rgba-blend-fn set-fn))
       (when (> alpha *alpha-epsilon*)
         (funcall set-fn x y red green blue alpha))))
    (:rgb
     (let ()
       (declare (type image-rgb-blend-fn set-fn))
       (when (> alpha *alpha-epsilon*)
         (funcall set-fn x y red green blue alpha))))
    (:gray
     (let ()
       (declare (type image-gray-blend-fn set-fn))
       (when (> alpha *alpha-epsilon*)
         (funcall set-fn x y (rgb->gray red green blue) alpha))))))

(declaim (inline %call-blend-image-span-fn))
(defun %call-blend-image-span-fn (image-type set-fn x1 y1 x2 y2 red green blue alpha)
  (declare (type octet red green blue alpha *alpha-epsilon*)
           (type fixnum x1 x2 y1 y2))
  (case image-type
    (:rgba
     (let ()
       (declare (type image-rgba-blend-span-fn set-fn))
       (when (> alpha *alpha-epsilon*)
         (funcall set-fn x1 y1 x2 y2 red green blue alpha))))
    (:rgb
     (let ()
       (declare (type image-rgb-blend-span-fn set-fn))
       (when (> alpha *alpha-epsilon*)
         (funcall set-fn x1 y1 x2 y2 red green blue alpha))))
    (:gray
     (let ()
       (declare (type image-gray-blend-span-fn set-fn))
       (when (> alpha *alpha-epsilon*)
         (funcall set-fn x1 y1 x2 y2 (rgb->gray red green blue) alpha))))))

