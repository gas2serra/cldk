(in-package :cldk-internals)

#+nil (declaim (optimize speed))

;;;
;;; make
;;;
(defvar *default-image-medium* :two-dim-array)

(defmethod make-image :around (medium type width height)
  (with-make-image-bounds (width height)
    (call-next-method medium type width height)))

(defmethod make-image ((medium (eql :default)) type width height)
  (make-image *default-image-medium* type width height))

;;;
;;; coerce
;;;
(defmethod coerce-image :around ((image image-mixin) type &optional medium)
  (if (eql type :auto)
      (call-next-method image (image-type image) (or medium (image-medium image)))
      (call-next-method image type (or medium (image-medium image)))))

(defmethod coerce-image ((image image-mixin) type &optional medium)
  (if (and (eql (image-type image) type)
           (eql (image-medium image) medium))
      image
      (clone-image image type medium)))

;;;
;;; clone
;;;
(defmethod clone-image :around ((image image-mixin) type &optional medium)
  (if (eql type :auto)
      (call-next-method image (image-type image) (or medium (image-medium image)))
      (call-next-method image type (or medium (image-medium image)))))

(defmethod clone-image ((image image-mixin) type &optional medium)
  (let ((dest (make-image medium type (image-width image) (image-height image))))
    (copy-image image 0 0 (image-width image) (image-height image) dest 0 0)
    dest))

;;;
;;; copy
;;;
(defmethod copy-image :around ((src-img image-mixin) sx sy width height
                               (dst-img image-mixin) x y)
  (with-copy-image-bounds (src-img sx sy width height dst-img x y)
    (call-next-method src-img sx sy width height dst-img x y)
    (values x y (+ x width) (+ y height))))

(declaim (inline %call-image-copy-fn))
(defun %call-image-copy-fn (src-image-type dst-image-type get-fn set-fn x y)
  (declare (type fixnum x y)
           (type symbol src-image-type dst-image-type))
  (case dst-image-type
    (:rgba
     (let ()
       (declare (type image-rgba-set-fn set-fn))
       (multiple-value-bind (red green blue alpha)
           (%call-image-rgba-get-fn src-image-type get-fn x y)
         (declare (type octet red green blue alpha))
         (funcall set-fn x y red green blue alpha))))
    (:rgb
     (let ()
       (declare (type image-rgb-set-fn set-fn))
       (multiple-value-bind (red green blue)
           (%call-image-rgb-get-fn src-image-type get-fn x y)
         (declare (type octet red green blue))
         (funcall set-fn x y red green blue))))
    (:gray
     (let ()
       (declare (type image-gray-set-fn set-fn))
       (multiple-value-bind (gray)
           (%call-image-gray-get-fn src-image-type get-fn x y)
         (declare (type octet gray))
         (funcall set-fn x y gray))))))

(defmethod copy-image ((src-img image-mixin) sx sy width height
                       (dst-img image-mixin) x y)
  (declare (type fixnum x y sx sy width height))
  (let ((dy (- sy y))
        (dx (- sx x)))
    (declare (type fixnum dx dy))
    (let ((set-fn (%image-set-fn dst-img))
          (get-fn (%image-get-fn src-img :dx dx :dy dy))
          (src-image-type (image-type src-img))
          (dst-image-type (image-type dst-img)))
      (do-copy-image src-img sx sy width height dst-img x y (i j)
        (%call-image-copy-fn src-image-type dst-image-type get-fn set-fn i j)))))

;;;
;;; blend
;;;
(defmethod blend-image :around ((src-img image-mixin) sx sy width height
                                (dst-img image-mixin) x y &key (alpha 255))
  (with-copy-image-bounds (src-img sx sy width height dst-img x y)
    (call-next-method src-img sx sy width height dst-img x y :alpha (round alpha))
    (values x y (+ x width) (+ y height))))

(declaim (inline %call-image-blend-fn))
(defun %call-image-blend-fn (src-image-type dst-image-type get-fn blend-fn x y salpha)
  (declare (type fixnum x y)
           (type octet salpha)
           (type symbol src-image-type dst-image-type))
  (case dst-image-type
    (:rgba
     (let ()
       (declare (type image-rgba-blend-fn blend-fn))
       (multiple-value-bind (red green blue alpha)
           (%call-image-rgba-get-fn src-image-type get-fn x y)
         (setf alpha (octet-mult salpha alpha))
         (when (> alpha *alpha-epsilon*)
           (funcall blend-fn x y red green blue alpha)))))
    (:rgb
     (let ()
       (declare (type image-rgb-blend-fn blend-fn))
       (multiple-value-bind (red green blue alpha)
           (%call-image-rgba-get-fn src-image-type get-fn x y)
         (setf alpha (octet-mult salpha alpha))
         (when (> alpha *alpha-epsilon*)
           (funcall blend-fn x y red green blue alpha)))))
    (:gray
     (let ()
       (declare (type image-gray-blend-fn blend-fn))
       (multiple-value-bind (red green blue alpha)
           (%call-image-rgba-get-fn src-image-type get-fn x y)
         (setf alpha (octet-mult salpha alpha)) 
         (when (> alpha *alpha-epsilon*)
           (funcall blend-fn x y (rgb->gray red green blue) alpha)))))))

(defmethod blend-image ((src-img image-mixin) sx sy width height
                        (dst-img image-mixin) x y &key (alpha 255))
  (declare (type fixnum x y sx sy width height))
  (let ((dy (- sy y))
        (dx (- sx x)))
    (declare (type fixnum dx dy))
    (let ((blend-fn (%image-blend-fn dst-img))
          (get-fn (%image-get-fn src-img :dx dx :dy dy))
          (src-image-type (image-type src-img))
          (dst-image-type (image-type dst-img)))
       (do-copy-image src-img sx sy width height dst-img x y (i j)
        (%call-image-blend-fn src-image-type dst-image-type get-fn blend-fn i j alpha)))))

;;;
;;; crop
;;;
(defmethod crop-image :around ((image image-mixin) sx sy width height &optional type medium)
  (call-next-method image sx sy width height (or type (image-type image))
                    (or medium (image-medium image))))

(defmethod crop-image ((image image-mixin) sx sy width height &optional type medium)
  (let ((dest (make-image medium type width height)))
    (copy-image image sx sy width height dest 0 0)
    dest))

;;;
;;; alpha channel
;;;
(defmethod copy-alpha-channel :around ((src-img image-mixin) sx sy width height
                                       (dst-img image-mixin) x y)
  (with-copy-image-bounds (src-img sx sy width height dst-img x y)
    (call-next-method src-img sx sy width height dst-img x y)
    (values x y (+ x width) (+ y height))))

(defmethod copy-alpha-channel ((src-img image-mixin) sx sy width height
                               (dst-img image-mixin) x y)
  (declare (type fixnum x y sx sy width height))
  (let ((dy (- sy y))
        (dx (- sx x)))
    (declare (type fixnum dx dy))
    (let ((set-fn (image-alpha-set-fn dst-img))
          (get-fn (image-alpha-get-fn src-img :dx dx :dy dy)))
      (declare (type image-alpha-set-fn set-fn)
               (type image-alpha-get-fn get-fn))
      (do-copy-image src-img sx sy width height dst-img x y (i j)
        (funcall set-fn i j (funcall get-fn i j))))))

(defmethod coerce-alpha-channel :around ((image image-mixin) &optional (type :gray) medium)
  (call-next-method image type (or medium (image-medium image))))

(defmethod coerce-alpha-channel ((image image-mixin) &optional type medium)
  (if (and (eql (image-type image) type)
           (eql (image-medium image) medium))
      image
      (clone-alpha-channel image type medium)))

(defmethod clone-alpha-channel :around ((image image-mixin) &optional (type :gray) medium)
  (call-next-method image type (or medium (image-medium image))))

(defmethod clone-alpha-channel ((image image-mixin) &optional type medium)
  (let ((dest (make-image medium type (image-width image) (image-height image))))
    (copy-alpha-channel image 0 0 (image-width image) (image-height image) dest 0 0)
    dest))

;;;
;;; I/O
;;;
(defmethod read-image (pathname &key format type medium)
  (unless format
    (setf format (intern (string-upcase
                          (pathname-type (pathname pathname)))
                         (find-package :keyword))))
  (if (image-format-read-supported-p format)
      (let ((image (funcall (gethash format *image-file-readers*)
                            pathname)))
        (if (or type medium)
            (coerce-image image type medium)
            image))
      (error "image format not supproted, yet")))

(defmethod write-image (image destination &key format quality)
  (declare (ignorable quality))
  (unless format
    (setf format (intern (string-upcase
                          (pathname-type (pathname destination)))
                         (find-package :keyword))))
  (if (image-format-write-supported-p format)
      (funcall (gethash format *image-file-writer*)
               image destination)
      (error "image format not supproted, yet")))

;;;
;;; set color
;;;

(defmethod set-image-color :around (image red green blue &key (alpha 255) (x 0) (y 0)
                                                           (width (image-width image))
                                                           (height (image-height image)))
  (declare (type octet red green blue alpha))
  (with-set-image-bounds (image x y width height)
    (call-next-method image red green blue :alpha alpha
                      :x x :y y :width width :height height)
    (values x y (+ x width) (+ y height))))

(defmethod set-image-color ((image image-mixin) red green blue &key alpha x y width height)
  (declare (type fixnum x y width height))
  (let ((set-span-fn (%image-set-span-fn image))
        (image-type (image-type image)))
    (let ((max-x (+ x width -1))
          (max-y (+ y height -1)))
      (declare (type fixnum max-x max-y))
      (%call-set-image-span-fn image-type set-span-fn
                               x y max-x max-y red green blue alpha))))

;;;
;;; set
;;;
(defmethod set-image :around ((image image-mixin) design
                               &key (x 0) (y 0)
                                 (width (image-width image))
                                 (height (image-height image)))
  (with-set-image-bounds (image x y width height)
    (call-next-method image design :x x :y y
                      :width width :height height)
    (values x y (+ x width) (+ y height))))




;;;
;;; fill color
;;;

(defmethod fill-image-color :around (image red green blue stencil
                                     &key (alpha 255) (x 0) (y 0)
                                       (width (image-width image))
                                       (height (image-height image))
                                       (stencil-dx 0) (stencil-dy 0))
  (declare (type octet red green blue alpha))
  (with-fill-image-bounds (image x y width height stencil stencil-dx stencil-dy)
    (call-next-method image red green blue stencil :alpha alpha :x x :y y
                      :width width :height height :stencil-dx stencil-dx :stencil-dy stencil-dy)
    (values x y (+ x width) (+ y height))))

(defmethod fill-image-color ((image image-mixin) red green blue (stencil (eql nil))
                             &key alpha x y width height stencil-dx stencil-dy)
  (declare (type fixnum x y width height)
           (ignore stencil stencil-dx stencil-dy))
  (let ((blend-span-fn (%image-blend-span-fn image))
        (image-type (image-type image)))
    (let ((max-x (+ x width -1))
          (max-y (+ y height -1)))
      (declare (type fixnum max-x max-y))
      (%call-blend-image-span-fn image-type blend-span-fn x y max-x max-y red green blue alpha))))

(defmethod fill-image-color ((image image-mixin) red green blue stencil
                             &key alpha x y width height stencil-dx stencil-dy)
  (declare (type fixnum x y width height))
  (let ((blend-fn (%image-blend-fn image))
        (image-type (image-type image))
        (stencil-fn (image-alpha-get-fn stencil :dx stencil-dx :dy stencil-dy)))
    (declare (type image-alpha-get-fn stencil-fn))
    (do-image image x y width height (i j)
      (let ((salpha (funcall stencil-fn i j)))
        (setf salpha (octet-mult salpha alpha))
        (%call-blend-image-fn image-type blend-fn 
                              i j red green blue salpha)))))

;;;
;;; fill
;;;
(defmethod fill-image :around ((image image-mixin) design stencil
                               &key (x 0) (y 0)
                                 (width (image-width image))
                                 (height (image-height image))
                                 (stencil-dx 0) (stencil-dy 0))
    (with-fill-image-bounds (image x y width height stencil stencil-dx stencil-dy)
      (call-next-method image design stencil :x x :y y
                        :width width :height height
                        :stencil-dx stencil-dx
                        :stencil-dy stencil-dy)
      (values x y (+ x width) (+ y height))))








