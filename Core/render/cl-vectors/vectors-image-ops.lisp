(in-package :cldk-render-internals)

;;;
;;; draw
;;;


(declaim (optimize speed))

(defmethod aa-render-draw-fn ((image image-mixin) clip-region pixeled-design)
  (let ((blend-fn (%image-blend-fn image))
        (image-type (image-type image))
        (design-fn (pixeled-design-rgba-get-fn pixeled-design)))
    (declare (type pixeled-design-fn design-fn))
    (lambda (x y alpha)
      (declare (type fixnum x y)
               (type fixnum alpha))
      (when (or (null clip-region)
                (clim:region-contains-position-p clip-region x y))
        (multiple-value-bind (r.fg g.fg b.fg a.fg)
            (funcall design-fn x y)
          (%call-blend-image-fn image-type blend-fn
                                x y r.fg g.fg b.fg (octet-mult a.fg alpha)))))))

(defmethod aa-render-draw-fn ((image image-mixin) clip-region
                              (pixeled-design pixeled-uniform-design))
  (let ((blend-fn (%image-blend-fn image))
        (image-type (image-type image)))
    (multiple-value-bind (r.fg g.fg b.fg a.fg)
        (values
         (pixeled-uniform-design-red pixeled-design)
         (pixeled-uniform-design-green pixeled-design)
         (pixeled-uniform-design-blue pixeled-design)
         (pixeled-uniform-design-alpha pixeled-design))
      (declare (type octet r.fg g.fg b.fg a.fg))
      (lambda (x y alpha)
        (declare (type fixnum x y)
                 (type fixnum alpha))
        (when (or (null clip-region)
                  (clim:region-contains-position-p clip-region x y))
          (%call-blend-image-fn image-type blend-fn
                                x y r.fg g.fg b.fg (octet-mult a.fg alpha)))))))

(defmethod aa-render-draw-span-fn ((image image-mixin) clip-region pixeled-design)
  (let ((blend-fn (%image-blend-fn image))
        (image-type (image-type image))
        (design-fn (pixeled-design-rgba-get-fn pixeled-design)))
    (declare (type pixeled-design-fn design-fn))
    (lambda (start-x end-x y alpha)
      (declare (type fixnum start-x end-x y)
               (type fixnum alpha))
      (loop for x from start-x below end-x do
           (when (or (null clip-region)
                     (clim:region-contains-position-p clip-region x y))
             (multiple-value-bind (r.fg g.fg b.fg a.fg)
                 (funcall design-fn x y)
               (%call-blend-image-fn image-type blend-fn
                                     x y r.fg g.fg b.fg (octet-mult a.fg alpha))))))))

(defmethod aa-render-draw-span-fn ((image image-mixin) clip-region
                                   (pixeled-design pixeled-uniform-design))
  (let ((blend-fn (%image-blend-fn image))
        (blend-span-fn (%image-blend-span-fn image))
        (image-type (image-type image)))
    (multiple-value-bind (r.fg g.fg b.fg a.fg)
        (values
         (pixeled-uniform-design-red pixeled-design)
         (pixeled-uniform-design-green pixeled-design)
         (pixeled-uniform-design-blue pixeled-design)
         (pixeled-uniform-design-alpha pixeled-design))
      (declare (type octet r.fg g.fg b.fg a.fg))
      (if (null clip-region)
          (lambda (start-x end-x y alpha)
            (declare (type fixnum start-x end-x y)
                     (type fixnum alpha))
            (%call-blend-image-span-fn image-type blend-span-fn
                                       start-x y end-x y r.fg g.fg b.fg (octet-mult a.fg alpha)))
          (lambda (start-x end-x y alpha)
            (declare (type fixnum start-x end-x y)
                     (type fixnum alpha))
            (loop for x from start-x below end-x do
                 (when (or (null clip-region)
                           (clim:region-contains-position-p clip-region x y))
                   (%call-blend-image-fn image-type blend-fn
                                         x y r.fg g.fg b.fg (octet-mult a.fg alpha)))))))))

;;;
;;;
;;;

(defmethod aa-render-xor-draw-fn ((image image-mixin) clip-region
                                  (pixeled-design pixeled-flipping-design))
  (let ((blend-fn (%image-xor-blend-fn image))
        (image-type (image-type image))
        (design-fn (pixeled-design-rgba-get-fn pixeled-design)))
    (declare (type pixeled-design-fn design-fn))
    (lambda (x y alpha)
      (declare (type fixnum x y)
               (type fixnum alpha))
      (when (or (null clip-region)
                (clim:region-contains-position-p clip-region x y))
        (multiple-value-bind (r.fg g.fg b.fg a.fg)
            (funcall design-fn x y)
          (%call-blend-image-fn image-type blend-fn
                                x y r.fg g.fg b.fg (octet-mult a.fg alpha)))))))

(defmethod aa-render-xor-draw-span-fn ((image image-mixin) clip-region
                                       (pixeled-design pixeled-flipping-design))
  (let ((blend-fn (%image-xor-blend-fn image))
        (image-type (image-type image))
        (design-fn (pixeled-design-rgba-get-fn pixeled-design)))
    (declare (type pixeled-design-fn design-fn))
    (lambda (start-x end-x y alpha)
      (declare (type fixnum start-x end-x y)
               (type fixnum alpha))
      (loop for x from start-x below end-x do
           (when (or (null clip-region)
                     (clim:region-contains-position-p clip-region x y))
             (multiple-value-bind (r.fg g.fg b.fg a.fg)
                 (funcall design-fn x y)
               (%call-blend-image-fn image-type blend-fn
                                     x y r.fg g.fg b.fg (octet-mult a.fg alpha))))))))
;;;
;;; alpha draw
;;;


(defmethod aa-render-alpha-draw-fn ((image gray-image-mixin) clip-region)
  (let ((set-fn (image-gray-set-fn image)))
    (declare (type image-gray-set-fn set-fn))
    (lambda (x y alpha)
      (declare (type fixnum x y)
               (type fixnum alpha))
      (when (or (null clip-region)
                (clim:region-contains-position-p clip-region x y))
        (funcall set-fn x y alpha)))))

(defmethod aa-render-alpha-draw-span-fn ((image gray-image-mixin) clip-region)
  (let ((set-fn (image-gray-set-fn image))
        (set-span-fn (image-gray-set-span-fn image)))
    (declare (type image-gray-set-fn set-fn))
    (lambda (x1 x2 y alpha)
      (declare (type fixnum x1 x2 y)
               (type fixnum alpha))
      (if (null clip-region)
          (funcall set-span-fn x1 y x2 y alpha)
          (loop for x from x1 below x2 do
               (clim:region-contains-position-p clip-region x y)
               (funcall set-fn x y alpha))))))
;;;
;;;
;;;

(defgeneric make-design-scanline-fn (design scanline-fn clip-region))

(defmethod make-design-scanline-fn (pixeled-design scanline-fn clip-region)
  (declare ;;(type (function () ) scanline-fn)
           (optimize speed (debug 0) (safety 0) (space 2)))
  (let ((design-fn (pixeled-design-rgba-get-fn pixeled-design))
        (start-x 0)
        (end-x 0)
        (y 0)
        (alpha 0))
    (declare (type pixeled-design-fn design-fn)
             (type fixnum start-x end-x y)
             (type octet alpha))
    (flet ((stepp ()
             (if (< start-x end-x)
                 (progn
                   ;;(format *debug-io* "==> ~A ~A~%" (list start-x end-x) alpha)
                   (setf start-x (1+ start-x))
                   (if (or (null clip-region)
                           (clim:region-contains-position-p clip-region start-x y))
                       (multiple-value-bind (r.fg g.fg b.fg a.fg)
                           (funcall design-fn start-x y)
                         (values :ok start-x start-x y r.fg g.fg b.fg (octet-mult a.fg alpha)))))
                 (multiple-value-bind (status nstart-x nend-x ny nalpha)
                     (funcall scanline-fn)
                   (if (eql status :stop)
                       (values :stop 0 0 0 0 0 0 0 0)
                       (progn
                         (setf y ny
                               start-x nstart-x
                               end-x nend-x
                               alpha nalpha)
                         ;;(format *debug-io* "===> ~A ~A ~A ~%" y (list start-x end-x) alpha)
                         (if (or (null clip-region)
                                 (clim:region-contains-position-p clip-region start-x y))
                             (multiple-value-bind (r.fg g.fg b.fg a.fg)
                                 (funcall design-fn start-x y)
                               (values :ok start-x start-x y r.fg g.fg b.fg (octet-mult a.fg alpha))))))))))
      (lambda ()
        (do ((val (stepp) (stepp)))
            (val val))))))

(defmethod make-design-scanline-fn-gray (pixeled-design scanline-fn clip-region)
  (declare ;;(type (function () ) scanline-fn)
           (optimize speed (debug 0) (safety 0) (space 2)))
  (let (
        (start-x 0)
        (end-x 0)
        (y 0)
        (alpha 0))
    (declare 
             (type fixnum start-x end-x y)
             (type octet alpha))
    (flet ((stepp ()
             (if (< start-x end-x)
                 (progn
                   ;;(format *debug-io* "==> ~A ~A~%" (list start-x end-x) alpha)
                   (setf start-x (1+ start-x))
                   (if (or (null clip-region)
                           (clim:region-contains-position-p clip-region start-x y))
                       (list start-x start-x y alpha)))
                 (multiple-value-bind (status nstart-x nend-x ny nalpha)
                     (funcall scanline-fn)
                   ;;(format *debug-io* "CALL: ~A~%" (list status nstart-x nend-x ny nalpha))
                   (if (eql status :stop)
                       (list :stop :stop)
                       (progn
                         (setf y ny
                               start-x nstart-x
                               end-x nend-x
                               alpha nalpha)
                         ;;(format *debug-io* "===> ~A ~A ~A ~%" y (list start-x end-x) alpha)
                         (if (or (null clip-region)
                                 (clim:region-contains-position-p clip-region start-x y))
                             (list start-x start-x y  alpha))))))))
      (lambda ()
        (do ((val (stepp) (stepp)))
            (val val))))))

(defmethod make-design-scanline-fn ((pixeled-design pixeled-uniform-design) scanline-fn clip-region)
  (declare ;;(type (function () ) scanline-fn)
           ;;(optimize speed (debug 0) (safety 0) (space 2)))
           )
  (let ((start-x 0)
        (end-x 0)
        (y 0)
        (alpha 0)
        (status :next))
    (declare (type fixnum start-x end-x y)
             (type octet alpha))
    (multiple-value-bind (red green blue a)
        (values
         (pixeled-uniform-design-red pixeled-design)
         (pixeled-uniform-design-green pixeled-design)
         (pixeled-uniform-design-blue pixeled-design)
         (pixeled-uniform-design-alpha pixeled-design))
      (declare (type octet red green blue a))
      #+nil (format *debug-io* "-------------------~%")
      (lambda ()
        (do ()
            ((or (eql status :val)
                 (eql status :vals)
                 (eql status :stop))
             (case status
               (:vals
                (setf status :next)
                (values :ok start-x end-x y red green blue (octet-mult a alpha)))
               (:val
                (let ((x start-x))
                  (setf start-x (1+ start-x))
                  (setf status :next-x)
                  (values :ok x x y red green blue (octet-mult a alpha))))
               (:stop
                (values :stop 0 0 0 0 0 0 0))))
          (case status
            (:next-x
             (if (< start-x end-x)
                 (if (clim:region-contains-position-p clip-region start-x y)
                     (setf status :val)
                     (progn
                       (setf start-x (1+ start-x))
                       (setf status :next-x)))
                 (setf status :next)))
            (:next
             (multiple-value-bind (astatus nstart-x nend-x ny nalpha)
                 (funcall scanline-fn)
               (if (eql astatus :stop)
                   (setf status :stop)
                   (progn
                     (setf y ny
                           start-x nstart-x
                           end-x nend-x
                           alpha nalpha)
                     (if (null clip-region)
                         (setf status :vals)
                         (setf status :next-x))))))))))))

(defmacro do-scanline3 (scanline-fn x1 y1 x2 y2 (start-x end-x y red green blue alpha) &body code)
  (let ((x-min (gensym "xn"))
        (x-max (gensym "xx"))
        (y-min (gensym "yn"))
        (y-max (gensym "yx"))
        (val (gensym))
        (stat (gensym)))
    `(let* ((,x-min ,x2)
            (,x-max ,x1)
            (,y-min ,y2)
            (,y-max ,y1)
            (,val :ok))
       (declare ;;(type (function () list) ,scanline-fn)
                (type fixnum ,x-min ,y-min ,x-max ,y-max))
       (do ()
           ((eql ,val :stop))
         (multiple-value-bind (,stat ,start-x ,end-x ,y ,red ,green ,blue ,alpha)
             (funcall ,scanline-fn)
           (declare (type fixnum ,start-x ,end-x ,y)
                    (type fixnum ,alpha))
           #+nil (format *debug-io* "==? ~A ~A~%" (list ,stat ,start-x ,end-x ,y ,red ,green ,blue ,alpha)
                   (list (type-of ,stat)
                         (eql ,stat :ok)))
           (if (eql ,stat :stop)
               (setf ,val :stop)
               (when (and (>= ,y ,y1) (<= ,y ,y2)
                          (<= ,start-x ,x2) (>= ,end-x ,x1))
                 (when (< ,start-x ,x1) (setf ,start-x ,x1))
                 (when (> ,end-x ,x2) (setf ,end-x ,x2))
                 (setf ,x-min (min ,x-min ,start-x))
                 (setf ,x-max (max ,x-max ,end-x))
                 (setf ,y-min (min ,y-min ,y))
                 (setf ,y-max (max ,y-max ,y))
                 ,@code))))
       (make-rectangle* ,x-min ,y-min (1+ ,x-max) (1+ ,y-max)))))

(defmacro do-scanline3.1 (scanline-fn x1 y1 x2 y2 (start-x end-x y alpha) &body code)
  (let ((x-min (gensym "xn"))
        (x-max (gensym "xx"))
        (y-min (gensym "yn"))
        (y-max (gensym "yx"))
        (val (gensym)))
    `(let* ((,x-min ,x2)
            (,x-max ,x1)
            (,y-min ,y2)
            (,y-max ,y1))
       (declare ;;(type (function () ) ,scanline-fn)
                (type fixnum ,x-min ,y-min ,x-max ,y-max))
       (do ((,val (funcall ,scanline-fn) (funcall ,scanline-fn)))
           ((eql (car ,val) :stop))
         ;;(format *debug-io* "==> ~A~%" ,val)
         (multiple-value-bind (,start-x ,end-x ,y ,alpha)
             (values-list ,val)
           (declare (type fixnum ,start-x ,end-x ,y)
                    (type fixnum ,alpha))
           (when (and (>= ,y ,y1) (<= ,y ,y2)
                      (<= ,start-x ,x2) (>= ,end-x ,x1))
             (when (< ,start-x ,x1) (setf ,start-x ,x1))
             (when (> ,end-x ,x2) (setf ,end-x ,x2))
             (setf ,x-min (min ,x-min ,start-x))
             (setf ,x-max (max ,x-max ,end-x))
             (setf ,y-min (min ,y-min ,y))
             (setf ,y-max (max ,y-max ,y))
             ,@code)))
       (make-rectangle* ,x-min ,y-min (1+ ,x-max) (1+ ,y-max)))))


(defmacro do-scanline (state x1 y1 x2 y2 (start-x end-x y alpha) &body code)
  (let ((x-min (gensym "xn"))
        (x-max (gensym "xx"))
        (y-min (gensym "yn"))
        (y-max (gensym "yx"))
        (sc (gensym))
        (fn (gensym))
        (val (gensym)))
    `(let* ((,sc (%make-scanline ,state ,x1 ,y1 ,x2 ,y2))
            (,fn (%make-scanline-fn2 ,sc))
            (,x-min ,x2)
            (,x-max ,x1)
            (,y-min ,y2)
            (,y-max ,y1))
       (declare (type (function () list) ,fn)
                (type fixnum ,x-min ,y-min ,x-max ,y-max))
       (do ((,val (funcall ,fn) (funcall ,fn)))
           ((eql (car ,val) :stop))
         (multiple-value-bind (,y ,start-x ,end-x ,alpha)
             (values-list ,val)
           (declare (type fixnum ,start-x ,end-x ,y)
                    (type fixnum ,alpha))
           (when (and (>= ,y ,y1) (<= ,y ,y2)
                      (<= ,start-x ,x2) (>= ,end-x ,x1))
             (when (< ,start-x ,x1) (setf ,start-x ,x1))
             (when (> ,end-x ,x2) (setf ,end-x ,x2))
             (setf ,x-min (min ,x-min ,start-x))
             (setf ,x-max (max ,x-max ,end-x))
             (setf ,y-min (min ,y-min ,y))
             (setf ,y-max (max ,y-max ,y))
             ,@code)))
       (make-rectangle* ,x-min ,y-min (1+ ,x-max) (1+ ,y-max)))))
     
(defmethod aa-render-alpha-draw ((image gray-image-mixin) clip-region
                                 state
                                 x1
                                 y1
                                 x2
                                 y2)
  (let ((set-fn (image-gray-set-fn image))
        (set-span-fn (image-gray-set-span-fn image)))
    (declare (type image-gray-set-fn set-fn))
    (do-scanline state x1 y1 x2 y2
        (start-x end-x y alpha)
      (if (and (null clip-region) (> end-x start-x))
          (funcall set-span-fn start-x y end-x y alpha)
          (loop for x from start-x below (1+ end-x) do
               (if (or (null clip-region)
                       (clim:region-contains-position-p clip-region x y))
                   (funcall set-fn x y alpha)))))))

(defmethod aa-render-draw2 ((image image-mixin) (pixeled-design pixeled-uniform-design)
                           clip-region
                           state
                           x1
                           y1
                           x2
                           y2)
  (let ((blend-fn (%image-blend-fn image))
        (blend-span-fn (%image-blend-span-fn image))
        (image-type (image-type image)))
    (multiple-value-bind (r.fg g.fg b.fg a.fg)
        (values
         (pixeled-uniform-design-red pixeled-design)
         (pixeled-uniform-design-green pixeled-design)
         (pixeled-uniform-design-blue pixeled-design)
         (pixeled-uniform-design-alpha pixeled-design))
      (declare (type octet r.fg g.fg b.fg a.fg))
      (do-scanline state x1 y1 x2 y2
          (start-x end-x y alpha)
        #+nil (declare (type fixnum start-x end-x y)
                 (type fixnum alpha))
        (if (and (null clip-region) (> end-x start-x))
            (%call-blend-image-span-fn image-type blend-span-fn
                                       start-x y end-x y r.fg g.fg b.fg (octet-mult a.fg alpha))
            (loop for x from start-x below (1+ end-x) do
                 (if (or (null clip-region)
                         (clim:region-contains-position-p clip-region x y))
                     (%call-blend-image-fn image-type blend-fn
                                           x y r.fg g.fg b.fg (octet-mult a.fg alpha)))))))))


(defmethod aa-render-draw2 ((image image-mixin) pixeled-design 
                           clip-region
                           state
                           x1
                           y1
                           x2
                           y2)
  (let ((blend-fn (%image-blend-fn image))
        (image-type (image-type image))
        (design-fn (pixeled-design-rgba-get-fn pixeled-design)))
    (declare (type pixeled-design-fn design-fn))
    (do-scanline state x1 y1 x2 y2
        (start-x end-x y alpha)
      #+nil (declare (type fixnum start-x end-x y)
                     (type fixnum alpha))
      (loop for x from start-x below (1+ end-x) do
           (if (or (null clip-region)
                   (clim:region-contains-position-p clip-region x y))
               (multiple-value-bind (r.fg g.fg b.fg a.fg)
                   (funcall design-fn x y)
                 (%call-blend-image-fn image-type blend-fn
                                       x y r.fg g.fg b.fg (octet-mult a.fg alpha))))))))


(defmethod aa-render-draw3 ((image image-mixin) pixeled-design 
                           clip-region
                           state
                           x1
                           y1
                           x2
                            y2)
  ;;(format *debug-io* "~A ~%" (list x1 y1 x2 y2))
  (let ((blend-fn (%image-blend-fn image))
        (blend-span-fn (%image-blend-span-fn image))
        (image-type (image-type image))
        (design-fn (make-design-scanline-fn pixeled-design
                                            (%make-scanline-fn3 (%make-scanline state x1 y1 x2 y2))
                                            clip-region)))
    ;;(declare (type pixeled-design-fn design-fn))
    (do-scanline3 design-fn x1 y1 x2 y2
        (start-x end-x y r.fg g.fg b.fg alpha)
      ;;(format *debug-io* "===> ~A ~%" (list start-x end-x y r.fg g.fg b.fg alpha))
      (if (= start-x end-x)
          (%call-blend-image-fn image-type blend-fn
                                start-x y r.fg g.fg b.fg alpha)
          (%call-blend-image-span-fn image-type blend-span-fn
                                     start-x y end-x y r.fg g.fg b.fg alpha)))))
          
(defmethod aa-render-alpha-draw3 ((image gray-image-mixin) pixeled-design 
                                  clip-region
                                  state
                                  x1
                                  y1
                                  x2
                                  y2)
  (let ((set-fn (image-gray-set-fn image))
        (set-span-fn (image-gray-set-span-fn image))
        (design-fn (make-design-scanline-fn-gray pixeled-design
                                            (%make-scanline-fn3 (%make-scanline state x1 y1 x2 y2))
                                            clip-region)))
    (declare (type image-gray-set-fn set-fn))
    (do-scanline3.1 design-fn x1 y1 x2 y2
        (start-x end-x y alpha)
      (if (= start-x end-x)
          (funcall set-fn start-x y alpha)
          (funcall set-span-fn start-x y end-x y alpha)))))
