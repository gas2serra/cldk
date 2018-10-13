(in-package :cldk-mcclim-render-internals)

(declaim (optimize speed))

(defgeneric aa-render-draw2 (image  pixeled-design clip-region
                            state
                                 x1
                                 y1
                                 x2
                                 y2
                            ))

(defgeneric aa-render-draw-fn (image clip-region pixeled-design))
(defgeneric aa-render-draw-span-fn (image clip-region pixeled-design))
(defgeneric aa-render-xor-draw-fn (image clip-region pixeled-design))
(defgeneric aa-render-xor-draw-span-fn (image clip-region pixeled-design))
(defgeneric aa-render-alpha-draw-fn (image clip-region))
(defgeneric aa-render-alpha-draw-span-fn (image clip-region))

(defgeneric aa-cells-sweep/rectangle (image design state clip-region))
(defgeneric aa-cells-alpha-sweep/rectangle (image design state clip-region))

(defmethod aa-cells-sweep/rectangle ((image rgb-image-mixin) (ink pixeled-design) state clip-region)
  (let ((draw-function nil)
        (draw-span-function nil)
        (current-clip-region
         (if (rectanglep clip-region)
             nil
             clip-region)))
    (clim:with-bounding-rectangle* (min-x min-y max-x max-y)
	clip-region
      (if (typep ink 'pixeled-flipping-design)
          nil
          (aa-render-draw3 image ink current-clip-region
                           state
                           (floor min-x)
                           (floor min-y)
                           (ceiling max-x)
                           (ceiling max-y))))))
#|                            
      (setf draw-function
            (if (typep ink 'pixeled-flipping-design)
                (aa-render-xor-draw-fn image current-clip-region ink)
                (aa-render-draw-fn image current-clip-region ink)))
      (setf draw-span-function
            (if (typep ink 'pixeled-flipping-design)
                (aa-render-xor-draw-span-fn image current-clip-region ink)
                (aa-render-draw-span-fn image current-clip-region ink)))
      (%aa-cells-sweep/rectangle state
                                (floor min-x)
                                (floor min-y)
                                (ceiling max-x)
                                (ceiling max-y)
                                draw-function
                                draw-span-function))))
|#
(defmethod aa-cells-alpha-sweep/rectangle ((image gray-image-mixin) ink state clip-region)
  (let ((draw-function nil)
        (draw-span-function nil)
        (current-clip-region
         (if (rectanglep clip-region)
             nil
             clip-region)))
    (clim:with-bounding-rectangle* (min-x min-y max-x max-y)
	clip-region
      (aa-render-alpha-draw3 image ink current-clip-region
                             state
                            (floor min-x)
                            (floor min-y)
                            (ceiling max-x)
                            (ceiling max-y))
                            
      #+nil (setf draw-function
            (aa-render-alpha-draw-fn image current-clip-region))
      #+nil(setf draw-span-function
            (aa-render-alpha-draw-span-fn image current-clip-region))
      #+nil(%aa-cells-sweep/rectangle state
                                (floor min-x)
                                (floor min-y)
                                (ceiling max-x)
                                (ceiling max-y)
                                draw-function
                                draw-span-function))))

(defun %aa-scanline-sweep (scanline function function-span &key start end)
  "Call FUNCTION for each pixel on the polygon covered by
SCANLINE. The pixels are scanned in increasing X. The sweep can
be limited to a range by START (included) or/and END (excluded)."
  (declare (optimize speed (debug 0) (safety 0) (space 2))
           (type (function (fixnum fixnum fixnum) *) function)
           (type (function (fixnum fixnum fixnum fixnum) *) function-span))
  (let ((x-min (aa::cell-x (car scanline)))
        (x-max (aa::cell-x (car scanline)))
        (cover 0)
        (y (aa::scanline-y scanline))
        (cells scanline)
        (last-x nil))
    (when start
      ;; skip initial cells that are before START
      (loop while (and cells (< (aa::cell-x (car cells)) start))
         do (incf cover (aa::cell-cover (car cells)))
         (setf last-x (aa::cell-x (car cells))
               cells (cdr cells))))
    (when cells
      (dolist (cell cells)
        (let ((x (aa::cell-x cell)))
          (when (and last-x (> x (1+ last-x)))
            (let ((alpha (aa::compute-alpha cover 0)))
              (unless (zerop alpha)
                (let ((start-x (if start (max start (1+ last-x)) (1+ last-x)))
                      (end-x (if end (min end x) x)))
                  (setf x-min (min x-min start-x))
                  (setf x-max (max x-max end-x))
                  (if function-span
                      (funcall function-span start-x end-x y alpha)
                      (loop for ix from start-x below end-x
                         do (funcall function ix y alpha)))))))
          (when (and end (>= x end))
            (return (values x-min x-max)))
          (incf cover (aa::cell-cover cell))
          (let ((alpha (aa::compute-alpha cover (aa::cell-area cell))))
            (unless (zerop alpha)
              (funcall function x y alpha)))
          (setf last-x x))))
    (values x-min x-max)))

(defun %aa-cells-sweep/rectangle (state x1 y1 x2 y2 function &optional function-span)
  "Call FUNCTION for each pixel on the polygon described by
previous call to LINE or LINE-F. The pixels are scanned in
increasing Y, then on increasing X. This is limited to the
rectangle region specified with (X1,Y1)-(X2,Y2) (where X2 must be
greater than X1 and Y2 must be greater than Y1, to describe a
non-empty region.)

For optimization purpose, the optional FUNCTION-SPAN, if
provided, is called for a full span of identical alpha pixel. If
not provided, a call is made to FUNCTION for each pixel in the
span."
  (let ((scanlines (aa::freeze-state state))
        (x-min x2)
        (x-max x1)
        (y-min y2)
        (y-max y1))
    #+nil (dolist (scanline scanlines)
      (setf y-min (min y-min (aa::scanline-y scanline)))
      (setf y-max (max y-max (aa::scanline-y scanline)))
      (when (<= y1 (aa::scanline-y scanline) (1- y2))
        (multiple-value-bind (xa xb)
            (%aa-scanline-sweep scanline function function-span :start x1 :end x2)
          (setf x-min (min x-min xa))
          (setf x-max (max x-max xb)))))
    (test-scanline state x1 y1 x2 y2 function function-span)
    #+nil (make-rectangle* x-min y-min (1+ x-max) (1+ y-max))))

(defclass scanline ()
  ((x1 :initarg :x1)
   (y1 :initarg :y1)
   (x2 :initarg :x2)
   (y2 :initarg :y2)
   (state :initarg :state)
   (scanlines :initarg :scanlines)))

(defmethod initialize-instance :after ((sl scanline)
                                       &key)
  (setf (slot-value sl 'scanlines)
        (aa::freeze-state (slot-value sl 'state))))

(defun %make-scanline (state x1 y1 x2 y2)
  (make-instance 'scanline :x1 x1 :y1 y1 :x2 x2 :y2 y2 :state state))
                 ;;:scanlines (aa::freeze-state state)))

(defun %make-scanline-fn (scanline)
  (declare (optimize speed (debug 0) (safety 0) (space 2)))
  (with-slots (x1 y1 x2 y2 scanlines) scanline
    (let ((cur-scanlines scanlines)
          (cur-cells nil)
          (cur-cell nil)
          (cover 0)
          (last-x nil)
          (rest nil)
          (status nil)
          (x nil)
          (y nil))
      (labels ((next-y ()
                 (setf status :y-move)
                 (setf cover 0
                       rest nil
                       x nil
                       last-x nil)
                 (do ()
                     ((or (eql status :stop)
                          (and (eql status :y-new)
                               (and (>= y y1) (<= y y2))))
                      y)
                   (if (null cur-scanlines)
                       (setf status :stop)
                       (progn
                         (setf status :y-new
                               cur-cells (car cur-scanlines)
                               cur-scanlines (cdr cur-scanlines)
                               y (aa::scanline-y cur-cells))))))
               (next-x ()
                 (cond (rest
                        (let ((next rest))
                          ;;(format *debug-io* "rest: ~A~%" rest)
                          (setf rest nil)
                          next))
                       ((null cur-cells)
                        :stop)
                       (t 
                        (progn
                          (setf cur-cell (car cur-cells))
                          (setf cur-cells (cdr cur-cells))
                          (setf x (aa::cell-x cur-cell))
                          (if (and last-x (> x (1+ last-x)))
                              (let ((alpha (aa::compute-alpha cover 0)))
                                (declare (type fixnum alpha))
                                (setf alpha (min (abs alpha) 255))
                                (let ((start-x (1+ last-x))
                                      (end-x x)
                                      (cur-alpha alpha))
                                  #+nil(format *debug-io* "last-x =>>> ~A ~A~%" (list start-x end-x)
                                          cur-alpha)
                                  (incf cover (aa::cell-cover cur-cell))
                                  (let ((alpha (aa::compute-alpha cover (aa::cell-area cur-cell))))
                                    (setf alpha (min (abs alpha) 255))
                                    (setf rest (list x x alpha)))
                                  (setf last-x x)
                                  (list start-x end-x cur-alpha)))
                              (progn
                                (incf cover (aa::cell-cover cur-cell))
                                (let ((alpha (aa::compute-alpha cover (aa::cell-area cur-cell))))
                                  (setf alpha (min (abs alpha) 255))
                                  ;;(format *debug-io* "first-x =>>> ~A ~A~%" x alpha)
                                  (setf last-x x)
                                  (list x x alpha))))))))
               (step-x ()
                 (let ((nx
                        (do ((x (next-x) (next-x)))
                            ((or (eql x :stop) (> (car (last x)) 0))
                             x)
                         nil)))
                   ;;(format *debug-io* "step x: ~A ~%" nx)
                   nx))
               (step-y ()
                 (let ((y (next-y)))
                   (if (eql status :stop)
                       :stop
                       y))))
        (declare (inline step-y step-x next-x next-y))
        (setf y (step-y))
        (if (eql y nil)
            (lambda ()
              (list :stop))
            (lambda ()
              (let ((res 
                     (do ((x (step-x) (step-x)))
                         ((or (eql y :stop) (not (eql x :stop)))
                          (progn
                            (cons y x)))
                       (setf y (step-y)))))
                ;;(format *debug-io* "res: ~A ~A~%" y res)
                res)))))))

            
(defun %make-scanline-fn3 (scanline)
  (with-slots (x1 y1 x2 y2 scanlines) scanline
    (let ((cur-scanlines scanlines)
          (cur-cells nil)
          (cur-cell nil)
          (cover 0)
          (last-x nil)
          (status :next-y)
          (alpha nil)
          (x nil)
          (y nil))
      (lambda ()
        (do ()
            ((or (eql status :val-last-x)
                 (eql status :val-x)
                 (eql status :stop))
             (case status
               (:val-last-x
                (setf status :x)
                ;;(format *debug-io* "=S> ~A~%" (list :ok (1+ last-x) x y alpha))
                (values :ok (1+ last-x) x y alpha))
               (:val-x
                (setf status :next-x)
                ;;(format *debug-io* "=V> ~A~%" (list :ok x x y alpha))
                (values :ok x x y alpha))
               (:stop
                (values :stop 0 0 0 0))))
          ;;(format *debug-io* "--> status: ~A~%" status)
          (case status
            (:next-y
             (if (null cur-scanlines)
                 (setf status :stop)
                 (progn 
                   (setf cur-cells (car cur-scanlines)
                         cur-scanlines (cdr cur-scanlines)
                         y (aa::scanline-y cur-cells)
                         cover 0
                         alpha 0
                         last-x nil)
                   (if (and (>= y y1) (<= y y2))
                       (setf status :next-x)
                       (setf status :next-y)))))
            (:next-x
             (if (null cur-cells)
                 (setf status :next-y)
                 (progn
                   (setf cur-cell (car cur-cells)
                         cur-cells (cdr cur-cells)
                         x (aa::cell-x cur-cell))
                   (if (and last-x (> x (1+ last-x)))
                       (setf status :last-x)
                       (setf status :x)))))
            (:last-x
             (setf alpha (aa::compute-alpha cover 0))
             (setf alpha (min (abs alpha) 255))
             (setf status :val-last-x))
            (:x
             (incf cover (aa::cell-cover cur-cell))
             (setf alpha (aa::compute-alpha cover (aa::cell-area cur-cell)))
             (setf alpha (min (abs alpha) 255))
             (setf last-x x)
             (setf status :val-x))))))))
    
     

(defun test-scanline (state x1 y1 x2 y2 function function-span)
  (let* ((sc (%make-scanline state x1 y1 x2 y2))
         (fn (%make-scanline-fn sc))
         (x-min x2)
         (x-max x1)
         (y-min y2)
         (y-max y1))
    (do ((val (funcall fn) (funcall fn)))
        ((eql (car val) :stop))
      (multiple-value-bind (y xa xb alpha)
          (values-list val)
        (when (and (>= y y1) (<= y y2)
                   (<= xa x2) (>= xb x1))
          ;;(format *debug-io* "==> ~A~%" val)
          (when (< xa x1) (setf xa x1))
          (when (> xb x2) (setf xb x2))
          (setf x-min (min x-min xa))
          (setf x-max (max x-max xb))
          (setf y-min (min y-min y))
          (setf y-max (max y-max y))
          (if (= xa xb)
              (funcall function xa y alpha)
              (if function-span
                  (funcall function-span xa xb y alpha)
                  (loop for ix from xa below xb
                     do (funcall function ix y alpha)))))))
    (make-rectangle* x-min y-min (1+ x-max) (1+ y-max))))


  
