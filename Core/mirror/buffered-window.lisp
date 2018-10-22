(in-package :cldk-internals)

;;;
;;;
;;;

(defclass buffered-window (window)
  ((buffer-lock :initform (bt:make-lock "buffer"))
   (updated-region-set :initform nil)
   (last-refresh-time :initform nil)))

(defgeneric create-buffered-window (display name &key pretty-name x y
                                                  width height mode
                                                   window-class))

(defmacro with-buffered-window-locked ((buffered-window) &rest body)
  `(with-slots (buffer-lock) ,buffered-window
     (bt:with-lock-held (buffer-lock)
         ,@body)))

(defgeneric copy-image-to-buffered-window (image rectangle-set buffered-window dx dy))

(defmethod copy-image-to-buffered-window (image rectangle-set (buffered-window buffered-window)
                                          dx dy)
  (with-slots (updated-region-set) buffered-window
    (let ((dst-image (buffered-window-image buffered-window)))
      (let ((w (image-width dst-image))
            (h (image-height dst-image)))
        (if (and (>= w (- (image-width image) 2))
                 (>= h (- (image-height image) 2)))
            (progn
              (map-over-rectangle-set-regions
               #'(lambda (x1 y1 x2 y2)
                   (copy-image image x1 y1 (- x2 x1) (- y2 y1)
                               dst-image (+ dx x1) (+ dy y1))
                   (setf updated-region-set (rectangle-set-union
                                             updated-region-set
                                             (rectangle->rectangle-set (+ dx x1) (+ dy y1)
                                                                       (+ dx x2) (+ dy y2)))))
               rectangle-set)
              t)
            (progn
              (log:warn "skip copy image to buffered window. ~A => ~A"
                        (list (image-width image)
                              (image-height image))
                        (list w h))
              nil))))))



;;;
;;;
;;;
(defclass kerneled-buffered-window-mixin (kerneled-window-mixin)
  ((image2 :initform nil
          :initarg :image
          :accessor buffered-window-image)))

(defmethod initialize-instance :after ((win kerneled-buffered-window-mixin)
                                       &key width height &allow-other-keys)
  (initialize-buffered-window win width height))

(defmethod create-buffered-window ((server kerneled-display-mixin) name
                                   &key (pretty-name name) (x 0) (y 0)
                                     (width 300) (height 300)
                                     (mode :managed) (window-class 'buffered-window))
  (let ((win (make-instance window-class :driver server 
                            :name name :pretty-name pretty-name
                            :x x :y y
                            :width width :height height :mode mode)))
    (setf (buffered-window-image win) (create-image win :rgb width height))
    (log:warn "++ ~A" (buffered-window-image win))
    win))

(defmethod initialize-buffered-window ((win kerneled-buffered-window-mixin) width height)
  )

(defmethod destroy-window :before ((window kerneled-buffered-window-mixin))
  )

(defmethod refresh-window ((window kerneled-buffered-window-mixin) &key (max-fps 10))
  (check-kernel-mode)
  (with-buffered-window-locked (window)
    (let ((image (buffered-window-image window)))
      (with-slots (last-refresh-time updated-region-set) window
        (when image
          (if (or (null last-refresh-time)
                  (> (- (get-internal-real-time) last-refresh-time)
                     (* (/ 1 max-fps) internal-time-units-per-second)))
              (when updated-region-set
                (flush-buffered-window window))
              (progn
                (when (null last-refresh-time)
                  (setf last-refresh-time (get-internal-real-time)))
                (when updated-region-set
                  (log:info "skip")))))))))

(defun flush-buffered-window (kwindow)
  (let ((image (buffered-window-image kwindow)))
    (when image
      (log:warn "-- ~A" image)
      (with-slots (updated-region-set) kwindow
        (map-over-rectangle-set-regions 
         #'(lambda (x1 y1 x2 y2)
             (driver-copy-image-to-window image
                                          x1 y1
                                          (- x2 x1)
                                          (- y2 y1)
                                          kwindow
                                          x1 y1))
         updated-region-set)
        (setf updated-region-set nil)))))

;;;
;;; buffered windows
;;;

(defmethod driver-cb-window-configuration-event :before ((handler driver-callback-handler)
                                                         kernel (window kerneled-buffered-window-mixin)
                                                         x y width height time)
  (check-kernel-mode)
  (flush-buffered-window window)
  (with-buffered-window-locked (window)
    (setf (buffered-window-image window)
          (create-image window :rgb width height))
    (log:warn "+++ ~A" (buffered-window-image window))))


  

