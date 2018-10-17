(in-package :cldk-internals)

;;;
;;;
;;;

(defclass buffered-window (window)
  ((buffer-lock :initform (bt:make-lock "buffer"))
   (last-refresh-time :initform nil)))

(defgeneric create-buffered-window (display name &key pretty-name x y
                                                  width height mode
                                                   window-class))

(defmacro with-buffered-window-image ((buffer image) &rest body)
  `(with-slots (buffer-lock) ,buffer
     (bt:with-lock-held (buffer-lock)
       (let ((,image (buffered-window-image ,buffer)))
         ,@body))))



;;;
;;;
;;;
(defclass kerneled-buffered-window-mixin (kerneled-window-mixin)
  ((image :initform nil
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
    (setf (buffered-window-image win) (cldk-render:make-image win :rgb width height))
    win))

(defmethod initialize-buffered-window ((win kerneled-buffered-window-mixin) width height)
  )

(defmethod destroy-window :before ((window kerneled-buffered-window-mixin))
  )

;;;
;;; TO FIX
;;;

;;; refresh
(defgeneric k-refresh-window (window &key max-fps)
  (:method ((kwindow t) &key max-fps)
    (declare (ignore max-fps))))

(defun k-refresh-windows (kernel)
  (check-kernel-mode)
  (dolist (win (kernel-kwindows kernel))
    (k-refresh-window win)))

;;;
;;; buffered windows
;;;

(defmethod driver-cb-window-configuration-event :before ((handler driver-callback-handler)
                                                         kernel (win kerneled-buffered-window-mixin)
                                                         x y width height time)
  (check-kernel-mode)
  (k-notify-resize-buffered-window win width height))

(defun k-notify-resize-buffered-window (window width height)
  (k-flush-buffered-window window)
  (with-buffered-window-image (window image)
    (log:warn "Update ~A => ~A"
              (list (cldk-render:image-width image)
                    (cldk-render:image-height image))
              (list width height))
    (setf (buffered-window-image window) (cldk-render:make-image window :rgb width height))
    (cldki::update-image image width height)))

(defun k-flush-buffered-window (kwindow)
  (let ((image (buffered-window-image kwindow)))
    (when image
      (with-slots (updated-region-set) image
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
  
(defmethod k-refresh-window ((window kerneled-buffered-window-mixin) &key (max-fps 10))
  (check-kernel-mode)
  (with-buffered-window-image (window image)
    (with-slots (last-refresh-time) window
      (when image
        (with-slots (updated-region-set) image
          (if (or (null last-refresh-time)
                  (> (- (get-internal-real-time) last-refresh-time)
                     (* (/ 1 max-fps) internal-time-units-per-second)))
              (when updated-region-set
                (k-flush-buffered-window window))
              (progn
                (when (null last-refresh-time)
                  (setf last-refresh-time (get-internal-real-time)))
                (when updated-region-set
                  (log:info "skip")))))))))

