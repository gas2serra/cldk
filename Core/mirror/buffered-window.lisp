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

;;;
;;;
;;;
(defclass kerneled-buffered-window-mixin (kerneled-window-mixin)
  ((obuffer :initform nil
            :initarg :obuffer
            :accessor window-obuffer)))

(defmethod initialize-instance :after ((win kerneled-buffered-window-mixin)
                                       &key width height &allow-other-keys)
  (initialize-buffered-window win width height))

(defmethod create-buffered-window ((server kerneled-display-mixin) name
                                   &key (pretty-name name) (x 0) (y 0)
                                     (width 300) (height 300)
                                     (mode :managed) (window-class 'buffered-window))
  (let ((buffer (create-buffer server width height)))
    (make-instance window-class :driver server 
                   :obuffer buffer
                   :name name :pretty-name pretty-name
                   :x x :y y
                   :width width :height height :mode mode)))

(defmethod initialize-buffered-window ((win kerneled-buffered-window-mixin) width height)
  (within-kernel-mode ((driver win) :block-p t)
    (driver-initialize-buffer (driver win) (window-obuffer win) width height)))

(defmethod destroy-window :before ((window kerneled-buffered-window-mixin))
  (destroy-buffer (window-obuffer window)))

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
  (let ((obuffer (window-obuffer window)))
    (when obuffer
      (k-flush-buffered-window window)
      (with-slots (pixels-lock) obuffer
        (bt:with-lock-held (pixels-lock)
          (update-buffer obuffer width height))))))

(defun k-flush-buffered-window (kwindow)
  (check-kernel-mode)
  (let ((obuffer (window-obuffer kwindow)))
    (when obuffer
      (with-slots (updated-region-set pixels-lock) (window-obuffer kwindow)
        (bt:with-lock-held (pixels-lock)
          (map-over-rectangle-set-regions 
           #'(lambda (x1 y1 x2 y2)
               (driver-copy-buffer-to-window obuffer
                                             x1 y1
                                             (- x2 x1)
                                             (- y2 y1)
                                             kwindow
                                             x1 y1))
           updated-region-set)
          (setf updated-region-set nil))))))
  
(defmethod k-refresh-window ((window kerneled-buffered-window-mixin) &key (max-fps 10))
  (check-kernel-mode)
  (with-slots (last-refresh-time) window
    (when (window-obuffer window)
      (with-slots (updated-region-set) (window-obuffer window)
        (if (or (null last-refresh-time)
                (> (- (get-internal-real-time) last-refresh-time)
                   (* (/ 1 max-fps) internal-time-units-per-second)))
            (when (and (window-obuffer window) updated-region-set)
              (k-flush-buffered-window window))
            (progn
              (when (null last-refresh-time)
                (setf last-refresh-time (get-internal-real-time)))
              (when updated-region-set
                (log:info "skip"))))))))

