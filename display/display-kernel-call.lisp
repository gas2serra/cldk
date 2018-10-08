(in-package :cldk-internals)

;;;
;;; windows
;;;

(defclass k-window-mixin (server-object)
  ())

;;; refresh
(defgeneric k-refresh-window (window &key max-fps)
  (:method ((kwindow t) &key max-fps)
    (declare (ignore max-fps))))

(defun k-refresh-windows (kernel)
  (check-kernel-mode)
  (dolist (win (kernel-kwindows kernel))
    (k-refresh-window win)))


;;; cursor

;;;
;;; buffer
;;;

(defclass k-buffer-mixin (buffer-image-mixin server-object)
  ())

;;;
;;; buffered windows
;;;

(defclass k-buffered-window-mixin (k-window-mixin)
  (
   (last-refresh-time :initform nil)))

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
               (driver-copy-buffer-to-window (server kwindow)
                                             obuffer
                                             x1 y1
                                             (- x2 x1)
                                             (- y2 y1)
                                             kwindow
                                             x1 y1))
           updated-region-set)
          (setf updated-region-set nil))))))
  
(defmethod k-refresh-window ((window k-buffered-window-mixin) &key (max-fps 10))
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

