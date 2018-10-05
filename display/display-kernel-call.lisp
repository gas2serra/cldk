(in-package :cldk-internals)


(defun k-screen-num (kernel)
  (check-kernel-mode)
  (driver-screen-num kernel))

(defun k-screen-size (kernel screen-index units)
  (check-kernel-mode)
  (driver-screen-size kernel screen-index units))

(defun k-screen-dpi (kernel screen-index)
  (check-kernel-mode)
  (driver-screen-dpi kernel screen-index))

(defun k-screen-pointer-position (kernel)
  (check-kernel-mode)
  (driver-screen-pointer-position kernel))

(defun k-avaiable-cursor-names (kernel)
  (check-kernel-mode)
  (driver-avaiable-cursor-names kernel))

;;;
;;; windows
;;;

(defclass k-window-mixin (server-object)
  ())

(defun window-driver-window (window)
  window)

(defun k-initialize-window (kwindow name pretty-name
                            x y width height mode)
  (check-kernel-mode)
  (driver-initialize-window (server kwindow) kwindow name pretty-name
                            x y width height mode)
  (register-server-object (server kwindow) kwindow))

(defun k-destroy-window (window)
  (check-kernel-mode)
  (driver-destroy-window (server window) (window-driver-window  window))
  (unregister-server-object (server window)
                            (window-driver-window window)))

(defun k-show-window (window)
  (check-kernel-mode)
  (driver-show-window (server window) (window-driver-window  window)))

(defun k-hide-window (window)
  (check-kernel-mode)
  (driver-hide-window (server window) (window-driver-window  window)))

(defun k-window-position (window)
  (check-kernel-mode)
  (driver-window-position (server window) (window-driver-window  window)))

(defun k-window-size (window)
  (check-kernel-mode)
  (driver-window-size (server window) (window-driver-window  window)))

(defun k-set-window-position (window x y)
  (check-kernel-mode)
  (driver-set-window-position (server window) (window-driver-window  window) x y))

(defun k-set-window-size (window width height)
  (check-kernel-mode)
  (driver-set-window-size (server window) (window-driver-window  window) width height))

(defun k-set-window-hints (window x y width height
                           max-width max-height min-width min-height)
  (check-kernel-mode)
  (driver-set-window-hints (server window) (window-driver-window  window)
                           x y width height
                           max-width max-height
                           min-width min-height))

(defun k-raise-window (window)
  (check-kernel-mode)
  (driver-raise-window (server window) (window-driver-window  window)))

(defun k-bury-window (window)
  (check-kernel-mode)
  (driver-bury-window (server window) (window-driver-window  window)))

(defun k-window-pointer-position (window)
  (check-kernel-mode)
  (driver-window-pointer-position (server window) (window-driver-window  window)))

(defun k-grab-window-pointer (window)
  (check-kernel-mode)
  (driver-grab-pointer (server window) (window-driver-window  window) 0))

(defun k-ungrab-window-pointer (window)
  (check-kernel-mode)
  (driver-ungrab-pointer (server window) (window-driver-window  window) 0))

;;; refresh
(defgeneric k-refresh-window (window &key max-fps)
  (:method ((kwindow t) &key max-fps)
    (declare (ignore max-fps))))

(defun k-refresh-windows (kernel)
  (check-kernel-mode)
  (with-slots (kwindows) kernel
    (dolist (win kwindows)
      (k-refresh-window win))))


;;; cursor

(defun k-set-window-cursor (window cursor)
  (check-kernel-mode)
  (driver-set-window-cursor (server window) (window-driver-window  window) cursor))

;;;
;;; buffer
;;;

(defclass k-buffer-mixin (buffer-image-mixin server-object)
  ())

(defun buffer-driver-buffer (buffer)
  buffer)

(defun k-initialize-buffer (kbuffer width height)
  (check-kernel-mode)
  (driver-initialize-buffer (server kbuffer) kbuffer width height)
  (register-server-object (server kbuffer) kbuffer))

(defun k-destroy-buffer (kbuffer)
  (check-kernel-mode)
  (driver-destroy-buffer (server kbuffer) (buffer-driver-buffer kbuffer))
  (unregister-server-object (server kbuffer)
                            (buffer-driver-buffer kbuffer)))

(defun k-update-buffer (kbuffer width height)
  (check-kernel-mode)
  (when (buffer-driver-buffer kbuffer)
    (driver-update-buffer (server kbuffer) (buffer-driver-buffer kbuffer) width height)))

;;;
;;; buffered windows
;;;

(defclass k-buffered-window-mixin (k-window-mixin)
  ((obuffer :initform nil
            :initarg :obuffer
            :accessor window-obuffer)
   (last-refresh-time :initform nil)))

(defun k-initialize-buffered-window (kwindow width height)
  (check-kernel-mode)
  #+nil (with-slots (obuffer) kwindow
    (k-initialize-buffer obuffer width height)))

(defun k-destroy-buffered-window (window)
  (check-kernel-mode)
  (with-slots (obuffer) window
    (k-destroy-buffer obuffer)))

(defun k-notify-resize-buffered-window (window width height)
  (with-slots (obuffer) window
    (when obuffer
      (k-flush-buffered-window window)
      (with-slots (pixels-lock) obuffer
        (bt:with-lock-held (pixels-lock)
          (k-update-buffer obuffer width height))))))

(defun k-flush-buffered-window (kwindow)
  (check-kernel-mode)
  (with-slots (obuffer) kwindow
    (when obuffer
      (with-slots (updated-region-set pixels-lock) (window-obuffer kwindow)
        (bt:with-lock-held (pixels-lock)
          (map-over-rectangle-set-regions 
           #'(lambda (x1 y1 x2 y2)
               (driver-copy-buffer-to-window (server kwindow)
                                             (buffer-driver-buffer obuffer)
                                             x1 y1
                                             (- x2 x1)
                                             (- y2 y1)
                                             (window-driver-window kwindow)
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

