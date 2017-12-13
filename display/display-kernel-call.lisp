(in-package :cldk-internals)

(defun k-screen-num (kernel)
  (driver-screen-num (driver kernel)))

(defun k-screen-size (kernel screen-index units)
  (driver-screen-size (driver kernel) screen-index units))

(defun k-screen-dpi (kernel screen-index)
  (driver-screen-dpi (driver kernel) screen-index))

(defun k-screen-pointer-position (kernel)
  (driver-screen-pointer-position (driver kernel)))

(defun k-avaiable-cursor-names (kernel)
  (driver-avaiable-cursor-names (driver kernel)))

;;;
;;; windows
;;;

(defclass kwindow-mixin (kernel-object-mixin)
  ((driver-window :initform nil
                  :initarg :driver-window
                  :reader window-driver-window)))

(defun k-initialize-window (kwindow name pretty-name
                           x y width height mode)
  (with-slots (driver-window) kwindow
    (setf driver-window (driver-create-window (driver kwindow) name pretty-name
                                              x y width height mode))
    (register-server-object (kernel kwindow) driver-window kwindow)))

(defgeneric k-refresh-window (window &key max-fps)
  (:method ((kwindow t) &key max-fps)
    (declare (ignore max-fps))))

(defun k-refresh-windows (kernel)
  (with-slots (kwindows) kernel
    (dolist (win kwindows)
      (k-refresh-window win))))

(defun k-window-size (window)
  (driver-window-size (driver window) (window-driver-window  window)))

(defun k-window-position (window)
  (driver-window-position (driver window) (window-driver-window  window)))

(defun k-set-window-size (window width height)
  (driver-set-window-size (driver window) (window-driver-window  window) width height))

(defun k-set-window-position (window x y)
  (driver-set-window-position (driver window) (window-driver-window  window) x y))

(defun k-destroy-window (window)
  (driver-destroy-window (driver window) (window-driver-window  window))
  (unregister-server-object (kernel window)
                            (window-driver-window window)))

(defun k-show-window (window)
  (driver-show-window (driver window) (window-driver-window  window)))

(defun k-hide-window (window)
  (driver-hide-window (driver window) (window-driver-window  window)))

(defun k-raise-window (window)
  (driver-raise-window (driver window) (window-driver-window  window)))

(defun k-bury-window (window)
  (driver-bury-window (driver window) (window-driver-window  window)))

(defun k-window-pointer-position (window)
  (driver-window-pointer-position (driver window) (window-driver-window  window)))

(defun k-set-window-hints (window x y width height
                          max-width max-height min-width min-height)
  (driver-set-window-hints (driver window) (window-driver-window  window)
                           x y width height
                           max-width max-height min-width min-height))

(defun k-grab-window-pointer (window)
  (driver-grab-pointer (driver window) (window-driver-window  window) 0))

(defun k-ungrab-window-pointer (window)
  (driver-ungrab-pointer (driver window) (window-driver-window  window) 0))

(defun k-set-window-cursor (window cursor)
  (driver-set-window-cursor (driver window) (window-driver-window  window) cursor))

;;;
;;; buffered windows
;;;

(defclass buffered-kwindow-mixin (kwindow-mixin)
  ((dbuffer :initform nil
           :initarg :buffer
           :accessor window-buffer)
   (dbuffer-width :initform nil)
   (dbuffer-height :initform nil)
   (last-refresh-time :initform nil)
   (updated-region-set :initform nil)))

(defun k-initialize-buffered-window (kwindow width height)
  (with-slots (dbuffer dbuffer-width dbuffer-height) kwindow
    (setf dbuffer (driver-create-buffer (driver kwindow) width height)
          dbuffer-width width
          dbuffer-height height)))

(defun k-destroy-buffered-window (window)
  (driver-destroy-buffer (driver window) (window-buffer window)))
  
(defun k-copy-image-to-buffered-window* (kwindow image rectangle-set)
  (with-slots (dbuffer dbuffer-width dbuffer-height updated-region-set) kwindow
    (when dbuffer
      (map-over-rectangle-set-regions 
       #'(lambda (x1 y1 x2 y2)
           (driver-copy-image-to-buffer (driver kwindow) image
                                        (max x1 0)
                                        (max y1 0)
                                        (min (- x2 x1) dbuffer-width)
                                        (min (- y2 y1) dbuffer-height)
                                        dbuffer))
       rectangle-set))
    (setf updated-region-set (rectangle-set-union
                              updated-region-set
                              rectangle-set))))

(defun k-copy-image-to-buffered-window (kwindow image x y width height)
  (with-slots (dbuffer dbuffer-width dbuffer-height updated-region-set) kwindow
    (setf width (min width dbuffer-width)
          height (min height dbuffer-height)
          x (max x 0)
          y (max y 0))
    (driver-copy-image-to-buffer (driver kwindow) image x y width height dbuffer)
    (setf updated-region-set (rectangle-set-union
                              updated-region-set
                              (rectangle->rectangle-set x y
                                                        (+ x width) (+ y height))))))

(defun k-flush-buffered-window (kwindow)
  (with-slots (dbuffer updated-region-set) kwindow
    (map-over-rectangle-set-regions 
     #'(lambda (x1 y1 x2 y2)
         (driver-copy-buffer-to-window (driver kwindow)
                                       dbuffer
                                       x1 y1
                                       (- x2 x1)
                                       (- y2 y1)
                                       (window-driver-window kwindow)
                                       x1 y1))
     updated-region-set)
    (setf updated-region-set nil)))

(defmethod k-refresh-window ((window buffered-kwindow-mixin) &key (max-fps 100))
  (with-slots (last-refresh-time updated-region-set) window
    (if (or (null last-refresh-time)
            (> (- (get-internal-real-time) last-refresh-time)
               (* (/ 1 max-fps) internal-time-units-per-second)))
        (when (and (window-buffer window) updated-region-set)
          (k-flush-buffered-window window))
        (progn
          (when (null last-refresh-time)
            (setf last-refresh-time (get-internal-real-time)))
          (when updated-region-set
            (log:info "skip"))))))
