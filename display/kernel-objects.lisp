(in-package :cldk-internals)

;;;
;;; objects
;;;

(defclass kobject-mixin ()
  ())

(defclass driver-kobject-mixin (kobject-mixin)
  ((driver :initform nil
           :initarg :driver
           :reader driver)))
;;;
;;; windows
;;;

(defclass kwindow-mixin (driver-kobject-mixin)
  ((driver-window :initform nil
                  :initarg :driver-window
                  :reader window-driver-window)))

(defun initialize-kwindow (kwindow name pretty-name
                           x y width height mode)
  (with-slots (driver driver-window) kwindow
    (setf driver-window (driver-create-window driver name pretty-name
                                              x y width height mode))
    (register-server-object driver driver-window kwindow)))

(defgeneric refresh-kwindow (window &key max-fps)
  (:method ((kwindow t) &key max-fps)
    (declare (ignore max-fps))))

(defun refresh-kwindows (kernel)
  (with-slots (kwindows) kernel
    (dolist (win kwindows)
      (refresh-kwindow win))))

(defun kwindow-size (window)
  (driver-window-size (driver window) (window-driver-window  window)))

(defun kwindow-position (window)
  (driver-window-position (driver window) (window-driver-window  window)))

(defun set-kwindow-size (window width height)
  (driver-set-window-size (driver window) (window-driver-window  window) width height))

(defun set-kwindow-position (window x y)
  (driver-set-window-position (driver window) (window-driver-window  window) x y))

(defun destroy-kwindow (window)
  (driver-destroy-window (driver window) (window-driver-window  window))
  (unregister-server-object (driver window)
                            (window-driver-window window)))

(defun show-kwindow (window)
  (driver-show-window (driver window) (window-driver-window  window)))

(defun hide-kwindow (window)
  (driver-hide-window (driver window) (window-driver-window  window)))

(defun raise-kwindow (window)
  (driver-raise-window (driver window) (window-driver-window  window)))

(defun bury-kwindow (window)
  (driver-bury-window (driver window) (window-driver-window  window)))

(defun kwindow-pointer-position (window)
  (driver-window-pointer-position (driver window) (window-driver-window  window)))

(defun set-kwindow-hints (window x y width height
                          max-width max-height min-width min-height)
  (driver-set-window-hints (driver window) (window-driver-window  window)
                           x y width height
                           max-width max-height min-width min-height))

(defun grab-kwindow-pointer (window)
  (driver-grab-pointer (driver window) (window-driver-window  window) 0))

(defun ungrab-kwindow-pointer (window)
  (driver-ungrab-pointer (driver window) (window-driver-window  window) 0))

(defun set-kwindow-cursor (window cursor)
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

(defun initialize-buffered-kwindow (kwindow width height)
  (with-slots (dbuffer dbuffer-width dbuffer-height) kwindow
    (setf dbuffer (driver-create-buffer (driver kwindow) width height)
          dbuffer-width width
          dbuffer-height height)))

(defun destroy-buffered-kwindow (window)
  (driver-destroy-buffer (driver window) (window-buffer window)))
  
(defun copy-image-to-buffered-kwindow* (kwindow image rectangle-set)
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

(defun copy-image-to-buffered-kwindow (kwindow image x y width height)
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

(defun flush-buffered-kwindow (kwindow)
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

(defmethod refresh-kwindow ((window buffered-kwindow-mixin) &key (max-fps 100))
  
  (with-slots (last-refresh-time updated-region-set) window
    (if (or (null last-refresh-time)
            (> (- (get-internal-real-time) last-refresh-time)
               (* (/ 1 max-fps) internal-time-units-per-second)))
        (when (and (window-buffer window) updated-region-set)
          (flush-buffered-kwindow window))
        (progn
          (when (null last-refresh-time)
            (setf last-refresh-time (get-internal-real-time)))
          (when updated-region-set
            (log:info "skip"))))))
