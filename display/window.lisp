(in-package :cldk-internals)

(defclass window (server-object kwindow-mixin)
  ((cached-x :initform nil)
   (cached-y :initform nil)
   (cached-width :initform nil)
   (cached-height :initform nil)))

(defmacro <kwindow+ (window fn &rest args)
  `(<call+ (server ,window)
             ,fn ,window ,@args))

(defmacro <kwindow- (window fn &rest args)
  `(<call- (server ,window)
             ,fn ,window ,@args))

(defmethod initialize-instance  :after ((win window) &key server name pretty-name
                                                       x y width height mode
                                                       &allow-other-keys)
  (with-slots (kwindows) server
    (<kwindow+ win #'k-initialize-window name pretty-name
               x y width height mode)
    (push win kwindows)))

(defgeneric create-window (server name &key pretty-name x y width height
                                         mode window-class)
  (:method ((server display-server) name &key (pretty-name name) (x 0) (y 0)
                                   (width 300) (height 300)
                                   (mode :managed) (window-class 'window))
    (make-instance window-class :server server :kernel server
                   :name name :pretty-name pretty-name
                   :x x :y y :width width :height height :mode mode)))


(defgeneric destroy-window (window)
  (:method ((window window))
    (with-slots (kwindows) (server window)
      (setf kwindows (delete window kwindows))
      ;; sdl requires to wait 
      (<kwindow+ window #'k-destroy-window))))

(defgeneric window-size (window &key force-query-p)
  (:method ((window window) &key (force-query-p nil))
    (with-slots (cached-width cached-height) window
      (when (or force-query-p (null cached-width) (null cached-height))
        (let ((size (<kwindow+ window #'k-window-size)))
          (setf cached-width (first size)
                cached-height (second size))))
      (values cached-width cached-height))))

(defgeneric window-position (window &key force-query-p)
  (:method ((window window) &key (force-query-p nil))
    (with-slots (cached-x cached-y) window
      (when (or force-query-p (null cached-x) (null cached-y))
        (let ((pos (<kwindow+ window #'k-window-position)))
          (setf cached-x (first pos)
                cached-y (second pos))))
      (values cached-x cached-y))))

(defgeneric set-window-size (window width height &key block-p)
  (:method ((window window) width height &key (block-p nil))
    (with-slots (cached-width cached-height) window
      (setf cached-width nil
            cached-height nil))
    (if block-p
        (<kwindow+ window #'k-set-window-size width height)
        (<kwindow- window #'k-set-window-size width height))))

(defgeneric set-window-position (window x y &key block-p)
  (:method ((window window) x y &key (block-p nil))
    (with-slots (cached-x cached-y) window
      (setf cached-x nil
            cached-y nil))
    (if block-p
        (<kwindow+ window #'k-set-window-position x y)
        (<kwindow- window #'k-set-window-position x y))))

(defgeneric set-window-hints (window &key x y width height max-width max-height
                                       min-width min-height block-p)
  (:method ((window window) &key x y width height max-width max-height
                              min-width min-height (block-p nil))
    (if block-p
        (<kwindow+ window #'k-set-window-hints x y width height
                  max-width max-height min-width min-height)
        (<kwindow- window #'k-set-window-hints x y width height
                  max-width max-height min-width min-height))))

(defgeneric raise-window (window &key block-p)
  (:method ((window window) &key (block-p nil))
    (if block-p
        (<kwindow+ window #'k-raise-window)
        (<kwindow- window #'k-raise-window))))

(defgeneric bury-window (window &key block-p)
  (:method ((window window) &key (block-p nil))
    (if block-p
        (<kwindow+ window #'k-bury-window)
        (<kwindow- window #'k-bury-window))))

(defgeneric show-window (window &key block-p)
  (:method ((window window) &key (block-p nil))
    (if block-p
        (<kwindow+ window #'k-show-window)
        (<kwindow- window #'k-show-window))))

(defgeneric hide-window (window &key block-p)
  (:method ((window window) &key (block-p nil))
    (if block-p
        (<kwindow+ window #'k-hide-window)
        (<kwindow- window #'k-hide-window))))

;;; pointer

(defgeneric window-pointer-position (window)
  (:method ((window window))
    (<kwindow+ window #'k-window-pointer-position)))

(defgeneric grab-window-pointer (window pointer &key block-p)
  (:method ((window window) pointer &key (block-p t))
    (if block-p
        (<kwindow+ window #'k-grab-window-pointer)
        (<kwindow- window #'k-grab-window-pointer))))

(defgeneric ungrab-window-pointer (window pointer &key block-p)
  (:method ((window window) pointer &key (block-p t))
    (if block-p
      (<kwindow+ window #'k-ungrab-window-pointer)
      (<kwindow- window #'k-ungrab-window-pointer))))

(defgeneric set-window-cursor (window named-cursor &key block-p)
  (:method ((window window) named-cursor &key (block-p t))
    (let ((server (server window)))
      (let ((cursor (gethash (or named-cursor :default) (server-cursor-table server))))
        (unless cursor
          (setf cursor (<d+ (server window)
                            #'driver-create-cursor (or named-cursor :default)))
          (setf (gethash (or cursor :default) (server-cursor-table server)) cursor))
        (if block-p
          (<kwindow+ window #'k-set-window-cursor cursor)
          (<kwindow- window #'k-set-window-cursor cursor))))))

;;;
;;; caching protocol
;;;

(defmethod handler-configure-event :before ((handler event-handler) win x y w h time)
  (with-slots (cached-x cached-y cached-width cached-height) win
    (when x (setf cached-x x))
    (when y (setf cached-y y))
    (when w (setf cached-width w))
    (when h (setf cached-height h))))
