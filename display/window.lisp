(in-package :cldk-internals)

(defclass window (kwindow-mixin)
  ((server :initform nil
           :initarg :server
           :reader window-server)
   (cached-x :initform nil)
   (cached-y :initform nil)
   (cached-width :initform nil)
   (cached-height :initform nil)))

(defmacro <kwindow+ (window fn &rest args)
  `(<c+ (window-server ,window)
             ,fn ,window ,@args))

(defmacro <kwindow- (window fn &rest args)
  `(<c- (window-server ,window)
             ,fn ,window ,@args))

(defmethod initialize-instance  :after ((win window) &key server name pretty-name
                                                       x y width height mode
                                                       &allow-other-keys)
  (with-slots (kwindows) server
    (<kwindow+ win #'initialize-kwindow name pretty-name
               x y width height mode)
    (push win kwindows)))

(defgeneric create-window (server name &key pretty-name x y width height
                                         mode window-class)
  (:method ((server display-server) name &key (pretty-name name) (x 0) (y 0)
                                   (width 300) (height 300)
                                   (mode :managed) (window-class 'window))
    (make-instance window-class :server server :driver (server-driver server)
                   :name name :pretty-name pretty-name
                   :x x :y y :width width :height height :mode mode)))


(defgeneric destroy-window (window)
  (:method ((window window))
    (with-slots (kwindows) (window-server window)
      (setf kwindows (delete window kwindows))
      ;; sdl requires to wait 
      (<kwindow+ window #'destroy-kwindow))))

(defgeneric window-size (window &key force-query-p)
  (:method ((window window) &key (force-query-p nil))
    (with-slots (cached-width cached-height) window
      (when (or force-query-p (null cached-width) (null cached-height))
        (let ((size (<kwindow+ window #'kwindow-size)))
          (setf cached-width (first size)
                cached-height (second size))))
      (values cached-width cached-height))))

(defgeneric window-position (window &key force-query-p)
  (:method ((window window) &key (force-query-p nil))
    (with-slots (cached-x cached-y) window
      (when (or force-query-p (null cached-x) (null cached-y))
        (let ((pos (<kwindow+ window #'kwindow-position)))
          (setf cached-x (first pos)
                cached-y (second pos))))
      (values cached-x cached-y))))

(defgeneric set-window-size (window width height &key block-p)
  (:method ((window window) width height &key (block-p nil))
    (with-slots (cached-width cached-height) window
      (setf cached-width nil
            cached-height nil))
    (if block-p
        (<kwindow+ window #'set-kwindow-size width height)
        (<kwindow- window #'set-kwindow-size width height))))

(defgeneric set-window-position (window x y &key block-p)
  (:method ((window window) x y &key (block-p nil))
    (with-slots (cached-x cached-y) window
      (setf cached-x nil
            cached-y nil))
    (if block-p
        (<kwindow+ window #'set-kwindow-position x y)
        (<kwindow- window #'set-kwindow-position x y))))

(defgeneric set-window-hints (window &key x y width height max-width max-height
                                       min-width min-height block-p)
  (:method ((window window) &key x y width height max-width max-height
                              min-width min-height (block-p nil))
    (if block-p
        (<kwindow+ window #'set-kwindow-hints x y width height
                  max-width max-height min-width min-height)
        (<kwindow- window #'set-kwindow-hints x y width height
                  max-width max-height min-width min-height))))

(defgeneric raise-window (window &key block-p)
  (:method ((window window) &key (block-p nil))
    (if block-p
        (<kwindow+ window #'raise-kwindow)
        (<kwindow- window #'raise-kwindow))))

(defgeneric bury-window (window &key block-p)
  (:method ((window window) &key (block-p nil))
    (if block-p
        (<kwindow+ window #'bury-kwindow)
        (<kwindow- window #'bury-kwindow))))

(defgeneric show-window (window &key block-p)
  (:method ((window window) &key (block-p nil))
    (if block-p
        (<kwindow+ window #'show-kwindow)
        (<kwindow- window #'show-kwindow))))

(defgeneric hide-window (window &key block-p)
  (:method ((window window) &key (block-p nil))
    (if block-p
        (<kwindow+ window #'hide-kwindow)
        (<kwindow- window #'hide-kwindow))))

;;; pointer

(defgeneric window-pointer-position (window)
  (:method ((window window))
    (<kwindow+ window #'kwindow-pointer-position)))

(defgeneric grab-window-pointer (window pointer &key block-p)
  (:method ((window window) pointer &key (block-p t))
    (if block-p
        (<kwindow+ window #'grab-kwindow-pointer)
        (<kwindow- window #'grab-kwindow-pointer))))

(defgeneric ungrab-window-pointer (window pointer &key block-p)
  (:method ((window window) pointer &key (block-p t))
    (if block-p
      (<kwindow+ window #'ungrab-kwindow-pointer)
      (<kwindow- window #'ungrab-kwindow-pointer))))

(defgeneric set-window-cursor (window named-cursor &key block-p)
  (:method ((window window) named-cursor &key (block-p t))
    (let ((server (window-server window)))
      (let ((cursor (gethash (or named-cursor :default) (server-cursor-table server))))
        (unless cursor
          (setf cursor (<d+ (window-server window)
                            #'driver-create-cursor (or named-cursor :default)))
          (setf (gethash (or cursor :default) (server-cursor-table server)) cursor))
        (if block-p
          (<kwindow+ window #'set-kwindow-cursor cursor)
          (<kwindow- window #'set-kwindow-cursor cursor))))))

;;;
;;; caching protocol
;;;

(defmethod handler-configure-event :before ((handler event-handler) win x y w h time)
  (with-slots (cached-x cached-y cached-width cached-height) win
    (when x (setf cached-x x))
    (when y (setf cached-y y))
    (when w (setf cached-width w))
    (when h (setf cached-height h))))
