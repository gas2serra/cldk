(in-package :cldk-internals)

(defclass kerneled-window-mixin (driver-object)
  ((cached-x :initform nil)
   (cached-y :initform nil)
   (cached-width :initform nil)
   (cached-height :initform nil)))

#|
(defmacro <kwindow+ (window fn &rest args)
  `(within-kernel-mode ((driver ,window) :block-p t)
                       (funcall ,fn ,window ,@args)))

(defmacro <kwindow- (window fn &rest args)
  `(within-kernel-mode ((driver ,window) :block-p nil)
                       (funcall ,fn ,window ,@args)))
|#

(defmethod initialize-instance  :after ((win kerneled-window-mixin) &key server name pretty-name
                                                       x y width height mode
                                                       &allow-other-keys)
  (within-kernel-mode ((driver win) :block-p t)
    (driver-initialize-window (driver win) win name pretty-name
                              x y width height mode)
    (with-slots (kwindows) (driver win)
      (push win kwindows))))

(defgeneric destroy-window (win)
  (:method ((win kerneled-window-mixin))
    (with-slots (kwindows) (driver win)
      (within-kernel-mode ((driver win) :block-p t)
        (driver-destroy-window (driver win) win))
      (setf kwindows (delete win kwindows)))))

(defgeneric window-size (window &key force-query-p)
  (:method ((window kerneled-window-mixin) &key (force-query-p nil))
    (with-slots (cached-width cached-height) window
      (when (or force-query-p (null cached-width) (null cached-height))
        (within-kernel-mode ((driver window) :block-p t)
          (multiple-value-bind (w h)
              (driver-window-size (driver window) window)
            (setf cached-width w
                  cached-height h))))
      (values cached-width cached-height))))

(defgeneric window-position (window &key force-query-p)
  (:method ((window kerneled-window-mixin) &key (force-query-p nil))
    (with-slots (cached-x cached-y) window
      (when (or force-query-p (null cached-x) (null cached-y))
        (within-kernel-mode ((driver window) :block-p t)
          (multiple-value-bind (x y)
              (driver-window-position (driver window) window)
            (setf cached-x x
                  cached-y y))))
      (values cached-x cached-y))))

(defgeneric set-window-size (window width height &key block-p)
  (:method ((window kerneled-window-mixin) width height &key (block-p nil))
    (with-slots (cached-width cached-height) window
      (setf cached-width nil
            cached-height nil))
    (within-kernel-mode ((driver window) :block-p block-p)  
      (driver-set-window-size (driver window) window width height))))

(defgeneric set-window-position (window x y &key block-p)
  (:method ((window kerneled-window-mixin) x y &key (block-p nil))
    (with-slots (cached-x cached-y) window
      (setf cached-x nil
            cached-y nil))
    (within-kernel-mode ((driver window) :block-p block-p)    
      (driver-set-window-position (driver window) window x y))))
  
(defgeneric set-window-hints (window &key x y width height max-width max-height
                                       min-width min-height block-p)
  (:method ((window kerneled-window-mixin) &key x y width height max-width max-height
                                             min-width min-height (block-p nil))
    (within-kernel-mode ((driver window) :block-p block-p)    
      (driver-set-window-hints (driver window) window x y width height
                               max-width max-height min-width min-height))))

(defgeneric raise-window (window &key block-p)
  (:method ((window kerneled-window-mixin) &key (block-p nil))
    (within-kernel-mode ((driver window) :block-p block-p)
      (driver-raise-window (driver window) window))))

(defgeneric bury-window (window &key block-p)
  (:method ((window kerneled-window-mixin) &key (block-p nil))
    (within-kernel-mode ((driver window) :block-p block-p)
      (driver-bury-window (driver window) window))))

(defgeneric show-window (window &key block-p)
  (:method ((window kerneled-window-mixin) &key (block-p nil))
    (within-kernel-mode ((driver window) :block-p block-p)
      (driver-show-window (driver window) window))))

(defgeneric hide-window (window &key block-p)
  (:method ((window kerneled-window-mixin) &key (block-p nil))
    (within-kernel-mode ((driver window) :block-p block-p)
      (driver-hide-window (driver window) window))))

;;; pointer

(defgeneric window-pointer-position (window)
  (:method ((window kerneled-window-mixin))
    (within-kernel-mode ((driver window) :block-p t)
      (driver-window-pointer-position (driver window) window))))

(defgeneric grab-window-pointer (window pointer &key block-p)
  (:method ((window kerneled-window-mixin) pointer &key (block-p t))
    (within-kernel-mode ((driver window) :block-p block-p)
      (driver-grab-pointer (driver window) window 0))))

(defgeneric ungrab-window-pointer (window pointer &key block-p)
  (:method ((window kerneled-window-mixin) pointer &key (block-p t))
    (within-kernel-mode ((driver window) :block-p block-p)
      (driver-ungrab-pointer (driver window) window 0))))

(defgeneric set-window-cursor (window named-cursor &key block-p)
  (:method ((window kerneled-window-mixin) cursor &key (block-p t))
    (within-kernel-mode ((driver window) :block-p block-p)
      (driver-set-window-cursor (driver window) window cursor))))

(defmethod set-window-cursor :around ((window kerneled-window-mixin) named-cursor &key (block-p t))
  (let ((driver (driver window)))
    (let ((cursor (gethash (or named-cursor :default) (server-cursor-table driver))))
      (unless cursor
        (setf cursor
              (within-kernel-mode ((driver window) :block-p t)
                (driver-create-cursor (driver window) (or named-cursor :default))))
        (setf (gethash (or cursor :default) (server-cursor-table driver)) cursor))
      (call-next-method window cursor :block-p block-p))))

;;;
;;; caching protocol
;;;

(defmethod handler-configure-event :before ((handler event-handler) win x y w h time)
  (with-slots (cached-x cached-y cached-width cached-height) win
    (when x (setf cached-x x))
    (when y (setf cached-y y))
    (when w (setf cached-width w))
    (when h (setf cached-height h))))


