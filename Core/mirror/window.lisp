(in-package :cldk-internals)

;;;
;;; window 
;;;
(defclass window ()
  ())

(defmethod initialize-instance  :after ((win window)
                                        &key driver name pretty-name
                                          x y width height mode
                                          &allow-other-keys)
  )
           
(defgeneric create-window (display name &key pretty-name x y width height
                                          mode window-class))
(defgeneric destroy-window (window))
(defgeneric window-size (window &key force-query-p))
(defgeneric window-position (window &key force-query-p))
(defgeneric set-window-size (window width height &key block-p))
(defgeneric set-window-position (window x y &key block-p))
(defgeneric set-window-hints (window &key x y width height max-width max-height
                                       min-width min-height block-p))
(defgeneric raise-window (window &key block-p))
(defgeneric bury-window (window &key block-p))
(defgeneric show-window (window &key block-p))
(defgeneric hide-window (window &key block-p))

(defgeneric window-pointer-position (window))
(defgeneric grab-window-pointer (window pointer &key block-p))
(defgeneric ungrab-window-pointer (window pointer &key block-p))
(defgeneric set-window-cursor (window named-cursor &key block-p))

(defgeneric copy-image-to-window (image x y w h window to-x to-y &key block-p))
(defgeneric refresh-window (window &key max-fps))


;;;
;;; kerneled window
;;;
(defclass kerneled-window-mixin (driver-object)
  ((cached-x :initform nil)
   (cached-y :initform nil)
   (cached-width :initform nil)
   (cached-height :initform nil)))

(defmethod initialize-instance  :after ((win kerneled-window-mixin)
                                        &key driver name pretty-name
                                          x y width height mode
                                          &allow-other-keys)
  (within-kernel-mode ((driver win) :block-p t)
    (driver-initialize-window (driver win) win name pretty-name
                              x y width height mode)
    (with-slots (kwindows) (driver win)
      (push win kwindows))))

(defmethod create-window ((display kerneled-display-mixin) name
                          &key (pretty-name name) (x nil) (y nil)
                            (width 300) (height 300)
                            (mode :managed) (window-class 'window))
  (make-instance window-class :driver display
                 :name name :pretty-name pretty-name
                 :x x :y y :width width :height height :mode mode))

(defmethod destroy-window ((win kerneled-window-mixin))
  (with-slots (kwindows) (driver win)
    (within-kernel-mode ((driver win) :block-p t)
      (driver-destroy-window win))
    (setf kwindows (delete win kwindows))))

(defmethod window-size ((window kerneled-window-mixin) &key (force-query-p nil))
  (with-slots (cached-width cached-height) window
    (when (or force-query-p (null cached-width) (null cached-height))
      (within-kernel-mode ((driver window) :block-p t)
        (multiple-value-bind (w h)
            (driver-window-size window)
          (setf cached-width w
                cached-height h))))
    (values cached-width cached-height)))

(defmethod window-position ((window kerneled-window-mixin) &key (force-query-p nil))
  (with-slots (cached-x cached-y) window
    (when (or force-query-p (null cached-x) (null cached-y))
      (within-kernel-mode ((driver window) :block-p t)
        (multiple-value-bind (x y)
            (driver-window-position window)
          (setf cached-x x
                cached-y y))))
    (values cached-x cached-y)))

(defmethod set-window-size ((window kerneled-window-mixin) width height &key (block-p nil))
  (with-slots (cached-width cached-height) window
    (setf cached-width nil
          cached-height nil))
  (within-kernel-mode ((driver window) :block-p block-p)  
    (driver-set-window-size window width height)))

(defmethod set-window-position ((window kerneled-window-mixin) x y &key (block-p nil))
  (with-slots (cached-x cached-y) window
    (setf cached-x nil
          cached-y nil))
  (within-kernel-mode ((driver window) :block-p block-p)    
    (driver-set-window-position window x y)))
  
(defmethod set-window-hints ((window kerneled-window-mixin) &key x y width height max-width max-height
                                                              min-width min-height (block-p nil))
  (within-kernel-mode ((driver window) :block-p block-p)    
    (driver-set-window-hints window x y width height
                             max-width max-height min-width min-height)))

(defmethod raise-window ((window kerneled-window-mixin) &key (block-p nil))
  (within-kernel-mode ((driver window) :block-p block-p)
    (driver-raise-window window)))

(defmethod bury-window ((window kerneled-window-mixin) &key (block-p nil))
  (within-kernel-mode ((driver window) :block-p block-p)
    (driver-bury-window window)))

(defmethod show-window ((window kerneled-window-mixin) &key (block-p nil))
  (within-kernel-mode ((driver window) :block-p block-p)
    (driver-show-window window)))

(defmethod hide-window ((window kerneled-window-mixin) &key (block-p nil))
  (within-kernel-mode ((driver window) :block-p block-p)
    (driver-hide-window window)))

;;; pointer

(defmethod window-pointer-position ((window kerneled-window-mixin))
  (within-kernel-mode ((driver window) :block-p t)
    (driver-window-pointer-position window)))

(defmethod grab-window-pointer ((window kerneled-window-mixin) pointer &key (block-p t))
  (within-kernel-mode ((driver window) :block-p block-p)
    (driver-grab-pointer (driver window) window 0)))

(defmethod ungrab-window-pointer ((window kerneled-window-mixin) pointer &key (block-p t))
  (within-kernel-mode ((driver window) :block-p block-p)
    (driver-ungrab-pointer (driver window) window 0)))

(defmethod set-window-cursor ((window kerneled-window-mixin) cursor &key (block-p t))
  (within-kernel-mode ((driver window) :block-p block-p)
    (driver-set-window-cursor window cursor)))

(defmethod set-window-cursor :around ((window kerneled-window-mixin) named-cursor &key (block-p t))
  (let ((driver (driver window)))
    (let ((cursor (gethash (or named-cursor :default)
                           (server-cursor-table driver))))
      (unless cursor
        (setf cursor
              (within-kernel-mode ((driver window) :block-p t)
                                  (driver-create-cursor (driver window)
                                                        (or named-cursor :default))))
        (setf (gethash (or cursor :default) (server-cursor-table driver))
              cursor))
      (call-next-method window cursor :block-p block-p))))

(defmethod copy-image-to-window (image x y w h (window kerneled-window-mixin) to-x to-y
                                 &key (block-p t))
  (within-kernel-mode ((driver window) :block-p block-p)
                      (driver-copy-image-to-window
                       image x y w h window to-x to-y)))

(defmethod refresh-window (window &key max-fps)
  (declare (ignore max-fps))
  nil)

;;;
;;; caching protocol
;;;

(defmethod handle-configure-event :before ((handler event-handler) win x y w h time)
  (with-slots (cached-x cached-y cached-width cached-height) win
    (when x (setf cached-x x))
    (when y (setf cached-y y))
    (when w (setf cached-width w))
    (when h (setf cached-height h))))


