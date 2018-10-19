(in-package :cldk-driver-clx)

(defclass clx-driver (display-driver)
  ((display :initform nil :reader clx-driver-display)
   (screen :initform nil)
   (root-window :initform nil)))

(defun clx-error-handler (display error-name
			  &rest args
			  &key major asynchronous &allow-other-keys)
  (warn "Received CLX ~A (~A) in process ~W for display ~W."
        error-name major (bt:thread-name (bt:current-thread)) display)
  ;; We ignore all asynchronous errors to keep the connection.
  ;; 42 is SetInputFocus, we ignore match-errors from that.
  (unless (or asynchronous
              (and (eql major 42)
                   (eq error-name 'xlib:match-error)))
    (apply #'xlib:default-error-handler display error-name args)))

(defmethod driver-start ((driver clx-driver))
  (with-slots (display screen root-window) driver
    (let ((options (driver-options driver)))
      (setf (driver-default-screen-index driver) (getf options :screen-id 0))
      (setf display (xlib:open-display (getf options :host) 
                                       :display (getf options :display-id 0) 
                                       :protocol (getf options :protocol 0)))
      (setf screen (nth (getf options :screen-id 0)
                        (xlib:display-roots display)))
      (setf root-window (xlib:screen-root screen))
      (setf (xlib:display-error-handler display) #'clx-error-handler)
      (load-mapping display))))

(defmethod driver-stop ((driver clx-driver))
  (with-slots (display screen root-window) driver
    (when display
      (handler-case
          (xlib:close-display display)
        (stream-error ()
          (xlib:close-display display :abort t)))
      (setf display nil
            screen nil
            root-window nil))))

(defmethod driver-kill ((driver clx-driver))
  (with-slots (display) driver
    (xlib:close-display display :abort t)
    (setf display nil)))

(defmethod driver-ping ((driver clx-driver))
  (with-slots (display) driver
    (and display (xlib:display-xid display))))

(defmethod driver-force-output ((driver clx-driver))
  (with-slots (display) driver
    (xlib:display-force-output display)))

;;; events

(defvar *clx-driver*)
(defvar *clx-kernel*)
(defvar *clx-error*)

(defmethod driver-process-next-event ((driver clx-driver))
  (with-slots (display) driver
    (if (not (xlib:event-listen display))
        nil
        (let ((*clx-driver* driver)
              (*clx-kernel* driver)
              (*clx-error* nil))
          (xlib:process-event display :timeout 0
                              :handler #'clx-event-handler :discard-p t)
          (when *clx-error*
            (error *clx-error*))
          t))))

;;; root
(defclass clx-driver-root (driver-root)
  ((xroot)
   (xscreen)))

(defmethod driver-object-id ((root clx-driver-root))
  (with-slots (xroot) root
    xroot))

(defmethod driver-initialize-root ((driver clx-driver) (root clx-driver-root))
  (with-slots (root-window screen) driver
    (with-slots (xroot xscreen) root
      (setf xroot root-window)
      (setf xscreen screen))))

(defmethod driver-destroy-root ((root clx-driver-root))
  (with-slots (xroot) root
    (setf xroot nil
          xscreen nil)))

(defmethod driver-root-size ((root clx-driver-root) units)
  (with-slots (xscreen) root
    (ecase units
      (:device (values (xlib:screen-width xscreen)
                       (xlib:screen-height xscreen)))
      (:inches (values (/ (xlib:screen-width-in-millimeters xscreen) 25.4s0)
                       (/ (xlib:screen-height-in-millimeters xscreen) 25.4s0)))
      (:millimeters (values (xlib:screen-width-in-millimeters xscreen)
                            (xlib:screen-height-in-millimeters xscreen))))))

(defmethod driver-root-pointer-position ((root clx-driver-root))
  (with-slots (xroot) root
    (multiple-value-bind (x y #|same-screen-p|#)
        (xlib:query-pointer xroot)
      (values x y))))

;;; window

(defclass clx-driver-window (driver-window)
  ((xwindow :initarg :xwindow)
   (gcontext :initarg :gcontext)))

(defmethod driver-object-id ((window clx-driver-window))
  (with-slots (xwindow) window
    xwindow))

(defparameter *event-mask* '(:exposure 
			     :key-press :key-release
			     :button-press :button-release
			     :owner-grab-button
			     :enter-window :leave-window
			     :structure-notify
			     :pointer-motion :button-motion))

(defmethod driver-initialize-window ((driver clx-driver) win name pretty-name x y
                                 width height mode)
  (let ((background '(0.5 0.5 0.5)))
    (with-slots (screen) driver
      (let* ((color (multiple-value-bind (r g b)
                        (values-list background)
                      (xlib:make-color :red r :green g :blue b)))
             (pixel (xlib:alloc-color (xlib:screen-default-colormap screen) color)))
        (let ((window (xlib:create-window
                       :parent (xlib:screen-root screen)
                       :width (max width 1)
                       :height (max height 1)
                       :x (or x 0)
                       :y (or y 0)
                       :border-width 0
                       :border 0
                       :override-redirect (if (eql mode :managed) :off :on)
                       :backing-store :not-useful
                       :save-under :off
                       :gravity :north-west
                       :bit-gravity :forget
                       :background pixel
                       :event-mask (apply #'xlib:make-event-mask
                                          *event-mask*))))
          (when (eql mode :managed)
            (setf (xlib:wm-hints window) (xlib:make-wm-hints :input :on))
            (setf (xlib:wm-name window) pretty-name)
            (setf (xlib:wm-icon-name window) pretty-name)
            (xlib:set-wm-class
             window
             (string-downcase name)
             (string-capitalize (string-downcase name)))
            (setf (xlib:wm-protocols window) `(:wm_delete_window))
            (xlib:change-property window
                                  :WM_CLIENT_LEADER (list (xlib:window-id window))
                                  :WINDOW 32))
          (with-slots (xwindow gcontext) win
            (setf xwindow window
                  gcontext
                  (xlib:create-gcontext :drawable window
                                        :background (values 0 0 0)
                                        :foreground (values 255 255 255)))))))))

(defmethod driver-destroy-window ((window clx-driver-window))
  (with-slots (xwindow gcontext) window
    (when xwindow 
      (xlib:destroy-window xwindow)
      (xlib:free-gcontext gcontext))
    (setf window nil
          gcontext nil)))

(defmethod driver-show-window ((window clx-driver-window))
  (with-slots (xwindow) window
    (xlib:map-window xwindow)
    (with-slots (display) (driver window)
      (xlib:display-force-output display))))

(defmethod driver-hide-window ((window clx-driver-window))
  (with-slots (xwindow) window
    (xlib:unmap-window xwindow)
    (with-slots (display) (driver window)
      (xlib:display-force-output display))))

(defmethod driver-window-position ((window clx-driver-window))
  (with-slots (xwindow) window
    (xlib:translate-coordinates xwindow 0 0 (slot-value driver 'root-window))))


(defmethod driver-window-size ((window clx-driver-window))
  (with-slots (xwindow) window
    (values (xlib:drawable-width xwindow)
            (xlib:drawable-height xwindow))))

(defmethod driver-set-window-position ((window clx-driver-window) x y)
  (with-slots (xwindow) window
    (setf (xlib:drawable-x xwindow) x
          (xlib:drawable-y xwindow) y)
    (with-slots (display) (driver window)
      (xlib:display-force-output display))))

(defmethod driver-set-window-size ((window clx-driver-window) width height)
  (with-slots (xwindow) window
    (setf (xlib:drawable-width xwindow) width
          (xlib:drawable-height xwindow) height)
    (with-slots (display) (driver window)
      (xlib:display-force-output display))))

(defmethod driver-set-window-hints ((window clx-driver-window) x y width height max-width max-height
                                    min-width min-height)
  (with-slots (xwindow) window
    (setf (xlib:wm-normal-hints xwindow)
          (xlib:make-wm-size-hints
           :user-specified-position-p (and x y)
           :x x :y y
           :width  width
           :height height
           :max-width max-width
           :max-height max-height
           :min-width (and min-width (max min-width 1))
           :min-height (and min-height (max min-height 1))))))

(defmethod driver-bury-window ((window clx-driver-window))
  (with-slots (xwindow) window
    (xlib:circulate-window-down xwindow)
    (with-slots (display) (driver window)
      (xlib:display-force-output display))))

(defmethod driver-raise-window ((window clx-driver-window))
  (with-slots (xwindow) window
    (xlib:circulate-window-up xwindow)
    (with-slots (display) (driver window)
      (xlib:display-force-output display))))

(defmethod driver-window-pointer-position ((window clx-driver-window))
  (with-slots (xwindow) window
    (multiple-value-bind (x y #|same-screen-p|#)
        (xlib:query-pointer xwindow)
      (values x y))))

(defmethod driver-copy-image-to-window (image x y width height
                                        (window clx-driver-window) to-x to-y)
  (let ((ximage (cldk-render-internals::clx-image->xlib-image image)))
    (with-slots (xwindow gcontext) window
      (when (and ximage
                 (>= x 0) (>= y 0) (> width 0) (> height 0) (>= to-x 0) (>= to-y 0))
        (xlib::put-image xwindow
                         gcontext
                         ximage
                         :src-x x :src-y y
                         :x to-x :y to-y
                         :width  (max 0 (min width
                                             (- (xlib:image-width ximage) x)))
                         :height (max 0 (min height
                                             (- (xlib:image-height ximage) y))))))))

;;; cursor

(defclass clx-driver-cursor (driver-cursor)
  ((xcursor :initarg :xcursor)))

(defvar *clx-cursor-mapping*  
  '(;; These are taken from the Franz CLIM User's Guide
    (:busy 150)
    (:button 60)
    (:default 68)
    (:horizontal-scroll 108)
    (:horizontal-thumb 108)
    (:lower-left 12)
    (:lower-right 14)
    (:move 52)
    (:position 130)
    (:prompt 152)
    (:scroll-down 106)
    (:scroll-left 110)
    (:scroll-right 112)
    (:scroll-up 114)
    (:upper-left 134)
    (:upper-right 136)
    (:vertical-scroll 116)
    (:vertical-thumb 116)
    ;; The following are not in the Franz docs, but might be useful.
    (:i-beam 152)
    (:vertical-pointer 22)
    (:pencil 86)
    (:rotate 50)    
    (:choose 60)))

(defmethod driver-avaiable-cursor-names ((driver clx-driver))
  (mapcar #'car *clx-cursor-mapping*))

(defmethod driver-create-cursor ((driver clx-driver) named-cursor)
  (with-slots (display) driver
    (let ((font (xlib:open-font display "cursor")))
      (let* ((code (second (or
                            (assoc named-cursor *clx-cursor-mapping*)
                            (assoc :default *clx-cursor-mapping*))))
             (cursor 
              (xlib:create-glyph-cursor :foreground (xlib:make-color :red 0.0 :green 0.0 :blue 0.0)
                                        :background (xlib:make-color :red 1.0 :green 1.0 :blue 1.0)
                                        :source-font font
                                        :source-char code
                                        :mask-font font
                                        :mask-char (1+ code))))
        (xlib:close-font font)
        (make-instance 'clx-driver-cursor :xcursor cursor)))))

(defmethod driver-destroy-cursor ((cursor clx-driver-cursor))
  (with-slots (xcursor) cursor
    (xlib:free-cursor xcursor)))

(defmethod driver-set-window-cursor ((window clx-driver-window) cursor)
  (with-slots (xwindow) window
    (with-slots (xcursor) cursor
    (setf (xlib:window-cursor xwindow) xcursor))))

;;; pointers

(defmethod driver-grab-pointer ((driver clx-driver) window pointer)
  (with-slots (xwindow) window
    (let ((grab-result (xlib:grab-pointer
                        xwindow
                        '(:button-press :button-release
                          :leave-window :enter-window
                          :pointer-motion :pointer-motion-hint)
                        ;; Probably we want to set :cursor here..
                        :owner-p t)))
      (if (eq grab-result :success)
          :success
          nil))))

(defmethod driver-ungrab-pointer ((driver clx-driver) window pointer)
  (xlib:ungrab-pointer (clx-driver-display driver)))
