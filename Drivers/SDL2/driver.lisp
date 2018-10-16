(in-package :cldk-driver-sdl2)

(defclass sdl2-driver (display-driver)
  ())

(defmethod driver-start ((driver sdl2-driver))
  (let ((options (driver-options driver)))
    (setf (driver-default-screen-index driver) (getf options :screen-id 0)))
  (sdl2:init :everything))
     
(defmethod driver-stop ((driver sdl2-driver))
  (sdl2:quit))

(defmethod driver-kill ((driver sdl2-driver))
  (and sdl2::*the-main-thread*
       (bt:thread-alive-p sdl2::*the-main-thread*)
       (bt:destroy-thread sdl2::*the-main-thread*)))

(defmethod driver-ping ((driver sdl2-driver))
  (and sdl2::*the-main-thread*
       (bt:thread-alive-p sdl2::*the-main-thread*)))

(defmethod driver-force-output ((driver sdl2-driver))
  )

;;; events

(defmethod driver-process-next-event ((driver sdl2-driver))
  (setf sdl2::*event-loop* t)
  (sdl2-event-handler driver driver))

;;; root
(defclass sdl2-driver-root (driver-root)
  ((screen-index)))

(defmethod driver-initialize-root ((driver sdl2-driver) root)
  (with-slots (screen-index) root
    (setf screen-index (driver-default-screen-index driver))))

(defmethod driver-destroy-root ((root sdl2-driver-root))
  (with-slots (screen-index) root
    (setf screen-index nil)))

(defmethod driver-root-size ((root sdl2-driver-root) units)
  (with-slots (screen-index) root
    (cffi:with-foreign-objects ((ddpi :float)
                                (hdpi :float)
                                (vdpi :float))
      (sdl2-ffi.functions:sdl-get-display-dpi screen-index ddpi hdpi vdpi)
      (let ((w)
            (h))
        (let ((rect (sdl2:get-display-bounds 0)))
          (setf w (sdl2:rect-width rect))
          (setf h (sdl2:rect-height rect)))
        (ecase units
          (:device (values w h))
          (:inches (values (/ w (cffi:mem-ref hdpi :float))
                           (/ h (cffi:mem-ref vdpi :float))))
          (:millimeters (values (* 25.4s0 (/ w (cffi:mem-ref hdpi :float)))
                                (* 25.4s0 (/ h (cffi:mem-ref vdpi :float))))))))))

(defmethod driver-root-pointer-position ((root sdl2-driver-root))
  (cffi:with-foreign-objects ((xpos :int)
                              (ypos :int))
    (sdl2-ffi.functions:sdl-get-global-mouse-state xpos ypos)
    (values (cffi:mem-ref xpos :int) (cffi:mem-ref ypos :int))))

;;; window

(defclass sdl2-driver-window (driver-window)
  ((sdlwindow :initarg :sdlwindow)))

(defmethod driver-object-id ((window sdl2-driver-window))
  (with-slots (sdlwindow) window
    (sdl2-ffi.functions::sdl-get-window-id sdlwindow)))

(defmethod driver-create-window ((driver sdl2-driver) name pretty-name x y width height mode)
  (let ((window-flags (sdl2::mask-apply 'sdl2::sdl-window-flags
                                        (if (eql mode :managed)
                                            '(:hidden :resizable)
                                            '(:hidden :borderless)))))
    ;; INPUT_GRABBED INPUT_FOCUS MOUSE_FOCUSSDL_WINDOW_ALWAYS_ON_TOP
    ;; SKIP_TASKBAR WINDOW_UTILITY WINDOW_TOOLTIP
    ;; WINDOW_POPUP_MENU
    (let ((window (sdl2::sdl-create-window pretty-name
                                           (or x (sdl2:windowpos-undefined))
                                           (or y (sdl2:windowpos-undefined))
                                           width
                                           height
                                           window-flags)))
      (make-instance 'sdl2-driver-window :sdlwindow window))))

(defmethod driver-initialize-window ((driver sdl2-driver) win name pretty-name x y width height mode)
  (let ((window-flags (sdl2::mask-apply 'sdl2::sdl-window-flags
                                        (if (eql mode :managed)
                                            '(:hidden :resizable)
                                            '(:hidden :borderless)))))
    ;; INPUT_GRABBED INPUT_FOCUS MOUSE_FOCUSSDL_WINDOW_ALWAYS_ON_TOP
    ;; SKIP_TASKBAR WINDOW_UTILITY WINDOW_TOOLTIP
    ;; WINDOW_POPUP_MENU
    (let ((window (sdl2::sdl-create-window pretty-name
                                           (or x (sdl2:windowpos-undefined))
                                           (or y (sdl2:windowpos-undefined))
                                           width
                                           height
                                           window-flags)))
      (with-slots (sdlwindow) win
        (setf sdlwindow window)))))


(defmethod driver-destroy-window ((window sdl2-driver-window))
  (with-slots (sdlwindow) window
    (sdl2::sdl-destroy-window sdlwindow)
    (setf sdlwindow nil)))

(defmethod driver-show-window ((window sdl2-driver-window))
  (with-slots (sdlwindow) window
    (sdl2:show-window sdlwindow)))
  
(defmethod driver-hide-window ((window sdl2-driver-window))
  (with-slots (sdlwindow) window
    (sdl2:hide-window sdlwindow)))

(defmethod driver-window-position ((window sdl2-driver-window))
  (with-slots (sdlwindow) window
    (sdl2:get-window-position sdlwindow)))

(defmethod driver-window-size ((window sdl2-driver-window))
  (with-slots (sdlwindow) window
    (sdl2:get-window-size sdlwindow)))
 
(defmethod driver-set-window-position ((window sdl2-driver-window) x y)
  (with-slots (sdlwindow) window
    (sdl2:set-window-position sdlwindow x y)))

(defmethod driver-set-window-size ((window sdl2-driver-window) width height)
  (with-slots (sdlwindow) window
    (sdl2:set-window-size sdlwindow width height)))

(defmethod driver-set-window-hints ((window sdl2-driver-window) x y width height max-width max-height
                                    min-width min-height)
  (with-slots (sdlwindow) window
    (when (and x y)
      (sdl2:set-window-position sdlwindow x y))
    (when (or width height)
      (sdl2:set-window-size sdlwindow (or width 100) (or height 100)))
    (when (or max-width max-height)
      (sdl2-ffi.functions:sdl-set-window-maximum-size sdlwindow
                                                      (or max-width 10000)
                                                      (or max-height 10000)))
    (when (or min-width min-height)
      (sdl2-ffi.functions:sdl-set-window-minimum-size sdlwindow
                                                      (or min-width 0)
                                                      (or min-height 0)))))

(defmethod driver-bury-window ((window sdl2-driver-window))
  nil)

(defmethod driver-raise-window ((window sdl2-driver-window))
  (with-slots (sdlwindow) window
    (sdl2-ffi.functions:sdl-raise-window sdlwindow)))

(defmethod driver-window-pointer-position ((window sdl2-driver-window))
  (with-slots (sdlwindow) window
    (cffi:with-foreign-objects ((xpos :int)
                                (ypos :int))
      (sdl2-ffi.functions:sdl-get-mouse-state xpos ypos)
      (values (cffi:mem-ref xpos :int) (cffi:mem-ref ypos :int)))))

;;; cursors
(defclass sdl2-driver-cursor (driver-cursor)
  ((sdlcursor :initarg :sdlcursor)))

(defvar *sdl2-cursor-mapping*  
  `((:busy ,sdl2-ffi:+sdl-system-cursor-wait+)
    (:default ,sdl2-ffi:+sdl-system-cursor-arrow+)
    (:i-beam ,sdl2-ffi:+sdl-system-cursor-ibeam+)
    (:crosshair ,sdl2-ffi:+sdl-system-cursor-crosshair+)
    (:hand ,sdl2-ffi:+sdl-system-cursor-hand+)
    (:no ,sdl2-ffi:+sdl-system-cursor-no+)
    (:sizeall ,sdl2-ffi:+sdl-system-cursor-sizeall+)
    (:sizenesw ,sdl2-ffi:+sdl-system-cursor-sizenesw+)
    (:sizens ,sdl2-ffi:+sdl-system-cursor-sizens+)
    (:sizenwse ,sdl2-ffi:+sdl-system-cursor-sizenwse+)
    (:sizewe ,sdl2-ffi:+sdl-system-cursor-sizewe+)
    (:waitarrow ,sdl2-ffi:+sdl-system-cursor-waitarrow+)))

(defmethod driver-avaiable-cursor-names ((driver sdl2-driver))
  (mapcar #'car *sdl2-cursor-mapping*))

(defmethod driver-create-cursor ((driver sdl2-driver) named-cursor)
  (with-slots (display) driver
    (let* ((code (second (or
                          (assoc named-cursor *sdl2-cursor-mapping*)
                          (assoc :default *sdl2-cursor-mapping*)))))
      (make-instance 'sdl2-driver-cursor :sdlcursor (sdl2-ffi.functions:sdl-create-system-cursor code)))))

(defmethod driver-destroy-cursor ((cursor sdl2-driver-cursor))
  (with-slots (sdlcursor) cursor
    (sdl2-ffi.functions:sdl-free-cursor sdlcursor)))

(defmethod driver-set-window-cursor ((window sdl2-driver-window) cursor)
  (declare (ignore window))
  (with-slots (sdlcursor) cursor
    (sdl2-ffi.functions:sdl-set-cursor sdlcursor)))

;;; buffer

(defclass sdl2-driver-buffer (driver-buffer)
  ((surface :initarg :surface
            :reader driver-buffer-surface
            :initform nil)
   (pixels :initform nil)))

(defmethod driver-initialize-buffer ((driver sdl2-driver) buffer width height)
  (driver-update-buffer driver buffer width height))

(defmethod driver-update-buffer ((buffer sdl2-driver-buffer) width height)
  (with-slots (surface pixels) buffer
    (when surface
      (static-vectors:free-static-vector pixels)
      (sdl2:free-surface surface))
    (setf pixels (static-vectors:make-static-vector (* width height 4)
                                                    :element-type '(unsigned-byte 8)))
    (setf surface (sdl2:create-rgb-surface-with-format-from
                   (static-vectors:static-vector-pointer pixels)
                   width
                   height
                   32
                   (* 4 width)
                   :format sdl2:+pixelformat-rgba8888+))))


(defmethod driver-destroy-buffer ((buffer sdl2-driver-buffer))
  (with-slots (surface pixels) buffer
    (when surface
      (static-vectors:free-static-vector pixels)
      (sdl2:free-surface surface)
      (setf pixels nil
            surface nil))))

(defmethod driver-copy-buffer-to-window ((buffer sdl2-driver-buffer) x y width height
                                         window to-x to-y)
  (with-slots (sdlwindow) window
    (with-slots (surface) buffer
      (when (and surface sdlwindow
                 (>= x 0) (>= y 0) (> width 0) (> height 0) (>= to-x 0) (>= to-y 0))
        (let ((w (min width
                      (- (sdl2:surface-width surface) x)))
              (h (min height
                      (- (sdl2:surface-height surface) y))))
          (let ((windsurf (sdl2-ffi.functions:sdl-get-window-surface sdlwindow)))
            (sdl2:with-rects
             ((src x y  w h))
             (sdl2:with-rects
              ((dst to-x to-y  w h))
               (sdl2:blit-surface  surface src windsurf dst)
               (sdl2-ffi.functions:sdl-update-window-surface-rects sdlwindow dst 1)))))))))

(deftype sdl2-rgb-image-pixels () 'cffi-sys:foreign-pointer)

(defmethod driver-buffer-rgb-get-fn ((buffer sdl2-driver-buffer) dx dy)
  (with-slots (surface) buffer
    (let ((pixels (sdl2:surface-pixels surface))
          (width (sdl2:surface-width surface)))
    (declare (type sdl2-rgb-image-pixels pixels))
    (lambda (x y)
      (cffi-sys:%mem-ref
       pixels :UNSIGNED-INT (* 4
                               (+
                                (* (+ y dy) width)
                                (+ x dx))))))))

(defmethod driver-buffer-rgb-set-fn ((buffer sdl2-driver-buffer) dx dy)
  (with-slots (surface) buffer
    (let ((pixels (sdl2:surface-pixels surface))
          (width (sdl2:surface-width surface)))
      (declare (type sdl2-rgb-image-pixels pixels))
      (lambda (x y r g b)
        (cffi-sys:%mem-set
         (dpb 255 (byte 8 0)
              (dpb b (byte 8 8)
                   (dpb g (byte 8 16)
                        (dpb r (byte 8 24) 0))))
         pixels :UNSIGNED-INT (* 4
                                 (+
                                  (* (+ y dy) width)
                                  (+ x dx))))))))

;; pointer

(defmethod driver-grab-pointer ((driver sdl2-driver) window pointer)
  (with-slots (sdlwindow) window
    (sdl2-ffi.functions:sdl-set-window-grab sdlwindow 1))
  :success)

(defmethod driver-ungrab-pointer ((driver sdl2-driver) window pointer)
  (with-slots (sdlwindow) window
    (sdl2-ffi.functions:sdl-set-window-grab sdlwindow 0))
  :success)
