(in-package :cldk-sdl2)

(defclass sdl2-driver (display-driver)
  ())

(defmethod driver-start ((driver sdl2-driver))
  (let ((options (driver-options driver)))
    (setf (driver-default-screen-index driver) (getf options :screen-id 0)))
  (with-slots (render) driver
    (sdl2:init :everything)))
     
(defmethod driver-stop ((driver sdl2-driver))
  (sdl2:quit))

(defmethod driver-force-output ((driver sdl2-driver))
  ;; it is no required
  )

;;; screens
(defmethod driver-screen-num ((driver sdl2-driver))
  (sdl2:get-num-video-displays))

(defmethod driver-screen-size ((driver sdl2-driver) screen-index units)
  (cffi:with-foreign-objects ((ddpi :float)
                              (hdpi :float)
                              (vdpi :float))
    (unless screen-index
      (setf screen-index (driver-default-screen-index driver)))
    (sdl2-ffi.functions:sdl-get-display-dpi screen-index ddpi hdpi vdpi)
    (let ((w)
          (h))
      (let ((rect (sdl2:get-display-bounds 0)))
        (setf w (sdl2:rect-width rect))
        (setf h (sdl2:rect-height rect)))
      (ecase units
        (:device (list w h))
        (:inches (list (/ w (cffi:mem-ref hdpi :float))
                       (/ h (cffi:mem-ref vdpi :float))))
        (:millimeters (list (* 25.4s0 (/ w (cffi:mem-ref hdpi :float)))
                            (* 25.4s0 (/ h (cffi:mem-ref vdpi :float)))))))))

(defmethod driver-screen-dpi ((driver sdl2-driver) screen-index)
  (cffi:with-foreign-objects ((ddpi :float)
                              (hdpi :float)
                              (vdpi :float))
    (unless screen-index
      (setf screen-index (driver-default-screen-index driver)))
    (sdl2-ffi.functions:sdl-get-display-dpi screen-index ddpi hdpi vdpi)
    (list
     (cffi:mem-ref hdpi :float)
     (cffi:mem-ref vdpi :float))))

(defmethod driver-screen-pointer-position ((driver sdl2-driver))
  (cffi:with-foreign-objects ((xpos :int)
                              (ypos :int))
    (sdl2-ffi.functions:sdl-get-global-mouse-state xpos ypos)
    (list (cffi:mem-ref xpos :int) (cffi:mem-ref ypos :int))))

;;; window

(defclass sdl2-window (driver-window)
  ((sdlwindow :initarg :sdlwindow)
   (render :initarg :render)))

(defmethod driver-object-id ((window sdl2-window))
  (with-slots (sdlwindow) window
    (sdl2-ffi.functions::sdl-get-window-id sdlwindow)))

(defmethod driver-create-window ((driver sdl2-driver) name pretty-name x y width height mode)
  (let ((window-flags (sdl2::mask-apply 'sdl2::sdl-window-flags
                                        (if (eql mode :managed)
                                            '(:resizable :hidden)
                                            '(:hidden :borderless)))))
    (log:info name pretty-name)
    (let ((window (sdl2::sdl-create-window pretty-name
                                           x
                                           y
                                           width
                                           height
                                           window-flags)))
      (make-instance 'sdl2-window :sdlwindow window
                     :render (sdl2:create-renderer window)))))

(defmethod driver-destroy-window ((driver sdl2-driver) window)
  (with-slots (sdlwindow render) window
    (sdl2::sdl-destroy-window sdlwindow)
    (setf sdlwindow nil)
    (setf render nil)
    ;; destroy render
    ))

(defmethod driver-show-window ((driver sdl2-driver) window)
  (with-slots (sdlwindow) window
    (sdl2:show-window sdlwindow)))
  
(defmethod driver-hide-window ((driver sdl2-driver) window)
  (with-slots (sdlwindow) window
    (sdl2:hide-window sdlwindow)))

(defmethod driver-window-position ((driver sdl2-driver) window)
  (with-slots (sdlwindow) window
    (multiple-value-list (sdl2:get-window-position sdlwindow))))

(defmethod driver-window-size ((driver sdl2-driver) window)
  (with-slots (sdlwindow) window
    (multiple-value-list (sdl2:get-window-size sdlwindow))))
 
(defmethod driver-set-window-position ((driver sdl2-driver) window x y)
  (with-slots (sdlwindow) window
    (sdl2:set-window-position sdlwindow x y)))

(defmethod driver-set-window-size ((driver sdl2-driver) window width height)
  (with-slots (sdlwindow) window
    (sdl2:set-window-size sdlwindow width height)))

(defmethod driver-set-window-hints ((driver sdl2-driver) window x y width height max-width max-height
                                         min-width min-height)
  (with-slots (sdlwindow render) window
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

(defmethod driver-bury-window ((driver sdl2-driver) window)
  (with-slots (sdlwindow render) window
    ))

(defmethod driver-raise-window ((driver sdl2-driver) window)
  (with-slots (sdlwindow render) window
    (sdl2-ffi.functions:sdl-raise-window sdlwindow)))

(defmethod driver-window-pointer-position ((driver sdl2-driver) window)
  (with-slots (sdlwindow render) window
    (cffi:with-foreign-objects ((xpos :int)
                                (ypos :int))
      (sdl2-ffi.functions:sdl-get-mouse-state xpos ypos)
      (list (cffi:mem-ref xpos :int) (cffi:mem-ref ypos :int)))))

;;; cursors
(defclass sdl2-cursor (driver-cursor)
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
      (make-instance 'sdl2-cursor :sdlcursor (sdl2-ffi.functions:sdl-create-system-cursor code)))))

(defmethod driver-destroy-cursor ((driver sdl2-driver) cursor)
  (with-slots (sdlcursor) cursor
    (sdl2-ffi.functions:sdl-free-cursor sdlcursor)))

(defmethod driver-set-window-cursor ((driver sdl2-driver) window cursor)
  (declare (ignore window))
  (with-slots (sdlcursor) cursor
    (sdl2-ffi.functions:sdl-set-cursor sdlcursor)))

;;; process events

(defmethod driver-process-next-event ((driver sdl2-driver) kernel &key timeout)
  (setf sdl2::*event-loop* t)
  (let ((ret
         (sdl2-event-handler driver kernel timeout)))
    (if ret
        t
        nil)))

(defclass sdl2-buffer (driver-buffer)
  ((surface :initarg :surface
            :initform nil)
   (width :initarg :width
          :reader driver-buffer-width)
   (height :initarg :height
           :reader driver-buffer-height)))

(defmethod driver-create-buffer ((driver sdl2-driver) width height)
  (let ((surface (sdl2:create-rgb-surface width height 32
                                          :r-mask #x000000ff
                                          :g-mask #x0000ff00
                                          :b-mask #x00ff0000
                                          :a-mask #xff000000)))
    
    (make-instance 'sdl2-buffer
                   ;;:data (sdl2:surface-pixels surface)
                   :surface surface
                   :width width :height height)))

(defmethod driver-destroy-buffer ((driver sdl2-driver) buffer)
  (with-slots (surface) buffer
    (sdl2:free-surface surface)
    (setf surface nil)))

(defmethod driver-copy-buffer-to-window ((driver sdl2-driver) buffer x y width height
                                         window to-x to-y)
  #+nil (log:info x y to-x to-y width height)
  (with-slots (sdlwindow render) window
    (with-slots (surface) buffer
      (when surface
        (let ((texture (sdl2:create-texture-from-surface render surface)))
          (sdl2:with-rects
              ((src x y
                    width height))
            (sdl2:with-rects
                ((dst to-x to-y
                      width height))
              (sdl2:render-copy render
                                texture :source-rect src
                                :dest-rect dst)))
          (sdl2::sdl-destroy-texture texture)
          (sdl2:render-present render))))))

(defmethod driver-copy-image-to-buffer ((driver sdl2-driver) image x y width height buffer)
  (let ((w (driver-buffer-width buffer)))
    (with-slots (surface) buffer
      (let ((data (sdl2:surface-pixels surface)))
              (when data
                (loop for j from y to (+ y height -1) do
                     (loop for i from x to (+ x width -1) do
                          (let* ((pixel (aref (image-pixels image) j i)))
                            (let ((red (ldb (byte 8 0) pixel))
                                  (green (ldb (byte 8 8) pixel))
                                  (blue (ldb (byte 8 16) pixel))
                                  (alpha (ldb (byte 8 24) pixel)))
                              (setf alpha 255)
                              (cffi-sys:%mem-set
                               (dpb blue (byte 8 0)
                                    (dpb green (byte 8 8)
                                         (dpb red (byte 8 16)
                                              (dpb alpha (byte 8 24) 0))))
                               data :UNSIGNED-INT (* 4
                                                     (+
                                                      (* j w)
                                                      i))))))))))))

(defmethod driver-grab-pointer ((driver sdl2-driver) window pointer)
  (with-slots (sdlwindow) window
    (let ((grab-result (SDL2-FFI.FUNCTIONS:SDL-SET-WINDOW-GRAB 
                        sdlwindow
                        1)))
      (log:info grab-result)
      (if (eq grab-result :success)
          :success
          nil)
      :success)))

(defmethod driver-ungrab-pointer ((driver sdl2-driver) window pointer)
  (with-slots (sdlwindow) window
    (let ((grab-result (SDL2-FFI.FUNCTIONS:SDL-SET-WINDOW-GRAB 
                        sdlwindow
                        0)))
      (log:info grab-result))))

