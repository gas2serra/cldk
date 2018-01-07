(in-package :cldk-sdl2)

(defun decode-sdl2-button-code (code)
  (let ((button-mapping #.(vector +pointer-left-button+
                                  +pointer-middle-button+
                                  +pointer-right-button+
                                  +pointer-x1-button+
                                  +pointer-x2-button+))
        (code (1- code)))
    (when (and (>= code 0)
               (< code (length button-mapping)))
      (aref button-mapping code))))

(defun sdl2-event-handler (driver kernel)
  (sdl2:with-sdl-event (event)
    (let ((r (sdl2:next-event event)))
      (when (< r 0)
        (log:error (sdl2-ffi.functions::sdl-get-error)))
      (when (= r 0)
        nil)
      (when (> r 0)
        (let ((etype (sdl2:get-event-type event )))
          #+nil (log:info "event ~A" etype)
          (case etype
            ((:mousebuttondown :mousebuttonup)
             (let ((win (sdl2::c-ref event sdl2-ffi:sdl-event :button :window-id))
		   (time (sdl2::c-ref event sdl2-ffi:sdl-event :button :timestamp))
		   (x (sdl2::c-ref event sdl2-ffi:sdl-event :button :x))	  
		   (y (sdl2::c-ref event sdl2-ffi:sdl-event :button :y))
		   (button (sdl2::c-ref event sdl2-ffi:sdl-event :button :button))	  
		   (which (sdl2::c-ref event sdl2-ffi:sdl-event :button :which))
		   (state (sdl2::c-ref event sdl2-ffi:sdl-event :button :state)))
               (declare (ignore state))
               (k-handle-button-event kernel
				      (if (eq etype :mousebuttondown)
					  :press
					  :release)
				      which
				      (decode-sdl2-button-code button)
				      win
				      time)))
            (:mousewheel
             (let ((win (sdl2::c-ref event sdl2-ffi:sdl-event :wheel :window-id))
		   (time (sdl2::c-ref event sdl2-ffi:sdl-event :wheel :timestamp))
		   (x (sdl2::c-ref event sdl2-ffi:sdl-event :wheel :x))	  
		   (y (sdl2::c-ref event sdl2-ffi:sdl-event :wheel :y))
		   (which (sdl2::c-ref event sdl2-ffi:sdl-event :wheel :which))
		   (direction (sdl2::c-ref event sdl2-ffi:sdl-event :wheel :direction)))
               (declare (ignore direction))
               (k-handle-scroll-event kernel
                                      which
                                      x
                                      (- y)
                                      win
                                      time)))
            (:mousemotion
             (let ((win (sdl2::c-ref event sdl2-ffi:sdl-event :motion :window-id))
		   (time (sdl2::c-ref event sdl2-ffi:sdl-event :motion :timestamp))
		   (x (sdl2::c-ref event sdl2-ffi:sdl-event :motion :x))	  
		   (y (sdl2::c-ref event sdl2-ffi:sdl-event :motion :y))
		   (xrel (sdl2::c-ref event sdl2-ffi:sdl-event :motion :xrel))	  
		   (yrel (sdl2::c-ref event sdl2-ffi:sdl-event :motion :yrel))
		   (which (sdl2::c-ref event sdl2-ffi:sdl-event :motion :which))
		   (state (sdl2::c-ref event sdl2-ffi:sdl-event :motion :state)))
	       (declare (ignore xrel yrel state))
	       (multiple-value-bind (pos-x pos-y)
                   (sdl2:get-window-position (sdl2-ffi.functions::sdl-get-window-from-id win))
                 (k-handle-motion-event kernel
					which
					x y
					(+ x pos-x) (+ y pos-y)
					win
					time))))
            ((:keydown :keyup)
             (let* ((w (sdl2::c-ref event sdl2-ffi:sdl-event :key :window-id))
                    (time (sdl2::c-ref event sdl2-ffi:sdl-event :key :timestamp))
                    (keysym (sdl2::c-ref event sdl2-ffi:sdl-event :key :keysym))	  
                    (state (sdl2::c-ref event sdl2-ffi:sdl-event :key :state))
                    (win w))
               (multiple-value-bind (keyname alpha-p modifier-state keysym-name)
                   (sdl2-event-to-key-name-and-modifiers driver
                                                         etype
                                                         (sdl2:sym-value keysym)
                                                         (sdl2:mod-value keysym))
                 (setf *key-modifiers* modifier-state)
                 (when (or (not alpha-p)
                           (= (logand +control-key+
                                      modifier-state)
                              +control-key+))
                   ;;(log:info "====> !! NOT CHAR: ~A ~A" alpha-p (decode-sdl2-mod-state (sdl2:mod-value keysym)))
                   (k-handle-key-event kernel
                                      (if (eq etype :keydown)
                                          :press
                                          :release)
                                      keysym-name
                                      (and (characterp keyname) keyname)
                                      modifier-state
                                      win
                                      time)))))
            (:textinput
             (let* ((w (sdl2::c-ref event sdl2-ffi:sdl-event :text :window-id))
                    (time (sdl2::c-ref event sdl2-ffi:sdl-event :text :timestamp))
                    (text (sdl2::c-ref event sdl2-ffi:sdl-event :text :text))	  
                    (win w))
               #+nil (log:info "====>>> TEXT INPUT!!! ~A ~A ~A" (code-char text) *key-modifiers*
                         (logand (- #xFFFF +shift-key+) *key-modifiers*))
               (when (= (logand +control-key+
                                *key-modifiers*)
                        0)
                 #+nil (log:info "====>>> CHAR!!! ~A ~A" text *key-modifiers*)
                 (k-handle-key-event kernel
                                     :press
                                     (make-symbol (string (code-char text)))
                                     (code-char text)
                                     (logand (- #xFFFF +shift-key+) *key-modifiers*)
                                     win
                                     time)
                 (k-handle-key-event kernel
                                     :release
                                     (make-symbol (string (code-char text)))
                                     (code-char text)
                                     (logand (- #xFFFF +shift-key+) *key-modifiers*)
                                     win
                                     time))))
            (:quit
             #+nil (log:info "QUIT"))
            (:windowevent
             (let* ((e (sdl2::c-ref event sdl2-ffi:sdl-event :window :event))
                    (win (sdl2::c-ref event sdl2-ffi:sdl-event :window :window-id))
                    (time (sdl2::c-ref event sdl2-ffi:sdl-event :window :timestamp))
                    (data1 (sdl2::c-ref event sdl2-ffi:sdl-event :window :data1))	  
                    (data2 (sdl2::c-ref event sdl2-ffi:sdl-event :window :data2)))
               ;;(log:info "Window Event:: ~A ~A ~A ~A ~A~%" e (list w win) time data1 data2)
               (cond
                 ((= e sdl2-ffi::+sdl-windowevent-close+)
                  (when win
                    (k-handle-wm-delete-event kernel win time)))
                 ((= e sdl2-ffi::+sdl-windowevent-enter+)
                  (cffi:with-foreign-objects ((x :int)
                                              (y :int))
                    (sdl2-ffi.functions:sdl-get-mouse-state x y)
                    (multiple-value-bind (pos-x pos-y)
                        (sdl2:get-window-position (sdl2-ffi.functions::sdl-get-window-from-id win))
                      (k-handle-enter-event kernel
                                            0
                                            (cffi:mem-ref x :int) (cffi:mem-ref y :int)
                                            (+ (cffi:mem-ref x :int) pos-x)
                                            (+ (cffi:mem-ref y :int) pos-y)
                                            win
                                            time))))
                 ((= sdl2-ffi::+sdl-windowevent-exposed+ e)
                  (multiple-value-bind (w h)
		      (sdl2:get-window-size (sdl2-ffi.functions::sdl-get-window-from-id win))
		    (k-handle-repaint-event kernel win 0 0 w h time)))
                 ((= sdl2-ffi::+sdl-windowevent-focus-gained+ e)
                  #+nil (log:info "Window Event Focus in:: ~A ~A ~A ~A ~A~%" e (list w win) time data1 data2)
                  nil)
                 ((= sdl2-ffi::+sdl-windowevent-focus-lost+ e)
                  #+nil (log:info "Window Event Focus out:: ~A ~A ~A ~A ~A~%" e (list w win) time data1 data2)
                  nil)
                 ((= sdl2-ffi::+sdl-windowevent-hidden+ e)
                  #+nil (log:info "Window Event Hidden:: ~A ~A ~A ~A ~A~%" e (list w win) time data1 data2)
                  nil)
                 ((= sdl2-ffi::+sdl-windowevent-leave+ e)
                  (k-handle-leave-event kernel 0 win time))
                 ((= sdl2-ffi::+sdl-windowevent-maximized+ e)
                  #+nil (log:info "Window Event Max:: ~A ~A ~A ~A ~A~%" e (list w win) time data1 data2)
                  nil)
                 ((= sdl2-ffi::+sdl-windowevent-minimized+ e)
                  #+nil (log:info "Window Event Min:: ~A ~A ~A ~A ~A~%" e (list w win) time data1 data2)
                  nil)
                 ((= sdl2-ffi::+sdl-windowevent-moved+ e)
                  (when win
                    (multiple-value-bind (w h)
                        (sdl2:get-window-size (sdl2-ffi.functions::sdl-get-window-from-id win))
                      (k-handle-window-configuration-event kernel win
                                                           data1 data2
                                                           w h time))))
                 ((= sdl2-ffi::+sdl-windowevent-none+ e)
                  #+nil (log:info "Window Event None:: ~A ~A ~A ~A ~A~%" e (list w win) time data1 data2)
                  nil)
                 ((= sdl2-ffi::+sdl-windowevent-resized+ e)
                  ;;(log:info "Window Event Resized:: ~A ~A ~A ~A ~A~%" e (list w win) time data1 data2)
                  #+nil(when win
                    (multiple-value-bind (pos-x pos-y)
                        (sdl2:get-window-position (sdl2-ffi.functions::sdl-get-window-from-id w))
                      (k-handle-window-configuration-event kernel win
                                               pos-x pos-y
                                               data1 data2 time)))
                  ;;(log:info "Window Resized:: ~A ~A ~A ~A ~A~%" e win time data1 data2)
                  nil)
                 ((= sdl2-ffi::+sdl-windowevent-restored+ e)
                  #+nil (log:info "Window Event Restored:: ~A ~A ~A ~A ~A~%" e (list w win) time data1 data2)
                  nil)
                 ((= sdl2-ffi::+sdl-windowevent-shown+ e)
                  #+nil (log:info "Window Event Shown:: ~A ~A ~A ~A ~A~%" e (list w win) time data1 data2)
                  nil)
                 ((= sdl2-ffi::+sdl-windowevent-size-changed+ e)
                  (when win
                    (multiple-value-bind (pos-x pos-y)
                        (sdl2:get-window-position (sdl2-ffi.functions::sdl-get-window-from-id win))
                      (k-handle-window-configuration-event kernel win
							   pos-x pos-y
							   data1 data2 time))))
                 (t
		  (log:info "Bo: ~a ~A ~A ~A ~A~%" e win time data1 data2)
		  nil))))
            (t
             (log:debug "event ~A" etype))))
        t))))
