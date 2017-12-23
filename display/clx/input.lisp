(in-package :cldk-clx)

(defun decode-x-button-code (code)  
  (let ((button-mapping #.(vector +pointer-left-button+
                                  +pointer-middle-button+
                                  +pointer-right-button+
                                  0 0 0 0
                                  +pointer-x1-button+
                                  +pointer-x2-button+))
        (code (1- code)))
    (when (and (>= code 0)
               (< code (length button-mapping)))
      (aref button-mapping code))))

(defun clx-event-handler (&key display window event-key code state mode time
                            type width height x y root-x root-y
                            data override-redirect-p send-event-p hint-p
                            target property requestor selection
                            request first-keycode count
                            &allow-other-keys)
  (handler-case
      (let ((win window))
        (case event-key
          ((:button-press :button-release)
           (if (and (>= code 4) (<= code 7))
               (k-handle-scroll-event *clx-kernel*
                                      0
                                      (case code
                                        (4 -1)
                                        (5 1)
                                        (otherwise 0))
                                      (case code
                                        (6 -1)
                                        (7 1)
                                        (otherwise 0))
                                      win
                                      time)
               (k-handle-button-event *clx-kernel* 
                                      (if (eq event-key :button-press)
                                          :press
                                          :release)
                                      0
                                      (decode-x-button-code code)
                                      win
                                      time)))
          (:motion-notify
           (k-handle-motion-event *clx-kernel* 0
                                  x y root-x root-y
                                  win time))
          ((:key-press :key-release)
           (multiple-value-bind (keyname modifier-state keysym-name)
               (x-event-to-key-name-and-modifiers *clx-driver* 
                                                  event-key code state)
             (k-handle-key-event *clx-kernel* 
                                 (if (eq event-key :key-press)
                                     :press
                                     :release)
                                 keysym-name
                                 (and
                                  (characterp keyname)
                                  keyname)
                                 modifier-state
                                 win
                                 time)))
          ((:enter-notify)
           (k-handle-enter-event *clx-kernel* 0
                                 x y root-x root-y
                                 win time))
          ((:leave-notify)
           (k-handle-leave-event *clx-kernel* 0 win time))
          (:configure-notify
           (with-slots (root-window) *clx-driver*
             (multiple-value-bind (x y)
                 (xlib:translate-coordinates window 0 0 root-window)
               (k-handle-window-configuration-event *clx-kernel* win x y width height time))))
          ((:exposure :display :graphics-exposure)
           (k-handle-repaint-event *clx-kernel* win x y width height time))
          (:client-message
           (port-client-message win time type data))
          (t
           (log:info "Bo: ~A" event-key)
           (unless (xlib:event-listen display)
             (xlib:display-force-output display))
           nil)))
    (error (condition) (setf *clx-error* condition))))

(defgeneric port-wm-protocols-message (sheet time message data))

(defmethod port-client-message (win time (type (eql :wm_protocols)) data)
  (port-wm-protocols-message win time
                             (xlib:atom-name (clx-driver-display *clx-driver*) (aref data 0))
                             data))

(defmethod port-client-message (sheet time (type t) data)
  (warn "Unprocessed client message: ~:_type = ~S;~:_ data = ~S;~_ sheet = ~S."
        type data sheet))

(defmethod port-wm-protocols-message (win time (message (eql :wm_delete_window)) data)
  (declare (ignore data))
  (k-handle-wm-delete-event *clx-kernel* win time))

(defmethod port-wm-protocols-message (sheet time (message t) data)
  (warn "Unprocessed WM Protocols message: ~:_message = ~S;~:_ data = ~S;~_ sheet = ~S."
        message data sheet))
  

