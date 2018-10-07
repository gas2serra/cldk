(in-package :cldk-driver-clx)

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
  (declare (ignore count first-keycode selection property
                   target hint-p send-event-p override-redirect-p
                   mode requestor request))
  (handler-case
      (let ((win window)
            (handler (driver-callback-handler *clx-kernel*)))
        (case event-key
          ((:button-press :button-release)
           (if (and (>= code 4) (<= code 7))
               (driver-cb-scroll-event handler *clx-kernel*
                                      0
                                      (case code
                                        (6 -1)
                                        (7 1)
                                        (otherwise 0))
                                      (case code
                                        (4 -1)
                                        (5 1)
                                        (otherwise 0))
                                      (lookup-driver-object *clx-kernel* win)
                                      time)
               (driver-cb-button-event handler *clx-kernel* 
                                      (if (eq event-key :button-press)
                                          :press
                                          :release)
                                      0
                                      (decode-x-button-code code)
                                      (lookup-driver-object *clx-kernel* win)
                                      time)))
          (:motion-notify
           (driver-cb-motion-event handler *clx-kernel* 0
                                  x y root-x root-y
                                  (lookup-driver-object *clx-kernel* win) time))
          ((:key-press :key-release)
           (multiple-value-bind (keyname modifier-state keysym-name)
               (decode-x-key event-key code state)
             (driver-cb-key-event handler *clx-kernel* 
                                 (if (eq event-key :key-press)
                                     :press
                                     :release)
                                 keysym-name
                                 (and
                                  (characterp keyname)
                                  keyname)
                                 modifier-state
                                 (lookup-driver-object *clx-kernel* win)
                                 time)))
          ((:enter-notify)
           (driver-cb-enter-event handler *clx-kernel* 0
                                 x y root-x root-y
                                 (lookup-driver-object *clx-kernel* win) time))
          ((:leave-notify)
           (driver-cb-leave-event handler *clx-kernel* 0 (lookup-driver-object *clx-kernel* win) time))
          (:configure-notify
           (with-slots (root-window) *clx-driver*
             (multiple-value-bind (x y)
                 (xlib:translate-coordinates window 0 0 root-window)
               (driver-cb-window-configuration-event handler *clx-kernel* (lookup-driver-object *clx-kernel* win) x y width height time))))
          ((:exposure :display :graphics-exposure)
           (driver-cb-repaint-event handler *clx-kernel* (lookup-driver-object *clx-kernel* win) x y width height time))
          (:mapping-notify
           (load-mapping (clx-driver-display *clx-driver*)))
          (:client-message
           (port-client-message win time type data))
          (t
           (log:info "Unprocessed clx event: ~A" event-key)
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
  (driver-cb-wm-delete-event (driver-callback-handler *clx-kernel*)
                             *clx-kernel*
                             (lookup-driver-object *clx-kernel* win)
                             time))

(defmethod port-wm-protocols-message (sheet time (message t) data)
  (warn "Unprocessed WM Protocols message: ~:_message = ~S;~:_ data = ~S;~_ sheet = ~S."
        message data sheet))
  

