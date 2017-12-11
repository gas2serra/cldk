(in-package :cldk-clx)

(defun decode-x-button-code (code)  
  (let ((button-mapping #.(vector +pointer-left-button+
                                  +pointer-middle-button+
                                  +pointer-right-button+
                                  +pointer-wheel-up+
                                  +pointer-wheel-down+
                                  +pointer-wheel-left+
                                  +pointer-wheel-right+
                                  +pointer-x1-button+
                                  +pointer-x2-button+))
        (code (1- code)))
    (when (and (>= code 0)
               (< code (length button-mapping)))
      (aref button-mapping code))))

#|
(defun decode-x-button-code (code)  
  (let ((button-mapping #.(vector :left
                                  :middle
                                  :right
                                  :wheel-up
                                  :wheel-down
                                  :wheel-left
                                  :wheel-right
                                  :x1
                                  :x2))
        (code (1- code)))
    (if (and (>= code 0)
             (< code (length button-mapping)))
        (aref button-mapping code)
        :unknow)))
|#

(defparameter mod-mappings (list (cons 1 :shift)
                                 (cons 2 :caps)
                                 (cons 4 :ctrl)
                                 (cons 8 :alt)
                                 (cons 16 :num)
                                 (cons 64 :gui)
                                 (cons 256 :left-alt)
                                 (cons 256 :left-ctrl)
                                 (cons 256 :left-gui)
                                 (cons 256 :left-shift)
                                 (cons 256 :mode)
                                 (cons 256 :right-alt)
                                 (cons 256 :right-ctrl)
                                 (cons 256 :right-gui)
                                 (cons 256 :right-shift)
                                 ))

(defun decode-clx-mod-state (state)
  (let ((res nil))
    (dolist (map mod-mappings)
      (unless (eql (logand (car map) state) 0)
        (push (cdr map) res)))
    res))

(defun clx-event-handler (&key display window event-key code state mode time
                        type width height x y root-x root-y
                        data override-redirect-p send-event-p hint-p
                        target property requestor selection
                        request first-keycode count
                            &allow-other-keys)
  (handler-case
      (let ((win (lookup-server-object *clx-driver* window)))
     (setf (driver-error *clx-driver*) nil)
     (case event-key
       ((:button-press :button-release)
        (if (and (>= code 4) (<= code 7))
            (khandle-wheel-event *clx-kernel* 
                                 (decode-x-button-code code)
                                 0
                                 win
                                 time)
            (khandle-button-event *clx-kernel* 
                                  (if (eq event-key :button-press)
                                      :press
                                      :release)
                                  0
                                  (decode-x-button-code code)
                                  win
                                  time)))
       (:motion-notify
        (khandle-motion-event *clx-kernel* 
                              0
                              x y
                              root-x root-y
                              win
                              time))
       ((:key-press :key-release)
        (multiple-value-bind (keyname modifier-state keysym-name)
            (x-event-to-key-name-and-modifiers *clx-driver* 
                                               event-key code state)
          (khandle-key-event *clx-kernel* 
                             (if (eq event-key :key-press)
                                 :press
                                 :release)
                             keysym-name
                             (and
                              (characterp keyname)
                              keyname)
                             modifier-state ;;(decode-clx-mod-state state)
                             win
                             time)))
       ((:enter-notify :leave-notify)
        (khandle-enter-leave-event *clx-kernel*
                                   (if (eq event-key :enter-notify)
                                       :enter
                                       :leave)
                                   0
                                   win
                                   time))
       (:destroy-notify
        (khandle-destroy-event *clx-kernel* win time))
       (:configure-notify
        (with-slots (root-window) *clx-driver*
          (multiple-value-bind (x y)
              (xlib:translate-coordinates window 0 0 root-window)
            (khandle-window-configuration-event *clx-kernel* win x y width height time))))
       ((:exposure :display :graphics-exposure)
        (khandle-repaint-event *clx-kernel* win x y width height time))
       (:client-message
        (port-client-message win time type data))
       (t
        (unless (xlib:event-listen display)
          (xlib:display-force-output display))
        nil)))
    (error (condition) (setf (driver-error *clx-driver*) condition))))
         
    


(defgeneric port-wm-protocols-message (sheet time message data))

(defmethod port-client-message (win time (type (eql :wm_protocols)) data)
  (port-wm-protocols-message win time
                             (xlib:atom-name (clx-driver-display *clx-driver*) (aref data 0))
                             data))

(defmethod port-client-message (sheet time (type t) data)
  (warn "Unprocessed client message: ~:_type = ~S;~:_ data = ~S;~_ sheet = ~S."
        type data sheet))

(defmethod port-wm-protocols-message (sheet time (message (eql :wm_take_focus)) data)
  #+nil(let ((timestamp (elt data 1))
        (mirror (sheet-xmirror sheet)))
    (when mirror
      (xlib:set-input-focus (clx-port-display *clx-port*)
                            mirror :parent timestamp))
    nil))

(defmethod port-wm-protocols-message (win time (message (eql :wm_delete_window)) data)
  (declare (ignore data))
  (khandle-wm-delete-event *clx-kernel*
                         win
                         time))

(defmethod port-wm-protocols-message (sheet time (message t) data)
  (warn "Unprocessed WM Protocols message: ~:_message = ~S;~:_ data = ~S;~_ sheet = ~S."
        message data sheet))

