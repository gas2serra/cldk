(in-package :cldk-internals)

(defclass default-display-callback-handler (display-driver-callback-handler)
  ())

(defmacro <e- (kernel fn &rest args)
  `(within-user-mode (,kernel :block-p nil)
                     (funcall ,fn (event-handler ,kernel) ,@args)))

(defmethod driver-cb-window-configuration-event ((handler default-display-callback-handler) kernel win x y width height time)
  (check-kernel-mode)
  (<e- kernel #'handle-configure-event win x y width height time))

(defmethod driver-cb-repaint-event ((handler default-display-callback-handler) kernel win x y width height time)
  (check-kernel-mode)
  (<e- kernel #'handle-repaint-event win x y width height time))

(defmethod driver-cb-scroll-event ((handler default-display-callback-handler) kernel pointer dx dy win timestamp)
  (check-kernel-mode)
  (<e- kernel #'handle-scroll-event pointer dx dy win timestamp))

(defmethod driver-cb-button-event ((handler default-display-callback-handler) kernel kind pointer button win timestamp)
  (check-kernel-mode)
  (<e- kernel #'handle-button-event kind pointer button win timestamp))

(defmethod driver-cb-motion-event ((handler default-display-callback-handler) kernel pointer x y root-x root-y
                              win timestamp)
  (check-kernel-mode)
  (<e- kernel #'handle-motion-event pointer x y root-x root-y
       win timestamp))

(defmethod driver-cb-key-event ((handler default-display-callback-handler) kernel kind keyname character modifiers
                           win timestamp)
  (check-kernel-mode)
  (<e- kernel #'handle-key-event kind keyname character modifiers
       win timestamp))

(defmethod driver-cb-enter-event ((handler default-display-callback-handler) kernel pointer x y root-x root-y win time)
  (check-kernel-mode)
  (<e- kernel #'handle-enter-event pointer x y root-x root-y win time))

(defmethod driver-cb-leave-event ((handler default-display-callback-handler) kernel pointer win time)
  (check-kernel-mode)
  (<e- kernel #'handle-leave-event pointer win time))

(defmethod driver-cb-wm-delete-event ((handler default-display-callback-handler) kernel win time)
  (check-kernel-mode)
  (<e- kernel #'handle-wm-delete-event win time))
