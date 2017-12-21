(in-package :cldk-internals)

(defmacro <e- (kernel fn &rest args)
  `(<callback- ,kernel ,fn (event-handler ,kernel) ,@args))

(defun k-handle-window-configuration-event (kernel window x y width height time)
  (check-kernel-mode)
  (let ((kwindow (lookup-server-object kernel window)))
    (when (typep kwindow 'k-buffered-window-mixin)
      (k-notify-resize-buffered-window kwindow width height))
    (<e- kernel #'handle-configure-event kwindow x y width height time)))

(defun k-handle-repaint-event (kernel window x y width height time)
  (check-kernel-mode)
  (let ((win (lookup-server-object kernel window)))
    (<e- kernel #'handle-repaint-event win x y width height time)))

(defun k-handle-scroll-event (kernel pointer dx dy window timestamp)
  (check-kernel-mode)
  (let ((win (lookup-server-object kernel window)))
    (<e- kernel #'handle-scroll-event pointer dx dy win timestamp)))

(defun k-handle-button-event (kernel kind pointer button window timestamp)
  (check-kernel-mode)
  (let ((win (lookup-server-object kernel window)))
    (<e- kernel #'handle-button-event kind pointer button win timestamp)))

(defun k-handle-motion-event (kernel pointer x y root-x root-y
                              window timestamp)
  (check-kernel-mode)
  (let ((win (lookup-server-object kernel window)))
    (<e- kernel #'handle-motion-event pointer x y root-x root-y
         win timestamp)))

(defun k-handle-key-event (kernel kind keyname character modifiers
                           window timestamp)
  (check-kernel-mode)
  (let ((win (lookup-server-object kernel window)))
    (<e- kernel #'handle-key-event kind keyname character modifiers
         win timestamp)))

(defun k-handle-enter-event (kernel pointer x y root-x root-y window time)
  (check-kernel-mode)
  (let ((win (lookup-server-object kernel window)))
    (<e- kernel #'handle-enter-event pointer x y root-x root-y win time)))

(defun k-handle-leave-event (kernel pointer window time)
  (check-kernel-mode)
  (let ((win (lookup-server-object kernel window)))
    (<e- kernel #'handle-leave-event pointer win time)))

(defun k-handle-wm-delete-event (kernel window time)
  (check-kernel-mode)
  (let ((win (lookup-server-object kernel window)))
    (<e- kernel #'handle-wm-delete-event win time)))
