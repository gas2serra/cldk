(in-package :cldk-internals)

(defmacro <e- (kernel fn &rest args)
  `(<callback- ,kernel ,fn (event-handler ,kernel) ,@args))

(defun k-handle-window-configuration-event (kernel window x y width height time)
  (let ((kwindow (lookup-server-object kernel window)))
    (when (and (typep kwindow 'buffered-kwindow-mixin)
               (window-buffer kwindow))
      (with-slots (dbuffer dbuffer-width dbuffer-height) kwindow
        (k-flush-buffered-window kwindow)
        (driver-destroy-buffer (driver kwindow) dbuffer)
        (setf dbuffer (driver-create-buffer
                       (driver kwindow)
                       width height)
              dbuffer-width width
              dbuffer-height height)))
    (<e- kernel #'handler-configure-event kwindow x y width height time)))

(defun k-handle-repaint-event (kernel window x y width height time)
  (let ((win (lookup-server-object kernel window)))
    (<e- kernel #'handler-repaint-event win x y width height time)))

(defun k-handle-wheel-event (kernel kind pointer window timestamp)
  (let ((win (lookup-server-object kernel window)))
    (<e- kernel #'handler-wheel-event kind pointer win timestamp)))

(defun k-handle-button-event (kernel kind pointer button window timestamp)
  (let ((win (lookup-server-object kernel window)))
    (<e- kernel #'handler-button-event kind pointer button win timestamp)))

(defun k-handle-motion-event (kernel pointer x y root-x root-y
                             window timestamp)
  (let ((win (lookup-server-object kernel window)))
    (<e- kernel #'handler-motion-event pointer x y root-x root-y
         win timestamp)))

(defun k-handle-key-event (kernel kind keyname character modifiers
                          window timestamp)
  (let ((win (lookup-server-object kernel window)))
    (<e- kernel #'handler-key-event kind keyname character modifiers
         win timestamp)))

(defun k-handle-enter-leave-event (kernel kind pointer window time)
  (let ((win (lookup-server-object kernel window)))
    (<e- kernel #'handler-enter-leave-event kind pointer win time)))

(defun k-handle-destroy-event (kernel  window time)
  (let ((win (lookup-server-object kernel window)))
    (<e- kernel #'handler-destroy-event win time)))

(defun k-handle-wm-delete-event (kernel window time)
  (let ((win (lookup-server-object kernel window)))
    (<e- kernel #'handler-wm-delete-event win time)))
