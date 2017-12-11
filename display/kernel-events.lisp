(in-package :cldk-internals)

(defmacro <e+ (kernel fn &rest args)
  `(apply ,fn ,kernel (list ,@args)))

(defun khandle-window-configuration-event (kernel kwindow x y width height time)
  (when (and (typep kwindow 'buffered-kwindow-mixin)
             (window-buffer kwindow))
    (with-slots (dbuffer dbuffer-width dbuffer-height) kwindow
      (flush-buffered-kwindow kwindow)
      (driver-destroy-buffer (driver kwindow) dbuffer)
      (setf dbuffer (driver-create-buffer
                     (server-driver (window-server kwindow))
                     width height)
            dbuffer-width width
            dbuffer-height height)))
  (<e- kernel #'handler-configure-event kwindow x y width height time))

(defun khandle-repaint-event (kernel win x y width height time)
  (<e- kernel #'handler-repaint-event win x y width height time))

(defun khandle-wheel-event (kernel kind pointer window timestamp)
  (<e- kernel #'handler-wheel-event kind pointer window timestamp))

(defun khandle-button-event (kernel kind pointer button window timestamp)
  (<e- kernel #'handler-button-event kind pointer button window timestamp))

(defun khandle-motion-event (kernel pointer x y root-x root-y
                             window timestamp)
  (<e- kernel #'handler-motion-event pointer x y root-x root-y
                        window timestamp))

(defun khandle-key-event (kernel kind keyname character modifiers
                          window timestamp)
  (<e- kernel #'handler-key-event kind keyname character modifiers
                     window timestamp))

(defun khandle-enter-leave-event (kernel kind pointer win time)
  (<e- kernel #'handler-enter-leave-event kind pointer win time))

(defun khandle-destroy-event (kernel  win time)
  (<e- kernel #'handler-destroy-event win time))

(defun khandle-wm-delete-event (kernel win time)
  (<e- kernel #'handler-wm-delete-event win time))
