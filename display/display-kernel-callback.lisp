(in-package :cldk-internals)

(defmacro <e- (kernel fn &rest args)
  `(<callback- ,kernel ,fn (event-handler ,kernel) ,@args))

(defun k-handle-window-configuration-event (kernel window x y width height time)
  (check-kernel-mode)
  (let ((kwindow (lookup-server-object kernel window)))
    (when (and (typep kwindow 'k-buffered-window-mixin)
               (window-buffer kwindow))
      (with-slots (dbuffer dbuffer-width dbuffer-height image obuffer) kwindow
        (k-flush-buffered-window kwindow)
        (with-slots (pixels-lock) (k-buffered-window-image kwindow)
          (bt:with-lock-held (pixels-lock)
            (driver-update-buffer (driver kwindow) dbuffer width height)
            (setf dbuffer-width width
                  dbuffer-height height)
            (driver-update-image (driver kwindow) image dbuffer)
            (k-update-buffer obuffer width height)
            ))))
    (<e- kernel #'handle-configure-event kwindow x y width height time)))

(defun k-handle-repaint-event (kernel window x y width height time)
  (check-kernel-mode)
  (let ((win (lookup-server-object kernel window)))
    (<e- kernel #'handle-repaint-event win x y width height time)))

(defun k-handle-wheel-event (kernel kind pointer window timestamp)
  (check-kernel-mode)
  (let ((win (lookup-server-object kernel window)))
    (<e- kernel #'handle-wheel-event kind pointer win timestamp)))

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

(defun k-handle-enter-leave-event (kernel kind pointer window time)
  (check-kernel-mode)
  (let ((win (lookup-server-object kernel window)))
    (<e- kernel #'handle-enter-leave-event kind pointer win time)))

(defun k-handle-destroy-event (kernel  window time)
  (check-kernel-mode)
  (let ((win (lookup-server-object kernel window)))
    (<e- kernel #'handle-destroy-event win time)))

(defun k-handle-wm-delete-event (kernel window time)
  (check-kernel-mode)
  (let ((win (lookup-server-object kernel window)))
    (<e- kernel #'handle-wm-delete-event win time)))
