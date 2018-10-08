(in-package :cldk-internals)

(defmethod driver-cb-window-configuration-event :before ((handler default-display-callback-handler)
                                                         kernel (win k-buffered-window-mixin)
                                                         x y width height time)
  (check-kernel-mode)
  (k-notify-resize-buffered-window win width height))
