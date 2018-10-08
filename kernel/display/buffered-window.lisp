(in-package :cldk-kernel)

(defclass kerneled-buffered-window-mixin (kerneled-window-mixin)
  ((obuffer :initform nil
            :initarg :obuffer
            :accessor window-obuffer)))

(defmethod initialize-buffered-window ((win kerneled-window-mixin) width height)
  (within-kernel-mode ((driver win) :block-p t)
    (driver-initialize-buffer (driver win) (window-obuffer win) width height)))

(defmethod destroy-window :before ((window kerneled-window-mixin))
  (destroy-buffer (window-obuffer window)))
