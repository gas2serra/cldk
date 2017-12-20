(in-package :cldk-internals)

(defclass buffered-window (window k-buffered-window-mixin)
  ((buffer-lock :initform (bt:make-lock "buffer"))))

(defmethod initialize-instance :after ((win buffered-window) &key width height &allow-other-keys)
  (<kwindow+ win #'k-initialize-buffered-window width height))

(defgeneric create-buffered-window (server name &key pretty-name x y
                                                  width height mode
                                                  window-class)
  (:method ((server display-server) name &key (pretty-name name) (x 0) (y 0)
                                   (width 300) (height 300)
                                   (mode :managed) (window-class 'buffered-window))
    (make-instance window-class :server server :kernel server
                   :obuffer (create-buffer server width height)
                   :name name :pretty-name pretty-name
                   :x x :y y
                   :width width :height height :mode mode)))

(defmethod destroy-window ((window buffered-window))
  (<kwindow- window #'k-destroy-buffered-window)
  (destroy-buffer (window-obuffer window))
  (call-next-method))



                        



