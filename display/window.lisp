(in-package :cldk-internals)

(defclass window (k-window-mixin kerneled-window-mixin)
  ())

(defgeneric create-window (server name &key pretty-name x y width height
                                         mode window-class)
  (:method ((server display-server) name &key (pretty-name name) (x nil) (y nil)
                                   (width 300) (height 300)
                                   (mode :managed) (window-class 'window))
    (make-instance window-class :server server :kernel server
                   :name name :pretty-name pretty-name
                   :x x :y y :width width :height height :mode mode)))

