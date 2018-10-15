(in-package :cldk-sdl2-backend)

(defclass cldk-sdl2-port-mixin (cldk-port-mixin)
  ())

(defmethod port-graft-mirror-class ((port cldk-sdl2-port-mixin))
  'cldk-sdl2::sdl2-root)

(defclass sdl2-graft (cldk-graft-mixin root sdl2-driver-root kerneled-root-mixin)
  ())

(defmethod port-graft-class ((port cldk-sdl2-port-mixin))
  'sdl2-graft)

(defclass sdl2-root (root sdl2-driver-root kerneled-root-mixin)
  ())

(defclass sdl2-window (window sdl2-driver-window kerneled-window-mixin)
  ())

(defclass sdl2-buffered-window (buffered-window sdl2-driver-window kerneled-buffered-window-mixin)
  ())





(in-package :clim-fb)

(defclass sdl2-fb-port (fb-port cldk-sdl2-backend:cldk-sdl2-port-mixin)
  ())
  
(setf (get :sdl2-cldk :port-type) 'sdl2-fb-port)
(setf (get :sdl2-cldk :server-path-parser) 'parse-cldk-server-path)

(defclass sdl2-fb-mirror (fb-mirror cldk-sdl2-backend::sdl2-buffered-window)
  ())

(defmethod fb-mirror-class ((port sdl2-fb-port))
  'sdl2-fb-mirror)
