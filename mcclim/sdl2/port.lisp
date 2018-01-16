(in-package :clim-fb)

(defclass sdl2-fb-port (fb-port)
  ())
  
(setf (get :sdl2-cldk :port-type) 'sdl2-fb-port)
(setf (get :sdl2-cldk :server-path-parser) 'parse-cldk-server-path)

(defclass sdl2-fb-mirror (fb-mirror)
  ())

(defmethod fb-mirror-class ((port sdl2-fb-port))
  'sdl2-fb-mirror)

