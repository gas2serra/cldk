(in-package :clim-fb)

(defclass clx-fb-port (fb-port)
  ())
  
(setf (get :clx-cldk :port-type) 'clx-fb-port)
(setf (get :clx-cldk :server-path-parser) 'parse-cldk-server-path)

(defclass clx-fb-mirror (fb-mirror cldk-clx::clx-buffered-window)
  ())

(defmethod fb-mirror-class ((port clx-fb-port))
  'clx-fb-mirror)

