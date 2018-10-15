(in-package :cldk-clx-backend)

(defclass cldk-clx-port-mixin (cldk-port-mixin)
  ())

(defmethod port-graft-mirror-class ((port cldk-clx-port-mixin))
  'cldk-clx::clx-root)


(defclass clx-graft (cldk-graft-mixin root clx-driver-root kerneled-root-mixin)
  ())

(defmethod port-graft-class ((port cldk-clx-port-mixin))
  'clx-graft)

(defclass clx-window (window clx-driver-window kerneled-window-mixin)
  ())

(defclass clx-root (root clx-driver-root kerneled-root-mixin)
  ())

(defclass clx-buffered-window (buffered-window clx-driver-window kerneled-buffered-window-mixin)
  ())


(in-package :clim-fb)

(defclass clx-fb-port (fb-port cldk-clx-backend:cldk-clx-port-mixin)
  ())
  
(setf (get :clx-cldk :port-type) 'clx-fb-port)
(setf (get :clx-cldk :server-path-parser) 'parse-cldk-server-path)

(defclass clx-fb-mirror (fb-mirror cldk-clx-backend::clx-buffered-window)
  ())

(defmethod fb-mirror-class ((port clx-fb-port))
  'clx-fb-mirror)
