(in-package :cldk-xcb-backend)

(defclass cldk-xcb-port-mixin (cldk-port-mixin)
  ())

(defmethod port-graft-mirror-class ((port cldk-xcb-port-mixin))
  'cldk-xcb::xcb-root)

(defclass xcb-graft (cldk-graft-mixin root xcb-driver-root kerneled-root-mixin)
  ())
(defclass xcb-root (root xcb-driver-root kerneled-root-mixin)
  ())

(defclass xcb-window (window xcb-driver-window kerneled-window-mixin)
  ())

(defclass xcb-buffered-window (buffered-window xcb-driver-window kerneled-buffered-window-mixin)
  ())

(defmethod port-graft-class ((port cldk-xcb-port-mixin))
  'xcb-graft)



(in-package :clim-fb)

(defclass xcb-fb-port (fb-port cldk-xcb-backend:cldk-xcb-port-mixin)
  ())
  
(setf (get :xcb-cldk :port-type) 'xcb-fb-port)
(setf (get :xcb-cldk :server-path-parser) 'parse-cldk-server-path)

(defclass xcb-fb-mirror (fb-mirror cldk-xcb-backend::xcb-buffered-window)
  ())

(defmethod fb-mirror-class ((port xcb-fb-port))
  'xcb-fb-mirror)
