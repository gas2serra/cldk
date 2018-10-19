(in-package :cldk-sdl2-backend)

(defclass cldk-sdl2-port-mixin (cldk-port-mixin
                                cldk-backend::single-thread-display-server
                                sdl2-driver)
  ())

(defmethod port-graft-mirror-class ((port cldk-sdl2-port-mixin))
  'sdl2-root)

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

;;;
;;;
;;;
(defun parse-sdl2-server-path (path)
  (pop path)
  (if path
      (list :cldk-fb-sdl2
	    :screen-id  (getf path :screen-id 0))
      (list :cldk-fb-sdl2
	    :screen-id  0)))

(setf (get :sdl2 :server-class) 'sdl2-server)
(setf (get :sdl2 :server-path-parser-fn) 'parse-sdl2-server-path)


(setf (get :cldk-fb-sdl2 :port-type) 'sdl2-fb-port)
(setf (get :cldk-fb-sdl2 :server-path-parser) 'parse-sdl2-server-path)

(defmethod cldk:create-buffer ((port sdl2-fb-port) width height)
  (make-instance 'cldk-sdl2-backend::sdl2-buffer :driver port :width width :height height))
