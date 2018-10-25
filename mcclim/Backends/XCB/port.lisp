(in-package :cldk-xcb-backend)

(defclass cldk-xcb-port-mixin (cldk-port-mixin
                               cldk-backend::multi-thread-display-server
                               xcb-driver)
  ())

(defmethod port-graft-mirror-class ((port cldk-xcb-port-mixin))
  'xcb-root)

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

(setf (get :cldk-fb-xcb :port-type) 'xcb-fb-port)
(setf (get :cldk-fb-xcb :server-path-parser) 'cldk-xcb-backend::parse-xcb-server-path)

(defmethod cldk:create-buffer ((port xcb-fb-port) width height)
  (make-instance 'cldk-xcb-backend::xcb-buffer :driver port :width width :height height))

;;;
;;;
;;;
(in-package :cldk-xcb-backend)

(defun automagic-xcb-server-path ()  
  (let ((name (uiop:getenv "DISPLAY")))
    (assert name (name)
            "Environment variable DISPLAY is not set")
    (let* (; this code courtesy telent-xcb.
           (slash-i (or (position #\/ name) -1))
           (colon-i (position #\: name :start (1+ slash-i)))
           (decnet-colon-p (eql (elt name (1+ colon-i)) #\:))
           (host (subseq name (1+ slash-i) colon-i))
           (dot-i (and colon-i (position #\. name :start colon-i)))
           (display (and colon-i
                      (parse-integer name
                                     :start (if decnet-colon-p
                                                (+ colon-i 2)
                                                (1+ colon-i))
                                     :end dot-i)))
           (screen (and dot-i
                     (parse-integer name :start (1+ dot-i))))
           (protocol
            (cond ((or (string= host "") (string-equal host "unix")) :local)
                  (decnet-colon-p :decnet)
                  ((> slash-i -1) (intern
                                   (string-upcase (subseq name 0 slash-i))
                                   :keyword))
                  (t :internet))))
      (list :cldk-fb-xcb
	    :host host
	    :display-id (or display 0)
	    :screen-id (or screen 0)
	    :protocol protocol))))

(defun helpfully-automagic-xcb-server-path ()
  #+(or windows ccl)
  (parse-xcb-server-path '(:cldk-fb-xcb :host "localhost" :protocol :internet))
  #-(or windows ccl)
  (restart-case (automagic-xcb-server-path)
    (use-localhost ()
      :report "Use local unix display"
      (parse-xcb-server-path '(:cldk-fb-xcb :host "" :protocol :unix)))))

(defun parse-xcb-server-path (path)
  (pop path)
  (if path
      (list :cldk-fb-xcb
	    :host       (getf path :host "localhost")
	    :display-id (getf path :display-id 0)
	    :screen-id  (getf path :screen-id 0)
	    :protocol   (getf path :protocol :internet))
      (helpfully-automagic-xcb-server-path)))
