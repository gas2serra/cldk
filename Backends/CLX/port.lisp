(in-package :cldk-clx-backend)

(defclass cldk-clx-port-mixin (cldk-port-mixin
                               cldk-backend::multi-thread-display-server
                               clx-driver)

  ())


;;;
;;;
;;;


(defmethod initialize-instance :after ((server cldk-clx-port-mixin) &rest args)
  (declare (ignore args))
  (setf (driver-options server) (cons :id (clim:port-server-path server)))
  (cldk-server:start-server server)
  (setf (cldk:server-event-handler server)
        (make-instance 'clim-fb::fb-event-handler :port server)))

;;;
;;; server-path parser
;;;

(defun automagic-clx-server-path ()  
  (let ((name (uiop:getenv "DISPLAY")))
    (assert name (name)
            "Environment variable DISPLAY is not set")
    (let* (; this code courtesy telent-clx.
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
      (list :cldk-fb-clx
	    :host host
	    :display-id (or display 0)
	    :screen-id (or screen 0)
	    :protocol protocol))))

(defun helpfully-automagic-clx-server-path ()
  #+(or windows ccl)
  (parse-clx-server-path '(:cldk-fb-clx :host "localhost" :protocol :internet))
  #-(or windows ccl)
  (restart-case (automagic-clx-server-path)
    (use-localhost ()
      :report "Use local unix display"
      (parse-clx-server-path '(:cldk-fb-clx :host "" :protocol :unix)))))

(defun parse-clx-server-path (path)
  (pop path)
  (if path
      (list :cldk-fb-clx
	    :host       (getf path :host "localhost")
	    :display-id (getf path :display-id 0)
	    :screen-id  (getf path :screen-id 0)
	    :protocol   (getf path :protocol :internet))
      (helpfully-automagic-clx-server-path)))


(setf (get :clx :server-path-parser-fn) 'parse-clx-server-path)



;;;
;;;
;;;

(defclass clx-window (window clx-driver-window kerneled-window-mixin)
  ())

(defclass clx-root (root clx-driver-root kerneled-root-mixin)
  ())

(defclass clx-buffered-window (buffered-window clx-driver-window kerneled-buffered-window-mixin)
  ())

(defmethod port-graft-mirror-class ((port cldk-clx-port-mixin))
  'clx-root)


(defclass clx-graft (cldk-graft-mixin root clx-driver-root kerneled-root-mixin)
  ())

(defmethod port-graft-class ((port cldk-clx-port-mixin))
  'clx-graft)




(in-package :clim-fb)

(defclass clx-fb-port (fb-port cldk-clx-backend:cldk-clx-port-mixin)
  ())
  
(setf (get :clx-cldk :port-type) 'clx-fb-port)
(setf (get :clx-cldk :server-path-parser) 'parse-cldk-server-path)

(defclass clx-fb-mirror (fb-mirror cldk-clx-backend::clx-buffered-window)
  ())

(defmethod fb-mirror-class ((port clx-fb-port))
  'clx-fb-mirror)

;;(setf (get :clx :server-class) 'clx-fb-port)
(setf (get :cldk-fb-clx :port-type) 'clx-fb-port)
(setf (get :cldk-fb-clx :server-path-parser) 'cldk-clx-backend::parse-clx-server-path)

(defmethod cldk:create-buffer ((server clx-fb-port) width height)
  (make-instance 'cldk-clx-backend::clx-buffer :driver server :width width :height height))
