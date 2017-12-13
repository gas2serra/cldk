(in-package :cldk-clx)

(defclass clx-server (display-server)
  ())

(defmethod initialize-instance :after ((server clx-server) &rest args)
  (declare (ignore args))
  (with-slots (driver) server
    (setf driver (make-instance 'clx-driver :options (cdr (server-path server)))))
  (start-server server))

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
      (list :clx
	    :host host
	    :display-id (or display 0)
	    :screen-id (or screen 0)
	    :protocol protocol))))

(defun helpfully-automagic-clx-server-path ()
  #+(or windows ccl)
  (parse-clx-server-path '(:clx :host "localhost" :protocol :internet))
  #-(or windows ccl)
  (restart-case (automagic-clx-server-path)
    (use-localhost ()
      :report "Use local unix display"
      (parse-clx-server-path '(:clx :host "" :protocol :unix)))))

(defun parse-clx-server-path (path)
  (pop path)
  (if path
      (list :clx
	    :host       (getf path :host "localhost")
	    :display-id (getf path :display-id 0)
	    :screen-id  (getf path :screen-id 0)
	    :protocol   (getf path :protocol :internet))
      (helpfully-automagic-clx-server-path)))

(setf (get :clx :server-class) 'clx-server)
(setf (get :clx :server-path-parser-fn) 'parse-clx-server-path)