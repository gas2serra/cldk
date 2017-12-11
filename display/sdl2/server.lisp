(in-package :cldk-sdl2)

(defclass sdl2-server (display-server)
  ())

(defmethod initialize-instance :after ((server sdl2-server) &rest args)
  (declare (ignore args))
  (with-slots (driver) server
    (setf driver (make-instance 'sdl2-driver :options (cdr (server-path server)))))
  (start-server server))

;;;
;;; server-path parser
;;;

(defun parse-sdl2-server-path (path)
  (pop path)
  (if path
      (list :sdl2
	    :screen-id  (getf path :screen-id 0))
      (list :sdl2
	    :screen-id  0)))

(setf (get :sdl2 :server-class) 'sdl2-server)
(setf (get :sdl2 :server-path-parser-fn) 'parse-sdl2-server-path)
