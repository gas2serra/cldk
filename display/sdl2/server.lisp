(in-package :cldk-sdl2)

(defclass sdl2-server (single-thread-display-server sdl2-driver)
  ())

(defmethod initialize-instance :after ((server sdl2-server) &rest args)
  (declare (ignore args))
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

(defclass sdl2-window (window sdl2-driver-window)
  ())

(defclass sdl2-buffered-window (buffered-window sdl2-driver-window)
  ())

(defmethod create-window ((server sdl2-server) name &key (pretty-name name) (x nil) (y nil)
                                                      (width 300) (height 300)
                                                      (mode :managed) (window-class 'window))
  (declare (ignore window-class))
  (call-next-method server name :pretty-name pretty-name :x x :y y
                    :width width :height height
                    :mode mode :window-class 'sdl2-window))

(defmethod create-buffered-window ((server sdl2-server) name &key (pretty-name name) (x nil) (y nil)
                                                               (width 300) (height 300)
                                                               (mode :managed) (window-class 'window))
  (declare (ignore window-class))
  (call-next-method server name :pretty-name pretty-name :x x :y y
                    :width width :height height
                    :mode mode :window-class (or window-class 'sdl2-buffered-window)))
