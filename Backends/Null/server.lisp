(in-package :cldk-null)

(defclass null-server (single-thread-display-server null-driver)
  ())

(defmethod initialize-instance :after ((server null-server) &rest args)
  (declare (ignore args))
  (start-server server))

;;;
;;; server-path parser
;;;

(defun parse-null-server-path (path)
  path)

(setf (get :null :server-class) 'null-server)
(setf (get :null :server-path-parser-fn) 'parse-null-server-path)

;;;
;;;

(defclass null-window (window null-driver-window kerneled-window-mixin)
  ())

(defclass null-buffered-window (buffered-window null-driver-window kerneled-buffered-window-mixin)
  ())

(defmethod create-window ((server null-server) name &key (pretty-name name) (x nil) (y nil)
                                                       (width 300) (height 300)
                                                       (mode :managed) (window-class 'window))
  (declare (ignore window-class))
  (call-next-method server name :pretty-name pretty-name :x x :y y
                    :width width :height height
                    :mode mode :window-class 'null-window))

(defmethod create-buffered-window ((server null-server) name &key (pretty-name name) (x nil) (y nil)
                                                              (width 300) (height 300)
                                                              (mode :managed) (window-class nil))
  (call-next-method server name :pretty-name pretty-name :x x :y y
                    :width width :height height
                    :mode mode :window-class (or window-class 'null-buffered-window)))
