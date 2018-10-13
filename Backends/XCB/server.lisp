(in-package :cldk-xcb)

(defclass xcb-server (single-thread-display-server xcb-driver)
  ())

(defmethod initialize-instance :after ((server xcb-server) &rest args)
  (declare (ignore args))
  (start-server server))

;;;
;;; server-path parser
;;;

(defun parse-xcb-server-path (path)
  path)

(setf (get :xcb :server-class) 'xcb-server)
(setf (get :xcb :server-path-parser-fn) 'parse-xcb-server-path)



(defclass xcb-window (window xcb-driver-window kerneled-window-mixin)
  ())

(defclass xcb-buffered-window (buffered-window xcb-driver-window kerneled-buffered-window-mixin)
  ())

(defmethod create-window ((server xcb-server) name &key (pretty-name name) (x nil) (y nil)
                                                       (width 300) (height 300)
                                                       (mode :managed) (window-class 'window))
  (declare (ignore window-class))
  (call-next-method server name :pretty-name pretty-name :x x :y y
                    :width width :height height
                    :mode mode :window-class 'xcb-window))

(defmethod create-buffered-window ((server xcb-server) name &key (pretty-name name) (x nil) (y nil)
                                                              (width 300) (height 300)
                                                              (mode :managed) (window-class nil))
  (call-next-method server name :pretty-name pretty-name :x x :y y
                    :width width :height height
                    :mode mode :window-class (or window-class 'xcb-buffered-window)))
