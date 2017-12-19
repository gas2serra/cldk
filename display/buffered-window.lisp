(in-package :cldk-internals)

(defclass buffered-window (window k-buffered-window-mixin)
  ((buffer-lock :initform (bt:make-lock "buffer"))))

(defmethod initialize-instance :after ((win buffered-window) &key width height &allow-other-keys)
  (<kwindow+ win #'k-initialize-buffered-window width height))

(defgeneric create-buffered-window (server name &key pretty-name x y
                                                  width height mode
                                                  window-class)
  (:method ((server display-server) name &key (pretty-name name) (x 0) (y 0)
                                   (width 300) (height 300)
                                   (mode :managed) (window-class 'buffered-window))
    (make-instance window-class :server server :kernel server
                   :obuffer (create-buffer server width height)
                   :name name :pretty-name pretty-name
                   :x x :y y
                   :width width :height height :mode mode)))

(defmethod destroy-window ((window buffered-window))
  (<kwindow- window #'k-destroy-buffered-window)
  (destroy-buffer (window-obuffer window))
  (call-next-method))
#|
(defgeneric copy-image-to-buffered-window (window image x y width height)
  (:method ((window buffered-window) image x y width height)
    (with-slots (buffer-lock) window
      (bt:with-lock-held (buffer-lock)
        (<kwindow+ window
                   #'k-copy-image-to-buffered-window image 
                   x y
                   width height)))))

(defgeneric copy-image-to-buffered-window* (window image rectangle-set)
  (:method ((window buffered-window) image rectangle-set)
    (with-slots (buffer-lock) window
      (bt:with-lock-held (buffer-lock)
        (<kwindow+ window
                   #'k-copy-image-to-buffered-window* image 
                   rectangle-set)))))
|#

(defgeneric buffered-window-image (window)
  (:method ((window buffered-window))
    #+nil (<kwindow+ window
                     #'k-buffered-window-image)
    (k-buffered-window-image window)))


                        



