(in-package :cldk-driver-clx)

(defclass clx-display (clx-driver
                       cldki::kerneled-display-mixin
                       multi-threaded-driver-mixin
                       driver-with-thread-mixin)
  ())

(defmethod cldki::driver-loop-step ((server clx-display))
  (driver-process-next-events server)
  (unless (or (cldki::driver-stopping-p server)
              (cldki::driver-stopped-p server))
    (refresh-windows server)
    (driver-force-output server)))

(defclass clx-window (window clx-driver-window kerneled-window-mixin)
  ())

(defclass clx-root (root clx-driver-root kerneled-root-mixin)
  ())

(defclass clx-buffered-window (buffered-window clx-driver-window kerneled-buffered-window-mixin)
  ())

(defmethod cldki::create-display ((id (eql :cldk-clx)) options)
  (let ((server (make-instance 'clx-display :options (append '(:id :cldk-clx) options))))
    (start-driver server)
    server))

(defmethod create-window ((display clx-display) name
                          &key (pretty-name name) (x nil) (y nil)
                            (width 300) (height 300)
                            (mode :managed) (window-class 'window))
  (declare (ignore window-class))
  (make-instance 'clx-window :driver display
                 :name name :pretty-name pretty-name
                 :x x :y y :width width :height height :mode mode))
  
