(load (asdf/pathname:merge-pathnames*
       (asdf/pathname:parse-unix-namestring "mcclim-howto/display-global.lisp")
       (asdf/pathname:pathname-directory-pathname (asdf:system-source-file :cldk))))


(defun set-pixel (data x y red green blue alpha)
  (setf (aref data y x)
       (dpb red (byte 8 0)
            (dpb green (byte 8 8)
                 (dpb blue (byte 8 16)
                      (dpb alpha (byte 8 24) 0))))))

(defun fill-buffer (img w h r g b)
  (let ((set-fn (cldk-render:image-rgb-set-fn img)))
    (loop for x from 0 to (1- w) do 
         (loop for y from 0 to (1- h) do
              (funcall set-fn x y r g b)))))

(defun fill-tri-buffer (img w h r g b)
  (let ((set-fn (cldk-render:image-rgb-set-fn img)))
    (loop for x from 0 to (1- w) do 
         (loop for y from x to (1- h) do
              (funcall set-fn x y r g b)))))

(defun fill2-window (w r g b border)  
  (multiple-value-bind (width height)
      (cldk:window-size w)
    (let ((img (cldk-render:make-image w :rgb width height)))
      (fill-buffer img width height r g b)
      (fill-tri-buffer img
                       (cldk-render:image-width img)
                       (cldk-render:image-height img)
                       (round (/ r 2)) (round (/ g 2)) (round (/ b 2)))
      ;; copy
      (cldki::copy-image-to-window img 0 0 width height
                                   w 0 0)
      #+nil (cldk:copy-image img 0 0 (- width (* 2 border)) height
                       (cldki::window-obuffer w) (+ 0 border) 0)
      ;;(cldk:flush-buffered-window w)
      )))

(defun fill3-window (w r g b border)  
  (multiple-value-bind (width height)
      (cldk:window-size w)
    (let ((img (cldk-render:make-image w :rgb width height)))
      (fill-buffer img width height r g b)
      (fill-tri-buffer img
                       (cldk-render:image-width img)
                       (cldk-render:image-height img)
                       (round (/ r 2)) (round (/ g 2)) (round (/ b 2)))
      ;; copy
      (cldk-render:copy-image img 0 0 (- width (* 2 border)) height
                       (cldki::buffered-window-image w) (+ 0 border) 0)
      ;;(cldk:flush-buffered-window w)
      )))


(defun display-step-06-0 (&optional (backend *backend*))
  (let* ((clim:*default-server-path* backend)
         (port (clim:find-port :server-path backend))
         (frame-manager (clim:find-frame-manager :port port))
         (my-app (clim:make-application-frame 'my-app-a)))
    (clim:adopt-frame frame-manager my-app)
    (let* ((top-pane (clim:frame-top-level-sheet my-app))
           (mirror (clim:sheet-mirror top-pane)))
      (log:info "show: ~A" (cldk:show-window mirror))
      (sleep 1)
      (fill2-window mirror 255 0 0 0)
      (sleep 1)
      (fill2-window mirror 0 255 0 0)
      (sleep 1)
      (fill2-window mirror 0 0 255 0)
      (sleep 2)
      (clim:destroy-port port))))

(defun display-step-06-1 (&optional (backend *backend*))
  (let* ((clim:*default-server-path* backend)
         (port (clim:find-port :server-path backend))
         (frame-manager (clim:find-frame-manager :port port))
         (my-app (clim:make-application-frame 'my-app-a)))
    (clim:adopt-frame frame-manager my-app)
    (let* ((top-pane (clim:frame-top-level-sheet my-app))
           (mirror (clim:sheet-mirror top-pane)))
      (log:info "show: ~A" (cldk:show-window mirror))
      (sleep 1)
      (fill3-window mirror 255 0 0 0)
      (sleep 1)
      (fill3-window mirror 0 255 0 0)
      (sleep 1)
      (fill3-window mirror 0 0 255 0)
      (sleep 2)
      (clim:destroy-port port))))
