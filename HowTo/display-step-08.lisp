
#|

|#

(load (asdf/pathname:merge-pathnames*
       (asdf/pathname:parse-unix-namestring "HowTo/display-global.lisp")
       (asdf/pathname:pathname-directory-pathname (asdf:system-source-file :cldk))))

#|
(log:info (cldk:map-over-servers #'cldk:server-path))
(cldk:map-over-servers #'cldk:destroy-server)
(cldk:map-over-servers #'cldki::kill-server)
(log:info (bt:all-threads))
|#

(defun set-pixel (data x y red green blue alpha)
  (setf (aref data y x)
       (dpb red (byte 8 0)
            (dpb green (byte 8 8)
                 (dpb blue (byte 8 16)
                      (dpb alpha (byte 8 24) 0))))))

(defun fill-buffer (pixels w h r g b)
  (loop for x from 0 to (1- w) do 
       (loop for y from 0 to (1- h) do
            (set-pixel pixels x y r g b 0))))

(defun fill-tri-buffer (pixels w h r g b)
  (loop for x from 0 to (1- w) do 
       (loop for y from x to (1- h) do
            (set-pixel pixels x y r g b 0))))

(defun fill2-window (w r g b border)  
  (multiple-value-bind (width height)
      (cldk:window-size w)
    (log:info "~A ~A" width height)
    (let ((img (make-instance 'cldki::rgb-image :width width :height height)))
      (fill-buffer (cldk:image-pixels img)
                   (cldk:image-width img)
                   (cldK:image-height img)
                   r g b)
      (fill-tri-buffer (cldk:image-pixels img)
                       (cldk:image-width img)
                       (cldk:image-height img)
                       (round (/ r 2)) (round (/ g 2)) (round (/ b 2)))
      ;; copy
      ;;(cldk:copy-image img 0 0 width height (cldk:buffered-window-image w) 0 0)
      (cldk:copy-image img 0 0 (- width (* 2 border)) height
                       (cldki::window-obuffer w) (+ 0 border) 0)
      ;;(cldk:flush-buffered-window w)
      )))

(defun display-step-08-0 (&optional (backend *backend*))
  (let ((display (cldk:find-display-server :server-path backend)))
    (let ((w (cldk:create-buffered-window display "Pippo" :width 100 :height 100 :mode :managed)))
      (cldk:show-window w)
      (sleep 1)
      (fill2-window w 255 0 0 0)
      (sleep 1)
      (fill2-window w 0 255 0 0)
      (sleep 1)
      (fill2-window w 0 0 255 0)
      (sleep 2)
      (cldk:destroy-window w))))

(defun display-step-08-1 (&optional (backend *backend*))
  (let ((display (cldk:find-display-server :server-path backend)))
    (let ((w (cldk:create-buffered-window display "Pippo" :width 100 :height 100 :mode :managed)))
      (cldk:show-window w)
      (sleep 1)
      (fill2-window w 255 0 0 0)
      (sleep 0.5)
      (fill2-window w 0 255 0 10)
      (sleep 0.5)
      (fill2-window w 0 0 255 20)
      (sleep 3)
      (cldk:destroy-window w))))

