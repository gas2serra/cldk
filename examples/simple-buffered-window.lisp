(ql:quickload :cldk)
(ql:quickload :cldk-sdl2)
(ql:quickload :cldk-clx)

#|
(log:info (cldk:map-over-servers #'cldk:server-path))
(cldk:map-over-servers #'cldk:destroy-server)
(cldk:map-over-servers #'cldki::kill-server)
(log:info (bt:all-threads))
|#

(defun set-pixel (data x y red green blue alpha)
  (setf (aref data y x)
       (dpb blue (byte 8 0)
            (dpb green (byte 8 8)
                 (dpb red (byte 8 16)
                      (dpb alpha (byte 8 24) 0))))))

(defun fill-bueffer (pixels w h r g b)
  (loop for x from 0 to (1- w) do 
       (loop for y from 0 to (1- h) do
            (set-pixel pixels x y r g b 0))))

(defun fill-tri-buffer (pixels w h r g b)
  (loop for x from 0 to (1- w) do 
       (loop for y from x to (1- h) do
            (set-pixel pixels x y r g b 0))))

(defun fill-window (w r g b)  
  (multiple-value-bind (width height)
      (cldk:window-size w)
    (let ((img (make-instance 'cldk:image :width width :height height)))
      (fill-buffer (cldk:image-pixels img)
                   (cldk:image-width img)
                   (cldk:image-height img)
                   r g b)
      (fill-tri-buffer (cldk:image-pixels img)
                       (cldk:image-width img)
                       (cldk:image-height img)
                       (round (/ r 2)) (round (/ g 2)) (round (/ b 2)))
      ;; copy
      (cldk:copy-image-to-buffered-window w img 0 0 width height)
      ;;(cldk:flush-buffered-window w)
      )))

(defun simple-buffered-window-01 (backend)
  (let ((*s* (cldk:find-display-server :server-path backend)))
    (let ((w (cldk:create-buffered-window *s* "Pippo" :mode :managed)))
      (cldk:show-window w)
      (fill-window w 255 0 0)
      (sleep 1)
      (fill-window w 0 255 0)
      (sleep 1)
      (fill-window w 0 0 255)
      (sleep 1)
      (cldk:destroy-window w))))
