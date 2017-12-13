(ql:quickload :cldk)
(ql:quickload :cldk-sdl2)
(ql:quickload :cldk-clx)

#|
(log:info (cldk:map-over-servers #'cldk:server-path))
(cldk:map-over-servers #'cldk:destroy-server)
(cldk:map-over-servers #'cldki::kill-server)
(log:info (bt:all-threads))
|#

(defun simple-ev-window (backend &optional (sec 10))
  (let ((*s* (cldk:find-display-server :server-path backend)))
    (setf (cldk:server-event-handler *s*)
                                     (make-instance 'cldk:event-handler))
    (let ((w (cldk:create-window *s* "Pippo" :mode :managed)))
      (cldk:show-window w)
      (sleep sec)
      (cldk:destroy-window w))))
