(ql:quickload :cldk)
(ql:quickload :cldk-sdl2)
(ql:quickload :cldk-clx)

#|
(log:info (cldk:map-over-servers #'cldk:server-path))
(cldk:map-over-servers #'cldk:destroy-server)
(cldk:map-over-servers #'cldki::kill-server)
(log:info (bt:all-threads))

cldk:*events-to-log-list*

|#

(defvar *s*)

(defun simple-ev-window (backend &optional (sec 10))
  (let ((*s* (cldk:find-display-server :server-path backend)))
    (setf (cldk:server-event-handler *s*)
          (make-instance 'cldk:log-event-handler))

    (setf (cldk:event-handler-events-to-log (cldk:server-event-handler *s*)) '(:key :modifiers))

    (let ((w (cldk:create-window *s* "Pippo" :mode :managed)))
      (cldk:show-window w)
      ;;(sleep 1)
      ;;(cldk:set-window-size w 500 500)
      ;;(sleep 1)
      ;;(cldk:set-window-position w 500 500)
      (sleep sec)
      (cldk:destroy-window w))))
