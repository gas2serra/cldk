(ql:quickload :cldk)
(ql:quickload :cldk-sdl2)
(ql:quickload :cldk-clx)

(log:info (cldk:map-over-servers #'cldk:server-path))
(cldk:map-over-servers #'cldk:destroy-server)
(cldk:map-over-servers #'cldki::kill-server)
(log:info (bt:all-threads))

(defparameter *s* nil)
(setf *s* (cldk:find-server))
(setf *s* (cldk:find-server :server-path '(:null)))
(setf *s* (cldk:find-server :server-path '(:clx)))
(setf *s* (cldk:find-server :server-path '(:sdl2)))

(let ((r (cldk:screen-size *s*)))
  (log:info r))

(let ((r (cldk:screen-dpi *s*)))
  (log:info r))

(let ((r (cldk:screen-pointer-position *s*)))
  (log:info r))
