(ql:quickload :mcclim-cldk)
(ql:quickload :cldk-clx)
(ql:quickload :cldk-sdl2)
(ql:quickload :clim-examples)

#|
(log:config '(cldki) :trace :daily "/tmp/cldk.log")
|#

#|
(log:info (cldk:map-over-servers #'cldk:server-path))
(cldk:map-over-servers #'cldk:destroy-server)
(clim:map-over-ports #'clim:destroy-port)
(cldk:map-over-servers #'cldki::kill-server)
(log:info (bt:all-threads))
|#

(defun demo (backend)
  (let ((clim:*default-server-path*  (list :cldk :cldk-driver backend)))
    (clim-demo:demodemo)))

