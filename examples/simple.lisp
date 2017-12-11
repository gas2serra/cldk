(ql:quickload :cldk)
(ql:quickload :cldk-sdl2)
(ql:quickload :cldk-clx)

#|
(log:info (cldk:map-over-servers #'cldk:server-path))
(cldk:map-over-servers #'cldk:destroy-server)
(cldk:map-over-servers #'cldki::kill-server)
(log:info (bt:all-threads))
|#

(defun simple-01 (backend)
  (let ((*s* (cldk:find-server :server-path backend)))
    (let ((num (cldk:screen-num *s*)))
      (format *debug-io* "pixel: ~A~%" (cldk:screen-size *s*))
      (format *debug-io* "mm: ~A~%" (cldk:screen-size *s* nil :millimeters))
      (format *debug-io* "in: ~A~%" (cldk:screen-size *s* nil :inches))
      (format *debug-io* "dpi: ~A~%" (cldk:screen-dpi *s* nil))
      (format *debug-io* "screens: ~A~%" num)
      (loop for si from 0 below num do
           (format *debug-io* "pixel: ~A~%" (cldk:screen-size *s* si))
           (format *debug-io* "mm: ~A~%" (cldk:screen-size *s* si :millimeters))
           (format *debug-io* "in: ~A~%" (cldk:screen-size *s* si :inches))
           (format *debug-io* "dpi: ~A~%" (cldk:screen-dpi *s* si))))))


