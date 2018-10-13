
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


(defun display-step-01-0 (&optional (backend *backend*))
  (let ((display (cldk:find-display-server :server-path backend)))
    (format *debug-io* "size pixel: ~A~%"
            (multiple-value-list (cldk:screen-size display)))
    (format *debug-io* "size mm: ~A~%"
            (multiple-value-list (cldk:screen-size display nil :millimeters)))
    (format *debug-io* "size in: ~A~%"
            (multiple-value-list (cldk:screen-size display nil :inches)))
    (format *debug-io* "dpi: ~A~%"
            (multiple-value-list (cldk:screen-dpi display nil)))
    (cldk:destroy-server display)))

(defun display-step-01-1 (&optional (backend *backend*))
  (let ((display (cldk:find-display-server :server-path backend)))
    (let ((num (cldk:screen-num display)))
      (format *debug-io* "screens: ~A~%" num)
      (loop for si from 0 below num do
           (format *debug-io* "pixel: ~A~%"
                   (multiple-value-list (cldk:screen-size display si)))
           (format *debug-io* "mm: ~A~%"
                   (multiple-value-list (cldk:screen-size display si :millimeters)))
           (format *debug-io* "in: ~A~%"
                   (multiple-value-list (cldk:screen-size display si :inches)))
           (format *debug-io* "dpi: ~A~%"
                   (multiple-value-list (cldk:screen-dpi display si)))))
    (cldk:destroy-server display)))

(defun display-step-01-2 (&optional (backend *backend*))
  (let ((display (cldk:find-display-server :server-path backend)))
    (loop for i from 0 below 10 do
         (format *debug-io* "pointer position: ~A~%"
                 (multiple-value-list (cldk:screen-pointer-position display)))
         (sleep 0.5))
    (cldk:destroy-server display)))

(defun display-step-01-3 (&optional (backend *backend*))
  (let ((display (cldk:find-display-server :server-path backend)))
    (loop for i from 0 below 10 do
         (format *debug-io* "screen size: ~A~%"
                 (multiple-value-list (cldk:screen-size display)))
         (sleep 2))
    (cldk:destroy-server display)))
