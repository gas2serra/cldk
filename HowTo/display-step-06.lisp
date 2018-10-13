
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


(defun display-step-06-0 (&optional (backend *backend*))
  (let ((display (cldk:find-display-server :server-path backend)))
    (let ((w (cldk:create-window display "Pippo" :mode :managed)))
      (cldk:show-window w)
      (sleep 2)
      (format *debug-io* "mouse pos: ~A~%" (multiple-value-list (cldk:window-pointer-position w)))
      (sleep 3)
      (cldk:destroy-window w))))

(defun display-step-06-1 (&optional (backend *backend*))
  (let ((display (cldk:find-display-server :server-path backend)))
    (let ((w (cldk:create-window display "Pippo" :mode :managed)))
      (cldk:show-window w)
      (sleep 5)
      (format *debug-io* "Pos: ~A~%" (multiple-value-list (cldk:window-position w)))
      (format *debug-io* "mouse pos: ~A~%" (multiple-value-list (cldk:window-pointer-position w)))
      (format *debug-io* "mouse global pos: ~A~%"
              (multiple-value-list (cldk:screen-pointer-position display)))
      (format *debug-io* "==>~%")
      (format *debug-io* "mouse pos: ~A~%"
              (multiple-value-list (cldk:window-pointer-position w)))
      (sleep 3)
      (cldk:destroy-window w))))
