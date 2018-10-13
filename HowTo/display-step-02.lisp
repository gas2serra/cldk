
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


(defun display-step-02-0 (&optional (backend *backend*))
  (let ((display (cldk:find-display-server :server-path backend)))
    (let ((w (cldk:create-window display "Pippo" :mode :managed)))
      (cldk:show-window w)
      (sleep 2)
      (cldk:hide-window w)
      (sleep 1)
      (cldk:destroy-window w))))


(defun display-step-02-1 (&optional (backend *backend*))
  (let ((display (cldk:find-display-server :server-path backend)))
    (let ((w (cldk:create-window display "Pippo" :mode :managed)))
      (cldk:show-window w)
      (format *debug-io* "Size: ~A~%" (multiple-value-list (cldk:window-size w)))
      (format *debug-io* "Pos: ~A~%" (multiple-value-list (cldk:window-position w)))
      (sleep 3)
      (cldk:destroy-window w))))

(defun display-step-02-2 (&optional (backend *backend*))
  (let ((display (cldk:find-display-server :server-path backend)))
    (let ((w (cldk:create-window display "Pippo" :mode :managed)))
      (cldk:show-window w)
      (loop for i from 0 below 10 do
           (format *debug-io* "Size: ~A~%" (multiple-value-list (cldk:window-size w)))
           (format *debug-io* "Pos: ~A~%" (multiple-value-list (cldk:window-position w)))
           (sleep 1))
      (sleep 2)
      (cldk:destroy-window w))))
