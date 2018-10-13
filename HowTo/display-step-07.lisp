
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


(defun display-step-07-0 (&optional (backend *backend*))
  (let ((display (cldk:find-display-server :server-path backend)))
    (let ((w (cldk:create-window display "Pippo" :mode :managed)))
      (cldk:show-window w)
      (sleep 2)
      (cldk:set-window-cursor w :busy)
      (sleep 1)
      (cldk:set-window-cursor w :i-beam)
      (sleep 1)
      (cldk:set-window-cursor w :default)
      (sleep 2)
      (cldk:destroy-window w))))

(defun display-step-07-1 (&optional (backend *backend*))
  (let ((display (cldk:find-display-server :server-path backend)))
    (let ((w (cldk:create-window display "Pippo" :mode :managed)))
      (cldk:show-window w)
      (sleep 2)
      (let ((cursors (cldk:avaiable-cursor-names display)))
        (format *debug-io* "~A~%" cursors)
        (loop for cur in cursors do
             (format *debug-io* "~A~%" cur)
             (cldk:set-window-cursor w cur)
             (sleep 1)))
      (sleep 2)
      (cldk:destroy-window w))))
