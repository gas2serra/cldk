
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


(defun display-step-03-0 (&optional (backend *backend*))
  (let ((display (cldk:find-display-server :server-path backend)))
    (let ((w1 (cldk:create-window display "Pippo" :mode :managed))
          (w2 (cldk:create-window display "Pippo" :mode :managed)))
      (cldk:show-window w1)
      (cldk:show-window w2)
      (sleep 3)
      (cldk:bury-window w2)
      (sleep 3) 
      (cldk:raise-window w2)     
      (sleep 3)
      (cldk:bury-window w2)     
      (sleep 3)
      (cldk:destroy-window w1)
      (cldk:destroy-window w2))))
