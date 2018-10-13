
(load (asdf/pathname:merge-pathnames*
       (asdf/pathname:parse-unix-namestring "HowTo/display-global.lisp")
       (asdf/pathname:pathname-directory-pathname (asdf:system-source-file :cldk))))

(defun display-step-00-0 (&optional (backend *backend*))
  (log:info (cldk:map-over-servers #'cldk:server-path))
  (let ((display (cldk:find-display-server :server-path backend)))
      (log:info (cldk:map-over-servers #'cldk:server-path))
      (cldk:destroy-server display))
  (log:info (cldk:map-over-servers #'cldk:server-path)))

(defun display-step-00-1 (&optional (backend *backend*))
  (log:info (bt:all-threads))
  (let ((display (cldk:find-display-server :server-path backend)))
      (log:info (bt:all-threads))
      (cldk:destroy-server display))
  (sleep 1)
  (log:info (bt:all-threads)))

(defun display-step-00-2 (&optional (backend *backend*))
  (let ((display (cldk:find-display-server :server-path backend)))
    (format *debug-io* "options: ~S~%" (cldk-driver:driver-options display))
    (format *debug-io* "id: ~S~%" (cldk-driver:driver-id display))      
    (cldk:destroy-server display)))
