
(load (asdf/pathname:merge-pathnames*
       (asdf/pathname:parse-unix-namestring "mcclim-howto/display-global.lisp")
       (asdf/pathname:pathname-directory-pathname (asdf:system-source-file :cldk))))
  
(defun display-step-00-0 (&optional (backend *backend*))
  (log-ports)
  (let ((port (clim:find-port :server-path backend)))
    (log-ports)
    (clim:destroy-port port))
  (log-ports))

(defun display-step-00-1 (&optional (backend *backend*))
  (log-port-threads)
  (let ((port (clim:find-port :server-path backend)))
    (log-port-threads)
    (clim:destroy-port port))
  (sleep 2)
  (log-port-threads))

(defun display-step-00-2 (&optional (backend *backend*))
  (let* ((port (clim:find-port :server-path backend))
         (display (cldk-backend:port-display-mirror port)))
    (log:info "display: ~A" display)
    (sleep 2)
    (clim:destroy-port port)))

(defun display-step-00-3 (&optional (backend *backend*))
  (let* ((port (clim:find-port :server-path backend))
         (driver (cldk-backend:port-display-driver port)))
    (log:info "driver id:~A" (cldk-driver:driver-id driver)) 
    (log:info "driver options: ~A" (cldk-driver:driver-options driver))
    (sleep 2)
    (clim:destroy-port port)))

