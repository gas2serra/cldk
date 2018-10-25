(load (asdf/pathname:merge-pathnames*
       (asdf/pathname:parse-unix-namestring "mcclim-howto/display-global.lisp")
       (asdf/pathname:pathname-directory-pathname (asdf:system-source-file :cldk))))

(defun display-step-01-0 (&optional (backend *backend*))
  (let* ((clim:*default-server-path* backend)
         (graft (clim:find-graft)))
    (log:info "orientation: ~A; units: ~A"
              (clim:graft-orientation graft)
              (clim:graft-units graft))
    (loop for units in '(:device :millimeters :inches)
       do 
         (log:info "size: ~A ~A"
                 units
                 (list (clim:graft-width graft :units units)
                       (clim:graft-height graft :units units))))
    (log:info "dpi: ~A; dpm: ~A"
              (clim:graft-pixels-per-inch graft)
              (clim:graft-pixels-per-millimeter graft))
    (let ((port (clim:find-port :server-path backend)))
      (clim:destroy-port port))))

(defun display-step-01-1 (&optional (backend *backend*))
  (let* ((clim:*default-server-path* backend)
         (graft (clim:find-graft)))
    (loop for i from 0 below 10 do
         (log:info "size: ~A"
                   (list (clim:graft-width graft)
                         (clim:graft-height graft)))
         (sleep 2))
    (let ((port (clim:find-port :server-path backend)))
      (clim:destroy-port port))))




