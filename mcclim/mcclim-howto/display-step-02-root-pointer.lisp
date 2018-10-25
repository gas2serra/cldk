(load (asdf/pathname:merge-pathnames*
       (asdf/pathname:parse-unix-namestring "mcclim-howto/display-global.lisp")
       (asdf/pathname:pathname-directory-pathname (asdf:system-source-file :cldk))))

(defun display-step-02-0 (&optional (backend *backend*))
  (let* ((clim:*default-server-path* backend)
         (graft (clim:find-graft)))
    (loop for i from 0 below 10 do
         (log:info "pointer position: ~A"
                   (multiple-value-list (clim:pointer-position graft)))
         (sleep 0.5))
    (let ((port (clim:find-port :server-path backend)))
      (clim:destroy-port port))))
