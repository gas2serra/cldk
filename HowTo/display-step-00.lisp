
(load (asdf/pathname:merge-pathnames*
       (asdf/pathname:parse-unix-namestring "HowTo/display-global.lisp")
       (asdf/pathname:pathname-directory-pathname (asdf:system-source-file :cldk))))

(defun display-step-00-0 (&optional (backend *backend*) (options nil))
  (log:info (bt:all-threads))
  (let ((display (cldki::create-display backend options)))
    (log:info (bt:all-threads))
    (cldki::destroy-driver display)
    #+nil(cldk:destroy-display display))
  (sleep 1)
  (log:info (bt:all-threads)))

(defun display-step-00-1 (&optional (backend *backend*) (options nil))
  (let ((display (cldki::create-display backend options)))
    (format *debug-io* "options: ~S~%" (cldk-driver:driver-options display))
    (format *debug-io* "id: ~S~%" (cldk-driver:driver-id display))
    (cldki::destroy-driver display)))
