
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


(defun display-step-04-0 (&optional (backend *backend*))
  (let ((display (cldk:find-display-server :server-path backend)))
    (let ((w (cldk:create-window display "Pippo" :mode :managed)))
      (cldk:show-window w)
      (sleep 2)
      (format *debug-io* "Size: ~A~%" (multiple-value-list
                                       (cldk:window-size w :force-query-p nil)))
      (format *debug-io* "updating size...~%")
      (cldk:set-window-size w 500 300 :block-p t)
      (format *debug-io* "Size: ~A~%" (multiple-value-list
                                       (cldk:window-size w :force-query-p nil)))
      (format *debug-io* "Size (force): ~A~%" (multiple-value-list
                                               (cldk:window-size w :force-query-p t)))
      (sleep 3)
      (format *debug-io* "Size: ~A~%" (multiple-value-list
                                       (cldk:window-size w :force-query-p nil)))
      (format *debug-io* "Size (force): ~A~%" (multiple-value-list
                                               (cldk:window-size w :force-query-p t)))
      (cldk:destroy-window w))))

(defun display-step-04-1 (&optional (backend *backend*))
  (let ((display (cldk:find-display-server :server-path backend)))
    (let ((w (cldk:create-window display "Pippo" :mode :managed)))
      (cldk:show-window w)
      (sleep 2)
      (format *debug-io* "Position: ~A~%" (multiple-value-list
                                           (cldk:window-position w :force-query-p nil)))
      (format *debug-io* "updating position...~%")
      (cldk:set-window-position w 100 500 :block-p t)
      (format *debug-io* "Position: ~A~%" (multiple-value-list
                                       (cldk:window-position w :force-query-p nil)))
      (format *debug-io* "Position (force): ~A~%" (multiple-value-list
                                               (cldk:window-position w :force-query-p t)))
      (sleep 3)
      (format *debug-io* "Position: ~A~%" (multiple-value-list
                                       (cldk:window-position w :force-query-p nil)))
      (format *debug-io* "Position (force): ~A~%" (multiple-value-list
                                               (cldk:window-position w :force-query-p t)))
      (cldk:destroy-window w))))

