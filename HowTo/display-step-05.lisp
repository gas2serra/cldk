
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


(defun display-step-05-0 (&optional (backend *backend*))
  (let ((display (cldk:find-display-server :server-path backend)))
    (let ((w (cldk:create-window display "Pippo" :mode :managed)))
      (cldk:show-window w)
      (sleep 2)
      (format *debug-io* "Size: ~A~%" (multiple-value-list
                                       (cldk:window-size w :force-query-p nil)))
      (format *debug-io* "updating max hints...~%")
      (cldk:set-window-hints w :max-width 100 :max-height 100)
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

(defun display-step-05-1 (&optional (backend *backend*))
  (let ((display (cldk:find-display-server :server-path backend)))
    (let ((w (cldk:create-window display "Pippo" :mode :managed)))
      (cldk:show-window w)
      (sleep 2)
      (format *debug-io* "Size: ~A~%" (multiple-value-list
                                       (cldk:window-size w :force-query-p nil)))
      (format *debug-io* "updating min hints...~%")
      (cldk:set-window-hints w :min-width 500 :min-height 400)
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

(defun display-step-05-2 (&optional (backend *backend*))
  (let ((display (cldk:find-display-server :server-path backend)))
    (let ((w (cldk:create-window display "Pippo" :mode :managed)))
      (cldk:show-window w)
      (sleep 2)
      (format *debug-io* "Size: ~A~%" (multiple-value-list
                                       (cldk:window-size w :force-query-p nil)))
      (format *debug-io* "updating base hints...~%")
      (cldk:set-window-hints w :width 500 :height 400)
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
