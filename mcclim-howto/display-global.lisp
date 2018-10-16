(ql:quickload :mcclim-cldk)
(ql:quickload :cldk-backend)
(ql:quickload :cldk-clx-backend)
(ql:quickload :cldk-sdl2-backend)
(ql:quickload :cldk-xcb-backend)


(defparameter *clx-backend* '(:clx-cldk :cldk-driver :clx))
(defparameter *sdl2-backend* '(:sdl2-cldk :cldk-driver :sdl2))
(defparameter *xcb-backend* '(:xcb-cldk :cldk-driver :xcb))
(defparameter *backend* *clx-backend*)

;;;
;;; logs utility
;;;
(defun log-ports ()
  (let ((ports nil))
    (clim:map-over-ports #'(lambda (port) (push port ports)))
    (log:info "ports: ~A" ports)))

(defun log-port-threads ()
  (let ((names nil)
        (slime-thread-names (list "repl-thread" "auto-flush-thread" "swank-indentation-cache-thread"
                                  "reader-thread" "control-thread" "Swank Sentinel" "main thread")))
    (dolist (thread (bt:all-threads))
      (unless (member (bt:thread-name thread) slime-thread-names :test #'equal)
        (push (bt:thread-name thread) names)))
    (log:info "threads: ~S" names)))

#|
(cldk:map-over-servers #'cldk:destroy-server)
(cldk:map-over-servers #'cldki::kill-server)
|#

;;;
;;; panes
;;;

(defclass null-pane (clim:basic-pane)
  ())

(defmethod clim:compose-space ((pane null-pane) &key (width 200) (height 200))
  (clim:make-space-requirement :width width :min-width 0 :max-width 600
                               :height height :min-height 0 :max-height 600))

;;;
;;; apps
;;;

(clim:define-application-frame my-app-a ()
  ()
  (:panes
   (int (clim:make-pane 'null-pane)))
  (:layouts
   (default int)))
