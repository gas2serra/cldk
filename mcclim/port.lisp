(in-package :clim-fb)

(defclass fb-port (standard-port
                   standard-handled-event-port-mixin
                   render-port-mixin)
  ((server :accessor fb-port-server)
   (pointer :reader port-pointer)
   (font-families :initform nil :accessor font-families)))
  
(defun parse-cldk-server-path (path)
  (cons :cldk (cdr path)))

(setf (get :cldk :port-type) 'fb-port)
(setf (get :cldk :server-path-parser) 'parse-cldk-server-path)

(defclass fb-pointer (standard-pointer)
  ((cursor :accessor pointer-cursor :initform :upper-left)))

(defmethod initialize-instance :after ((port fb-port) &rest args)
  (declare (ignore args))
  (let ((options (cdr (port-server-path port))))
    (let ((server (cldk:find-display-server :server-path (cons (getf options :cldk-driver :null)
                                                       options))))
      (setf (fb-port-server port) server)
      (setf (cldk:server-event-handler server)
            (make-instance 'clim-fb::fb-event-handler :port port))))
  (make-graft port)
  (setf (slot-value port 'pointer)
        (make-instance 'fb-pointer :port port))
  (clim-extensions:port-all-font-families port)
  (push (make-instance 'fb-frame-manager :port port)
	(slot-value port 'frame-managers))
  (log:info (port-server-path port)))



  
(defun round-coordinate (x)
  "Function used for rounding coordinates."
  ;; We use "mercantile rounding", instead of the CL round to nearest
  ;; even number, when in doubt.
  ;;
  ;; Reason: As the CLIM drawing model is specified, you quite often
  ;; want to operate with coordinates, which are multiples of 1/2. 
  ;; Using CL:ROUND gives you "random" results. Using "mercantile
  ;; rounding" gives you consistent results.
  ;;
  ;; Note that CLIM defines pixel coordinates to be at the corners,
  ;; while in X11 they are at the centers. We don't do much about the
  ;; discrepancy, but rounding up at half pixel boundaries seems to
  ;; work well.
  (floor (+ x .5)))

;;;

(defmethod make-medium ((port fb-port) sheet)
  (make-instance 'fb-medium 
		 :sheet sheet))

;;; Pixmap

(defmethod destroy-mirror ((port fb-port) (pixmap image-pixmap-mixin))
  (when (port-lookup-mirror port pixmap)
    (destroy-mirror port pixmap)))

(defmethod realize-mirror ((port fb-port) (pixmap image-pixmap-mixin))
  (setf (sheet-parent pixmap) (graft port))
  (let ((mirror (make-instance 'image-mirror-mixin)))
    (port-register-mirror port pixmap mirror)
    (%make-image mirror pixmap)))

(defgeneric make-fb-mirror (port  win))

(defmethod make-fb-mirror ((port fb-port) win)
  (make-instance 'fb-mirror :window win))


(defmethod realize-mirror ((port fb-port) (sheet mirrored-sheet-mixin))
  (let ((q (compose-space sheet))
        (mirror-region (%sheet-mirror-region sheet))
        (mirror-transformation (%sheet-mirror-transformation sheet)))
    (let ((name (frame-name (pane-frame sheet)))
          (pretty-name (frame-pretty-name (pane-frame sheet)))
          (x (if mirror-transformation
                 (round-coordinate (nth-value 0 (transform-position
                                                 mirror-transformation
                                                 0 0)))
                 0))
          (y (if mirror-transformation
                 (round-coordinate (nth-value 1 (transform-position
                                                 mirror-transformation
                                                 0 0)))
                 0))
          (width (if mirror-region
                     (round-coordinate (bounding-rectangle-width mirror-region))
                     (round-coordinate (space-requirement-width q))))
          (height (if mirror-region
                      (round-coordinate (bounding-rectangle-height mirror-region))
                      (round-coordinate (space-requirement-height q))))
          (background (typecase sheet
                            (permanent-medium-sheet-output-mixin ;; sheet-with-medium-mixin
                              (medium-background sheet))
                            (basic-pane ; CHECKME [is this sensible?] seems to be
                              (let ((background (pane-background sheet)))
                                (if (typep background 'color)
                                    background
                                    +white+)))
                            (t
                              +white+))))
      (let ((mirror
             (cldk:create-buffered-window (fb-port-server port) name
                                          :pretty-name pretty-name
                                          :x x :y y
                                          :width width :height height 
                                          :mode (if (typep sheet 'unmanaged-top-level-sheet-pane) :unmanaged :managed)
                                          :window-class 'fb-mirror)))
        (port-register-mirror port sheet mirror)
        (port-lookup-mirror port sheet)))))

(defmethod port-allocate-pixmap ((port fb-port) sheet width height)
  (let ((pixmap (make-instance 'fb-pixmap
			       :sheet sheet
			       :width width
			       :height height
			       :port port)))
    (when (sheet-grafted-p sheet)
      (realize-mirror port pixmap))
    pixmap))

(defmethod port-deallocate-pixmap ((port fb-port) pixmap)
  )

(defmethod port-set-mirror-region ((port fb-port) mirror mirror-region)
  (cldk:set-window-size mirror
                        (floor (bounding-rectangle-max-x mirror-region))
                        (floor (bounding-rectangle-max-y mirror-region))))
                                   
(defmethod port-set-mirror-transformation
    ((port fb-port) mirror mirror-transformation)
  (cldk:set-window-position mirror
                            (floor (nth-value 0 (transform-position mirror-transformation 0 0)))
                            (floor (nth-value 1 (transform-position mirror-transformation 0 0)))))


(defmethod pointer-position ((pointer fb-pointer))
  (let* ((port (port pointer))
	 (sheet (port-pointer-sheet port)))
    (when sheet
      (log:info sheet (sheet-mirror sheet))
      (multiple-value-bind (x y)
          (values-list (cldk:window-pointer-position (sheet-mirror sheet)))
        (untransform-position (sheet-native-transformation sheet) x y)))))

(defmethod set-sheet-pointer-cursor ((port fb-port) (sheet mirrored-sheet-mixin) cursor)
  #+nil (driver-set-pointer-cursor (fb-port-driver port) (sheet-mirror sheet) cursor))

(defmethod port-frame-keyboard-input-focus ((port fb-port) frame)
  (frame-properties frame 'focus))

(defmethod (setf port-frame-keyboard-input-focus) (focus (port fb-port) frame)
  (setf (frame-properties frame 'focus) focus))

(defmethod graft ((port fb-port))
  (first (port-grafts port)))

(defmethod make-graft ((port fb-port) &key (orientation :default) (units :device))
  (let ((graft (make-instance 'fb-graft
                              :port port
                              :mirror nil
                              ;;:mirror (clx-port-window port)
                              :orientation orientation
                              :units units)))
    (setf (sheet-region graft)
          (make-bounding-rectangle 0 0
                                   (first (cldk:screen-size (fb-port-server port) 0 units))
                                   (second (cldk:screen-size (fb-port-server port) 0 units))))
    (push graft (port-grafts port))
    graft))

(defmethod port-force-output ((port fb-port))
  (maphash #'(lambda (key val)
               (declare (ignore val))
               (when (typep key 'fb-mirrored-sheet-mixin)
                 (mcclim-render-internals::%mirror-force-output (sheet-mirror key))))
           (slot-value port 'climi::sheet->mirror))
  (cldki::driver-force-output (cldki::driver (fb-port-server port))))

(defmethod synthesize-pointer-motion-event ((pointer fb-pointer))
  (let* ((port (port pointer))
	 (sheet (port-pointer-sheet port)))
    (when sheet
      (let ((mirror (sheet-direct-mirror sheet))
            (eh (cldk:server-event-handler
                 (fb-port-server port))))
        (log:info (cldk:event-handler-cur-x eh) (cldk:event-handler-cur-y eh))
	(when mirror
	  (multiple-value-bind (x y)
	      (values-list (cldk:window-pointer-position mirror))
            (make-instance
             'pointer-motion-event
             :pointer 0 :button (cldk:event-handler-pressed-buttons eh)
             :x (cldk:event-handler-cur-x eh)
             :y (cldk:event-handler-cur-y eh)
             :graft-x (cldk:event-handler-cur-root-x eh)
             :graft-y (cldk:event-handler-cur-root-y eh)
             :sheet sheet
             :modifier-state (cldk:event-handler-modifier-state eh)
             ;; The event initialization code will give us a
             ;; reasonable timestamp.
             :timestamp 0)))))))

(defmethod climi::port-grab-pointer ((port fb-port) pointer sheet)
  ;; FIXME: Use timestamps?
  (let ((grab-result (cldk:grab-window-pointer
                      (sheet-mirror sheet)
                      pointer)))
    (if grab-result
 	(setf (pointer-grab-sheet port) sheet)
	nil)))

(defmethod port-ungrab-pointer ((port fb-port) pointer sheet)
  (when (eq (pointer-grab-sheet port) sheet)
    (cldk:ungrab-window-pointer (sheet-mirror sheet) pointer)
    (setf (pointer-grab-sheet port) nil)))

(defmethod clim:pointer-button-state ((pointer fb-pointer))
  (cldk:event-handler-pressed-buttons
   (cldk:server-event-handler (fb-port-server (port pointer)))))
  
  #+nil (multiple-value-bind (x y same-screen-p child mask)
      (xlib:query-pointer (clx-port-window (port pointer)))
    (declare (ignore x y same-screen-p child))
    (ldb (byte 5 8) mask))
