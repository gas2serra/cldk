(in-package :cldk-driver-xcb)

(defclass xcb-driver (display-driver)
  ((display :initform nil :reader xcb-driver-display)
   (screen :initform nil)
   (root-window :initform nil)
   (gc :initform nil)))

(defmethod driver-start ((driver xcb-driver))
  (with-slots (display screen root-window gc) driver
    (let ((options (driver-options driver)))
      (setf (driver-default-screen-index driver) (getf options :screen-id 0))
      (setf display (connect (getf options :host "") (cffi:null-pointer)))
      #+nil(setf screen (nth (getf options :screen-id 0)
                             (getf (setup-roots-iterator (get-setup display)) 'data)))
      (setf screen (getf (setup-roots-iterator (get-setup display)) 'data))
      (setf root-window (cffi:mem-ref screen :uint32))
      (setf gc (generate-id display))
      (check (xcb-create-gc display gc root-window 0 (cffi:null-pointer))))))

(defmethod driver-stop ((driver xcb-driver))
  (with-slots (display) driver
    (disconnect display))
  (log:trace driver))

(defmethod driver-kill ((driver xcb-driver))
  (with-slots (display) driver
    (disconnect display)))

(defmethod driver-ping ((driver xcb-driver))
  (with-slots (display) driver
    (and display)))

(defmethod driver-force-output ((driver xcb-driver))
  (with-slots (display) driver
    (xcb-flush display)))

;;; events
(defmethod driver-process-next-event ((driver xcb-driver))
  (with-slots (display) driver
    (let ((e (poll-for-event display)))
      (if (cffi:null-pointer-p e)
	  nil
	  (progn
            (xcb-event-handler driver e)
            t)))))

;;; root
(defclass xcb-driver-root (driver-root)
  ((xroot)
   (xscreen)))

(defmethod driver-object-id ((root xcb-driver-root))
  (with-slots (xroot) root
    xroot))

(defmethod driver-initialize-root ((driver xcb-driver) root)
  (with-slots (root-window screen) driver
    (with-slots (xroot xscreen) root
      (setf xroot root-window)
      (setf xscreen screen))))

(defmethod driver-destroy-root ((root xcb-driver-root))
  (with-slots (xroot xscreen) root
    (setf xroot nil
          xscreen nil)))

(defmethod driver-root-size ((root xcb-driver-root) units)
  (with-slots (xscreen) root
    (ecase units
      (:device (cffi:with-foreign-slots ((width-in-pixels height-in-pixels)
                                         xscreen (:struct screen-t))
                 (values width-in-pixels height-in-pixels)))
      (:inches (cffi:with-foreign-slots ((width-in-mm height-in-mm)
                                         xscreen (:struct screen-t))
                 (values (/ width-in-mm 25.4s0) (/ height-in-mm 25.4s0))))
      (:millimeters (cffi:with-foreign-slots ((width-in-mm height-in-mm) xscreen (:struct screen-t))
                      (values width-in-mm height-in-mm))))))

(defmethod driver-root-pointer-position ((root xcb-driver-root))
  (with-slots (display) (driver root)
    (with-slots (xroot) root
      (let ((cookie (xcb-query-pointer display xroot)))
      (let ((response (xcb-query-pointer-reply display cookie (cffi:null-pointer))))
        (cffi:with-foreign-slots ((root-x root-y) response (:struct xcb-query-pointer-replay-t))
          (values root-x root-y)))))))

;;; window

(defclass xcb-driver-window (driver-window)
  ((xwindow :initarg :xwindow)))

(defmethod driver-object-id ((window xcb-driver-window))
  (with-slots (xwindow) window
    xwindow))

(defmethod driver-initialize-window ((driver xcb-driver) window name pretty-name x y
                                     width height mode)
  (with-slots (display screen root-window) driver
    (cffi:with-foreign-slots ((root root-visual white-pixel black-pixel root-depth)
                              screen
			      (:struct screen-t))
      (let ((win-id (generate-id display)))
        (w-foreign-values (vals
			   ;;:uint32 white-pixel
			   :uint32 (+
				    EVENT-MASK-EXPOSURE
				    EVENT-MASK-STRUCTURE-NOTIFY
				    ;;EVENT-MASK-RESIZE-REDIRECT
				    ;; EVENT-MASK-BUTTON-PRESS
				    EVENT-MASK-KEY-PRESS))
          (with-slots (xwindow) window
            (setf xwindow win-id)
            (check (xcb-create-window
                    display root-depth win-id
                    root
                    0 0 200 200 10
                    WINDOW-CLASS-INPUT-OUTPUT
                    root-visual
                    (+ CW-EVENT-MASK)
                    vals))))))))

(defmethod driver-destroy-window ((driver xcb-driver) window)
  (log:trace window)
  (with-slots (display) driver
    (with-slots (xwindow) window
      (xcb-destroy-window display xwindow))))

(defmethod driver-show-window ((driver xcb-driver) window)
  (log:trace window)
  (with-slots (display) driver
    (with-slots (xwindow) window
      (xcb-map-window-checked display xwindow)
      (xcb-flush display))))

(defmethod driver-hide-window ((driver xcb-driver) window)
  (log:trace window)
  (with-slots (display) driver
    (with-slots (xwindow) window
      (xcb-unmap-window display xwindow)
      (xcb-flush display))))

(defmethod driver-window-position ((driver xcb-driver) window)
  (with-slots (display root) driver
    (with-slots (xwindow) window
      (let ((geom (xcb-get-geometry-reply display
                                          (xcb-get-geometry display xwindow)
                                          (cffi:null-pointer))))
        (cffi:with-foreign-slots ((x y)
                                  geom
			          (:struct get-geometry-reply-t))
          (let ((coord (xcb-translate-coordinates-reply display
                                                        (xcb-translate-coordinates display window root x y)
                                                        (cffi:null-pointer))))
            (cffi:with-foreign-slots ((dest-x dest-y)
                                      coord
			              (:struct translate-coordinates-reply-t))
              (values dest-x dest-y))))))))

(defmethod driver-window-size ((driver xcb-driver) window)
  (with-slots (display) driver
    (with-slots (xwindow) window
      (let ((geom (xcb-get-geometry-reply display
                                          (xcb-get-geometry display xwindow)
                                          (cffi:null-pointer))))
        (cffi:with-foreign-slots ((width height)
                                  geom
			          (:struct get-geometry-reply-t))
                                 (values width height))))))
 
(defmethod driver-set-window-position ((driver xcb-driver) window x y)
  (with-slots (display) driver
    (with-slots (xwindow) window
      (w-foreign-values (vals
	                 :uint32 x
                         :uint32 y)
        (check (xcb-configure-window
                display xwindow
                (+ CONFIG-WINDOW-X CONFIG-WINDOW-Y)
                vals))))))

(defmethod driver-set-window-size ((driver xcb-driver) window width height)
  (with-slots (display) driver
    (with-slots (xwindow) window
      (w-foreign-values (vals
	                 :uint32 width
                         :uint32 height)
        (check (xcb-configure-window
                display xwindow
                (+ CONFIG-WINDOW-WIDTH CONFIG-WINDOW-HEIGHT)
                vals))))))


(defmethod driver-set-window-hints ((driver xcb-driver) window x y width height max-width max-height
                                    min-width min-height)
  (with-slots (display) driver
    (with-slots (xwindow) window
      (log:trace "hints")
      (w-foreign-values (hints
		         :UINT32 (+
                                  (if (and width height) SIZE-HINT-BASE-SIZE 0)
                                  (if (and max-width max-height) SIZE-HINT-P-MAX-SIZE 0)
                                  (if (and min-width min-height) SIZE-HINT-P-MIN-SIZE 0)
                                  (if (and x y) SIZE-HINT-P-POSITION 0))
		         :UINT32 (or x 0) :UINT32 (or y 0) ;x y 
		         :UINT32 (or width 0) :UINT32 (or height 0) ;w h
		         :UINT32 (or min-width 0) :UINT32 (or min-height 0) ;min
		         :UINT32 (or max-width 0) :UINT32 (or max-height 0) ;max
		         :UINT32 0 :UINT32 0 ;inc
		         :UINT32 0 :UINT32 0 ;min aspec
		         :UINT32 0 :UINT32 0 ;max aspect
		         :UINT32 (or width 0) :UINT32 (or height 0) ;base
		         :UINT32 0 ;grav
		         )
        (check (set-wm-normal-hints display xwindow hints))))))


(defmethod driver-bury-window ((driver xcb-driver) window)
  (with-slots (display) driver
    (with-slots (xwindow) window
      (w-foreign-values (vals
	                 :uint32 STACK-MODE-BOTTOM-IF)
        (check (xcb-configure-window
                display xwindow
                (+ CONFIG-WINDOW-STACK-MODE)
                vals))))))

(defmethod driver-raise-window ((driver xcb-driver) window)
  (with-slots (display) driver
    (with-slots (xwindow) window
      (w-foreign-values (vals
	                 :uint32 STACK-MODE-TOP-IF)
        (check (xcb-configure-window
                display xwindow
                (+ CONFIG-WINDOW-STACK-MODE)
                vals))))))

(defmethod driver-window-pointer-position ((driver xcb-driver) window)
  (with-slots (display) driver
    (with-slots (xwindow) window
      (let ((cookie (xcb-query-pointer display xwindow)))
        (let ((response (xcb-query-pointer-reply display cookie (cffi:null-pointer))))
          (cffi:with-foreign-slots ((win-x win-y) response (:struct xcb-query-pointer-replay-t))
            (values win-x win-y)))))))

;;; cursors
(defclass xcb-cursor (driver-cursor)
  ((xcursor :initarg :xcursor)))

(defvar *xcb-cursor-mapping*  
  '(;; These are taken from the Franz CLIM User's Guide
    (:busy 150)
    (:button 60)
    (:default 68)
    (:horizontal-scroll 108)
    (:horizontal-thumb 108)
    (:lower-left 12)
    (:lower-right 14)
    (:move 52)
    (:position 130)
    (:prompt 152)
    (:scroll-down 106)
    (:scroll-left 110)
    (:scroll-right 112)
    (:scroll-up 114)
    (:upper-left 134)
    (:upper-right 136)
    (:vertical-scroll 116)
    (:vertical-thumb 116)
    ;; The following are not in the Franz docs, but might be useful.
    (:i-beam 152)
    (:vertical-pointer 22)
    (:pencil 86)
    (:rotate 50)    
    (:choose 60)))

(defmethod driver-avaiable-cursor-names ((driver xcb-driver))
  (mapcar #'car *xcb-cursor-mapping*))

(defmethod driver-create-cursor ((driver xcb-driver) named-cursor)
  (with-slots (display) driver
    (let ((code (second (or
                         (assoc named-cursor *xcb-cursor-mapping*)
                         (assoc :default *xcb-cursor-mapping*))))
          (font-id (generate-id display))
          (cid (generate-id display)))
      (check (xcb-open-font display font-id (length "cursor") "cursor"))
      (check (xcb-create-glyph-cursor display cid font-id font-id code (+ code 1)
                                      0 0 0 0 0 0))
      (make-instance 'xcb-cursor :xcursor cid))))

(defmethod driver-destroy-cursor ((driver xcb-driver) cursor)
  (with-slots (display) driver
    (with-slots (xcursor) cursor
      (xcb-free-cursor display xcursor))))

(defmethod driver-set-window-cursor ((driver xcb-driver) window cursor)
  (with-slots (display) driver
    (with-slots (xwindow) window
      (with-slots (xcursor) cursor
        (w-foreign-values (vals
	                   :uint32 xcursor)
          (check (xcb-change-window-attributes
                  display xwindow
                  (+ CW-CURSOR)
                  vals)))))))

;;; buffer
(defclass xcb-driver-buffer (driver-buffer)
  ((xpixels :initarg :ximage
           :initform nil)
   (w :initform :w)
   (h :initform :h)))

(deftype xcb-rgb-image-pixels () 'cffi-sys:foreign-pointer)

(defmethod driver-initialize-buffer ((driver xcb-driver) buffer width height)
  (driver-update-buffer driver buffer width height))

(defmethod driver-update-buffer ((driver xcb-driver) buffer width height)
  (with-slots (display screen) driver
    (cffi:with-foreign-slots ((root-depth)
                              screen
			      (:struct screen-t))
      (with-slots (xpixels w h) buffer
        (when xpixels
          (static-vectors:free-static-vector xpixels))
        (setf w width
              h height
              xpixels (static-vectors:make-static-vector (* width height 4)
                                                        :element-type '(unsigned-byte 8)))))))
                   
(defmethod driver-destroy-buffer ((driver xcb-driver) buffer)
  (with-slots (xpixels) buffer
    (when xpixels
      (static-vectors:free-static-vector xpixels)
      (setf xpixels nil))))

(defmethod driver-copy-buffer-to-window ((driver xcb-driver) buffer src-x src-y
                                         width height
                                         window to-x to-y)
  (with-slots (display screen gc) driver
    (cffi:with-foreign-slots ((root-depth)
                              screen
			      (:struct screen-t))
      (log:info "~A ~A => ~A (~A)" (list src-x src-y)
                (list width height) (list to-x to-y) root-depth)
        
    (with-slots (xpixels w h) buffer
      (with-slots (xwindow) window
        (check (xcb-put-image display IMAGE-FORMAT-Z-PIXMAP xwindow gc
                              100 100
                              10 10
                              0 root-depth
                              (+ (* 4 100 100) (* 0 100))
                              (static-vectors:static-vector-pointer xpixels
                                                                    :offset
                                                                    0
                                                                    ))))))))

#+nil(xcb-image-put display xwindow gc xpixels to-x to-y)

(defmethod driver-buffer-rgb-get-fn ((driver xcb-driver) buffer dx dy)
  (with-slots (xpixels w) buffer
    (let ((pixels (static-vectors:static-vector-pointer xpixels))
          (width w))
      (declare (type xcb-rgb-image-pixels pixels))
      (lambda (x y)
        (cffi-sys:%mem-ref
         pixels :UNSIGNED-INT (* 4
                                 (+
                                  (* (+ y dy) width)
                                  (+ x dx))))))))

(defmethod driver-buffer-rgb-set-fn ((driver xcb-driver) buffer dx dy)
  (with-slots (xpixels w) buffer
    (let ((pixels (static-vectors:static-vector-pointer xpixels))
          (width w))
      (declare (type xcb-rgb-image-pixels pixels))
      (lambda (x y r g b)
        (cffi-sys:%mem-set
         (dpb b (byte 8 0)
              (dpb g (byte 8 8)
                   (dpb r (byte 8 16)
                        (dpb 255 (byte 8 24) 0))))
         pixels :UNSIGNED-INT (* 4
                                 (+
                                  (* (+ y dy) width)
                                  (+ x dx))))))))

