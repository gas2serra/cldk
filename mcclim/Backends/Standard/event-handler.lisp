(in-package :cldk-backend)

;;; event handler

(defclass cldk-event-handler (cldk:event-handler)
  ((port :initarg :port
         :reader handler-port)))

(defmethod cldk:handle-button-event ((handler cldk-event-handler) kind pointer button
                                      win time)
  (let ((event
         (make-instance (if (eq kind :press)
                            'clim:pointer-button-press-event
                            'clim:pointer-button-release-event)
                        :pointer pointer
                        :button button
                        :x (cldk:event-handler-cur-x handler)
                        :y (cldk:event-handler-cur-y handler)
			:graft-x (cldk:event-handler-cur-root-x handler)
			:graft-y (cldk:event-handler-cur-root-y handler)
			:sheet (climi::port-lookup-sheet (handler-port handler) win)
                        :modifier-state
                        (logior (cldk:event-handler-modifiers handler)
                                (cldk:event-handler-pressed-buttons handler))
			:timestamp time)))
    (clim:distribute-event (handler-port handler) event)))

(defmethod cldk:handle-scroll-event ((handler cldk-event-handler) pointer dx dy win time)
  (let ((event
         (make-instance 'climi::pointer-scroll-event
                        :pointer pointer
                        :x (cldk:event-handler-cur-x handler)
                        :y (cldk:event-handler-cur-y handler)
			:graft-x (cldk:event-handler-cur-root-x handler)
			:graft-y (cldk:event-handler-cur-root-y handler)
                        :delta-x dx
                        :delta-y dy 
                        :sheet (climi::port-lookup-sheet (handler-port handler) win)
                        :modifier-state (logior (cldk:event-handler-modifiers handler)
                                                (cldk:event-handler-pressed-buttons handler))
			:timestamp time)))
    (clim:distribute-event (handler-port handler) event)))

(defmethod cldk:handle-motion-event ((handler cldk-event-handler) pointer x y
                                      root-x root-y win time)
    (let ((event
           (make-instance 'clim:pointer-motion-event
                          :pointer pointer
                          :button (cldk:event-handler-pressed-buttons handler)
                          :x x :y y
                          :graft-x root-x
                          :graft-y root-y
                          :sheet (climi::port-lookup-sheet (handler-port handler) win)
                          :modifier-state
                          (logior (cldk:event-handler-modifiers handler)
                                  (cldk:event-handler-pressed-buttons handler))
                          :timestamp time)))
      (clim:distribute-event (handler-port handler) event)))

(defmethod cldk:handle-key-event ((handler cldk-event-handler) kind keyname character modifiers
                                  win time)

  (let* ((sheet (climi::port-lookup-sheet (handler-port handler) win))
         (event
          (make-instance (if (eq kind :press)
                             'clim:key-press-event
                             'clim:key-release-event)
                         :key-name keyname
                         :key-character character
                         :x (or (cldk:event-handler-cur-x handler) 0)
                         :y (or (cldk:event-handler-cur-y handler) 0)
                         :graft-x (or (cldk:event-handler-cur-root-x handler) 0)
                         :graft-y (or (cldk:event-handler-cur-root-y handler) 0)
                         :sheet (or (climi::frame-properties (climi::pane-frame sheet) :focus) sheet)
                         :modifier-state modifiers
                         :timestamp time)))
    (log:warn "~A ~A " (climi::frame-properties (climi::pane-frame sheet) ':focus)
              event)
      (clim:distribute-event (handler-port handler) event)))

(defmethod cldk:handle-enter-leave-event ((handler cldk-event-handler) kind pointer
                                           win time)
  (let* ((sheet (climi::port-lookup-sheet (handler-port handler) win))
         (button (cldk:event-handler-pressed-buttons handler))
         (modifier (cldk:event-handler-modifiers handler))
         (event
          (if (eq kind :enter)
              (make-instance 'clim:pointer-enter-event
                             :pointer 0
                             :button button
                             :x (cldk:event-handler-cur-x handler)
                             :y (cldk:event-handler-cur-y handler)
                             :graft-x (cldk:event-handler-cur-root-x handler)
                             :graft-y (cldk:event-handler-cur-root-y handler)
                             :sheet sheet
                             :modifier-state modifier
                             :timestamp time)
              (make-instance 'clim:pointer-exit-event
                             :pointer 0 :button button
                             :x (cldk:event-handler-cur-x handler)
                             :y (cldk:event-handler-cur-y handler)
                             :graft-x (cldk:event-handler-cur-root-x handler)
                             :graft-y (cldk:event-handler-cur-root-y handler)
                             :sheet sheet
                             :modifier-state modifier
                             :timestamp time))))
    (clim:distribute-event (handler-port handler) event)))

(defmethod cldk:handle-configure-event ((handler cldk-event-handler) win x y w h time)
    (let* ((sheet (climi::port-lookup-sheet (handler-port handler) win))
           (event
            (make-instance 'clim:window-configuration-event
                           :sheet sheet
                           :x x
                           :y y
                           :width w :height h)))
      (clim:distribute-event (handler-port handler) event)))

(defmethod cldk:handle-repaint-event ((handler cldk-event-handler) win x y w h time)
  (let* ((sheet (climi::port-lookup-sheet (handler-port handler) win))
         (event   (make-instance 'clim:window-repaint-event
                                 :timestamp time
                                 :sheet sheet
                                 :region (clim:make-rectangle* x y (+ x w) (+ y h)))))
    (clim:distribute-event (handler-port handler) event)))

(defmethod cldk:handle-wm-delete-event ((handler cldk-event-handler) win time)
  (let ((event
         (make-instance 'clim:window-manager-delete-event
                        :sheet (climi::port-lookup-sheet (handler-port handler) win)
                        :timestamp time)))
    (clim:distribute-event (handler-port handler) event)))
