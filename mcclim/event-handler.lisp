(in-package :clim-fb)
;;; event handler

(defclass fb-event-handler (cldk:event-handler)
  ((port :initarg :port
         :reader handler-port)))

(defmethod cldk:handler-button-event ((handler fb-event-handler) kind pointer button
                                      win time)
  (let ((event
         (make-instance (if (eq kind :press)
                            'pointer-button-press-event
                            'pointer-button-release-event)
                        :pointer pointer
                        :button button
                        :x (cldk:event-handler-cur-x handler)
                        :y (cldk:event-handler-cur-y handler)
			:graft-x (cldk:event-handler-cur-root-x handler)
			:graft-y (cldk:event-handler-cur-root-y handler)
			:sheet (climi::port-lookup-sheet (handler-port handler) win)
                        :modifier-state
                        (logior (cldk:event-handler-modifier-state handler)
                                (cldk:event-handler-pressed-buttons handler))
			:timestamp time)))
    (clim:distribute-event (handler-port handler) event)))

(defmethod cldk:handler-wheel-event ((handler fb-event-handler) kind pointer win time)
  (let ((event
         (make-instance 'pointer-button-press-event
                        :pointer pointer
                        :button kind
                        :x (cldk:event-handler-cur-x handler)
                        :y (cldk:event-handler-cur-y handler)
			:graft-x (cldk:event-handler-cur-root-x handler)
			:graft-y (cldk:event-handler-cur-root-y handler)
			:sheet (climi::port-lookup-sheet (handler-port handler) win)
                        :modifier-state (logior (cldk:event-handler-modifier-state handler)
                                                (cldk:event-handler-pressed-buttons handler))
			:timestamp time)))
    (clim:distribute-event (handler-port handler) event)))

(defmethod cldk:handler-motion-event ((handler fb-event-handler) pointer x y
                                      root-x root-y win time)
    (let ((event
           (make-instance 'pointer-motion-event
                          :pointer pointer
                          :button (cldk:event-handler-pressed-buttons handler)
                          :x x :y y
                          :graft-x root-x
                          :graft-y root-y
                          :sheet (climi::port-lookup-sheet (handler-port handler) win)
                          :modifier-state
                          (logior (cldk:event-handler-modifier-state handler)
                                  (cldk:event-handler-pressed-buttons handler))
                          :timestamp time)))
      (clim:distribute-event (handler-port handler) event)))

(defmethod cldk:handler-key-event ((handler fb-event-handler) kind keyname character modifiers
                                   win time)
  (let* ((sheet (climi::port-lookup-sheet (handler-port handler) win))
         (event
          (make-instance (if (eq kind :press)
                             'key-press-event
                             'key-release-event)
                         :key-name keyname
                         :key-character character
                         :x (cldk:event-handler-cur-x handler)
                         :y (cldk:event-handler-cur-y handler)
                         :graft-x (cldk:event-handler-cur-root-x handler)
                         :graft-y (cldk:event-handler-cur-root-y handler)
                         :sheet (or (frame-properties (pane-frame sheet) 'focus) sheet)
                         :modifier-state modifiers
                         :timestamp time)))
      (clim:distribute-event (handler-port handler) event)))

(defmethod cldk:handler-enter-leave-event ((handler fb-event-handler) kind pointer
                                           win time)
  (let* ((sheet (climi::port-lookup-sheet (handler-port handler) win))
         (button (cldk:event-handler-pressed-buttons handler))
         (modifier (cldk:event-handler-modifier-state handler))
         (event
          (if (eq kind :enter)
              (make-instance 'pointer-enter-event
                             :pointer 0
                             :button button
                             :x (cldk:event-handler-cur-x handler)
                             :y (cldk:event-handler-cur-y handler)
                             :graft-x (cldk:event-handler-cur-root-x handler)
                             :graft-y (cldk:event-handler-cur-root-y handler)
                             :sheet sheet
                             :modifier-state modifier
                             :timestamp time)
              (make-instance 'pointer-exit-event
                             :pointer 0 :button button
                             :x (cldk:event-handler-cur-x handler)
                             :y (cldk:event-handler-cur-y handler)
                             :graft-x (cldk:event-handler-cur-root-x handler)
                             :graft-y (cldk:event-handler-cur-root-y handler)
                             :sheet sheet
                             :modifier-state modifier
                             :timestamp time))))
    (clim:distribute-event (handler-port handler) event)))

(defmethod cldk:handler-configure-event ((handler fb-event-handler) win x y w h time)
    (let* ((sheet (climi::port-lookup-sheet (handler-port handler) win))
           (event
            (make-instance 'window-configuration-event
                           :sheet sheet
                           :x x
                           :y y
                           :width w :height h)))
      (clim:distribute-event (handler-port handler) event)))

(defmethod cldk:handler-repaint-event ((handler fb-event-handler) win x y w h time)
  (let* ((sheet (climi::port-lookup-sheet (handler-port handler) win))
         (event   (make-instance 'window-repaint-event
                                 :timestamp time
                                 :sheet sheet
                                 :region (make-rectangle* x y (+ x w) (+ y h)))))
    (clim:distribute-event (handler-port handler) event)))

(defmethod cldk:handler-destroy-event ((handler fb-event-handler) win time)
  (log:info "destroy"))

(defmethod cldk:handler-wm-delete-event ((handler fb-event-handler) win time)
  (let ((event
         (make-instance 'window-manager-delete-event
                        :sheet (climi::port-lookup-sheet (handler-port handler) win)
                        :timestamp time)))
    (clim:distribute-event (handler-port handler) event)))

;;(setf *fb-event-handler* (make-instance 'fb-event-handler))

(defmethod clim:dispatch-event ((client fb-graft) event)
  )
