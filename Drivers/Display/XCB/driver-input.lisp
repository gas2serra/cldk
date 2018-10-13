(in-package :cldk-display-xcb)

(defun event-type (event)
  "return type of event"
  (ldb (byte 7 0) (cffi:mem-ref event :uint8)))
  
(defun xcb-event-handler (driver event)
  (let ((event-type (event-type event))
        (handler (driver-callback-handler driver)))
    (case event-type
      (22
       (cffi:with-foreign-slots ((window x y width height sequence)
                                 event
			         (:struct ES-CONFIGURE))
         (driver-cb-window-configuration-event handler
                                               driver
                                               (lookup-driver-object driver window)
                                               x y width height sequence)))
      (t
       (log:warn "Unhandled event: ~A" (aref events (event-type event)))))))
