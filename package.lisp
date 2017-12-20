(in-package :common-lisp-user)

(defpackage :cldk
  (:use :common-lisp)
  (:export
   #:*default-server-path*
   #:find-server
   #:find-display-server
   #:server-path
   #:server-event-handler
   #:restart-server
   #:destroy-server
   #:map-over-servers

   #:+pointer-left-button+
   #:+pointer-middle-button+
   #:+pointer-right-button+
   #:+pointer-wheel-up+
   #:+pointer-wheel-down+
   #:+pointer-wheel-left+
   #:+pointer-wheel-right+
   #:+pointer-x1-button+
   #:+pointer-x2-button+
   #:+shift-key+
   #:+control-key+
   #:+meta-key+
   #:+super-key+
   #:+hyper-key+
   #:+alt-key+
   #:event-handler
   #:log-event-handler
   #:event-handler-events-to-log
   #:*events-to-log-list*
   
   #:event-handler-cur-x
   #:event-handler-cur-y
   #:event-handler-cur-root-x
   #:event-handler-cur-root-y
   #:event-handler-modifier-state
   #:event-handler-pressed-buttons
   #:handle-button-event
   #:handle-wheel-event
   #:handle-motion-event
   #:handle-key-event
   #:handle-key-modifier-event
   #:handle-enter-leave-event
   #:handle-configure-event
   #:handle-repaint-event
   #:handle-destroy-event
   #:handle-wm-delete-event
   
   #:screen-num
   #:screen-size
   #:screen-dpi
   #:screen-pointer-position
   #:avaiable-cursor-names
   
   #:create-window
   #:destroy-window
   #:window-size
   #:window-position
   #:set-window-size
   #:set-window-position
   #:set-window-hints
   
   #:raise-window
   #:bury-window
   #:show-window
   #:hide-window

   #:window-pointer-position
   #:grab-window-pointer
   #:ungrab-window-pointer
   #:set-window-cursor

   #:rgb-image
   #:copy-image
   #:image
   #:image-width
   #:image-height
   #:image-pixels
   
   #:buffered-window
   #:create-buffered-window
   #:copy-image-to-buffered-window
   #:flush-buffered-window
   #:buffered-window-image
   ))

(defpackage :cldk-extension
  (:use :common-lisp)
  (:export
   ))

(defpackage :cldk-driver
  (:use :common-lisp)
  (:export
   #:display-server
   #:display-driver
   #:driver
   #:start-server

   #:lookup-server-object
   
   #:driver-start
   #:driver-stop
   #:driver-ping
   #:driver-options
   #:driver-default-screen-index
   #:driver-process-next-event
   #:driver-force-output
   
   #:driver-screen-num
   #:driver-screen-size
   #:driver-screen-dpi
   #:driver-screen-pointer-position

   #:driver-object-id
   
   #:driver-window
   #:driver-create-window
   #:driver-destroy-window
   #:driver-show-window
   #:driver-hide-window
   #:driver-window-position
   #:driver-window-size
   #:driver-set-window-position
   #:driver-set-window-size
   #:driver-set-window-hints
   #:driver-raise-window
   #:driver-bury-window
   #:driver-window-pointer-position

   #:driver-grab-pointer
   #:driver-ungrab-pointer
   
   #:driver-avaiable-cursor-names 
   #:driver-cursor
   #:driver-create-cursor
   #:driver-destroy-cursor
   #:driver-set-window-cursor

   #:driver-buffer
   #:driver-buffer-width
   #:driver-buffer-height
   #:driver-buffer-data
   #:driver-create-buffer
   #:driver-update-buffer
   #:driver-destroy-buffer
   #:driver-copy-buffer-to-window
   #:driver-create-image
   #:driver-update-image

   #:k-handle-window-configuration-event
   #:k-handle-repaint-event
   #:k-handle-wheel-event
   #:k-handle-button-event
   #:k-handle-motion-event
   #:k-handle-key-event
   #:k-handle-enter-event
   #:k-handle-leave-event
   #:k-handle-destroy-event
   #:k-handle-wm-delete-event

   #:buffer
   #:create-buffer
   #:image-rgb-get-fn
   #:image-rgb-set-fn
   ))

(defpackage :cldk-internals
  (:use :cldk :cldk-driver :cldk-extension :common-lisp)
  (:nicknames :cldki))
