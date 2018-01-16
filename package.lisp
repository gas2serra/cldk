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
   #:event-handler-modifiers
   #:event-handler-pressed-buttons
   #:handle-button-event
   #:handle-scroll-event
   #:handle-motion-event
   #:handle-key-event
   #:handle-key-modifier-event
   #:handle-enter-leave-event
   #:handle-configure-event
   #:handle-repaint-event
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

   ;;  driver
   #:driver
   #:driver-options
   #:driver-start
   #:driver-stop
   #:driver-kill
   #:driver-ping
   #:driver-force-output

   ;; driver object
   #:driver-object
   #:driver-object-id

   ;; event driver
   #:event-driver-mixin
   #:driver-process-next-event

   ;; display driver
   #:display-driver
   #:display-driver-object
   #:driver-default-screen-index
   #:driver-screen-num
   #:driver-screen-size
   #:driver-screen-dpi
   #:driver-screen-pointer-position
   
   ;; display window 
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

   ;; display cursor
   #:driver-cursor
   #:driver-avaiable-cursor-names 
   #:driver-create-cursor
   #:driver-destroy-cursor
   #:driver-set-window-cursor

   ;; display buffer
   #:driver-buffer
   #:driver-create-buffer
   #:driver-update-buffer
   #:driver-destroy-buffer
   #:driver-copy-buffer-to-window

   #:k-handle-window-configuration-event
   #:k-handle-repaint-event
   #:k-handle-scroll-event
   #:k-handle-button-event
   #:k-handle-motion-event
   #:k-handle-key-event
   #:k-handle-enter-event
   #:k-handle-leave-event
   #:k-handle-wm-delete-event

   #:display-server
   #:lookup-server-object
   #:start-server

   #:buffer
   #:create-buffer
   #:image-rgb-get-fn
   #:image-rgb-set-fn
   ))

(defpackage :cldk-internals
  (:use :cldk :cldk-driver :cldk-extension :common-lisp)
  (:nicknames :cldki))
