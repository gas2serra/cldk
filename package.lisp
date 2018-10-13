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

   #|
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
   |#


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

   #:window
   #:buffered-window
   #:create-buffered-window
   #:copy-image-to-buffered-window
   #:flush-buffered-window
   #:buffered-window-image
   #:event-handler
   
   #:buffer
   #:create-buffer
   #:destroy-buffer
   #:update-buffer
   
   #:image-rgb-get-fn
   #:image-rgb-set-fn
  ))

(defpackage :cldk-driver
  (:use :common-lisp)
  (:export
   ;;
   ;;  driver
   ;;
   #:driver
   #:driver-options
   #:driver-id
   #:driver-start
   #:driver-stop
   #:driver-kill
   #:driver-ping
   #:driver-force-output
   #:driver-process-next-event
   #:driver-process-next-events
   ;; callback hanlder
   #:register-driver-object
   #:unregister-driver-object
   #:lookup-driver-object
   #:driver-callback-handler
   ;; driver object
   #:driver-object
   #:driver-object-id
   ;; threaded driver
   #:threaded-driver-mixin
   #:driver-lock
   #:with-driver-locked
   #:invoke-with-driver-locked
   #:single-threaded-driver-mixin
   #:multi-threaded-driver-mixin
   ;;
   ;; display drivers
   ;;
   #:display-driver
   #:display-driver-object
   #:display-driver-callback-handler
   
   #:driver-default-screen-index
   #:driver-screen-num
   #:driver-screen-size
   #:driver-screen-dpi
   #:driver-screen-pointer-position
   
   ;; display window 
   #:driver-window
   #:driver-initialize-window
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
   #:driver-initialize-buffer
   #:driver-update-buffer
   #:driver-destroy-buffer
   #:driver-copy-buffer-to-window
   #:driver-buffer-rgb-get-fn
   #:driver-buffer-rgb-set-fn
   
   ;; callback
   #:+pointer-left-button+ 
   #:+pointer-middle-button+
   #:+pointer-right-button+
   #:+pointer-x1-button+
   #:+pointer-x2-button+
   #:+shift-key+
   #:+control-key+
   #:+meta-key+
   #:+super-key+
   #:+hyper-key+
   #:+alt-key+
   
   #:driver-cb-window-configuration-event
   #:driver-cb-repaint-event
   #:driver-cb-scroll-event
   #:driver-cb-button-event
   #:driver-cb-motion-event
   #:driver-cb-key-event
   #:driver-cb-enter-event
   #:driver-cb-leave-event
   #:driver-cb-wm-delete-event
   ))

(defpackage :cldk-kernel
  (:use :common-lisp :cldk-driver)
  (:export
   ;;
   ;; kernel
   ;;
   #:*kernel-mode*
   #:check-kernel-mode
   #:check-user-mode
   #:kerneled-driver-mixin
   #:within-kernel-mode
   #:within-user-mode
   #:kernel-call
   #:kernel-callback

   #:lparallel-kernel-call-mixin
   #:next-kernel-cal
   #:process-next-kernel-calls
   
   #:lparallel-kernel-callback-mixin
   #:next-kernel-callback
   #:process-next-kernel-callback-loop
   
   ;;
   ;; kernel display
   ;;
   #:kerneled-buffer-mixin
   #:kerneled-window-mixin
   #:kerneled-buffered-window-mixin
   #:initialize-buffered-window
   
   #:window-obuffer

   #:server-cursor-table
   #:kernel-kwindows

   #:k-refresh-windows
   ))

(defpackage :cldk-server
  (:use :common-lisp :cldk-driver :cldk-kernel)
  (:export
   #:server
   #:start-server
   #:stop-server
   #:kill-server
   #:display-server
   #:EVENT-SERVER-MIXIN
   #:COMMAND-SERVER-MIXIN
   #:SERVER-WITH-THREAD-MIXIN
   #:server-kernel
   #:event-driver-mixin
   #:multi-thread-display-server
   #:single-thread-display-server
   #:start-server
   #:process-next-calls
   #:server-stopping-p
   #:server-object
   #:K-PROCESS-NEXT-DRIVER-EVENTS
   #:MAP-OVER-RECTANGLE-SET-REGIONS
   #:CALLBACK-QUEUE-WITH-THREAD-MIXIN
   #:COMMAND-QUEUE-MIXIN
   #:SERVER-LOOP-STEP
   #:server-loop
   #:callback-loop-fn
   #:SERVER-FORCE-OUTPUT
   ;;#:BUFFER-IMAGE-MIXIN
   ;;#:UPDATED-REGION-SET
   ))
(defpackage :cldk-internals
  (:use :cldk :cldk-driver :cldk-kernel :cldk-server
        :common-lisp)
  (:nicknames :cldki))
