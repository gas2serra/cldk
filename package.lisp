(in-package :common-lisp-user)

(defpackage :cldk
  (:use :common-lisp)
  (:import-from :cldk-kernel
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
                
                #:kerneled-buffer-mixin
                #:destroy-buffer

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

                #:buffered-window
                #:create-buffered-window
                #:copy-image-to-buffered-window
                #:flush-buffered-window
                #:kernel-kwindows

                #:screen-num
                #:screen-size
                #:screen-dpi
                #:screen-pointer-position
                #:avaiable-cursor-names
                )
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
   ))


(defpackage :cldk-extension
  (:use :common-lisp)
  (:import-from :cldk-kernel
                #:create-buffer)
  (:export
   #:server-kernel
   
   ;; event driver
   #:event-driver-mixin
   
   #:multi-thread-display-server
   #:single-thread-display-server
   #:start-server

   #:buffer
   #:create-buffer
   #:image-rgb-get-fn
   #:image-rgb-set-fn
   ))

(defpackage :cldk-internals
  (:use :cldk :cldk-driver :cldk-kernel :cldk-extension :common-lisp)
  (:nicknames :cldki))
