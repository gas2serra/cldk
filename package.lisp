(in-package :common-lisp-user)

(defpackage :cldk
  (:use :common-lisp)
  (:export
   ;;;
   ;;; images
   ;;;
   ;; colors
   #:octet
   #:octet-mult
   #:octet-blend-function
   #:octet-rgba-blend-function
   #:octet-rgb-blend-function
   #:octet-gray-blend-function
   #:octet-alpha-blend-function
   #:rgba->rgb
   #:rgba->gray
   #:rgba->gray-alpha
   #:rgba->alpha
   #:rgb->rgba
   #:rgb->gray
   #:rgb->alpha
   #:gray->rgba
   #:gray->rgb
   #:gray->alpha
   #:alpha->gray
   ;; image
   #:image
   #:image-width
   #:image-height
   #:image-medium
   #:image-mixin
   #:image-pixels
   #:pixels
   #:rgba-image-mixin
   #:rgb-image-mixin
   #:gray-image-mixin
   #:image-rgba-get-fn
   #:image-rgba-set-fn
   #:image-rgb-get-fn
   #:image-rgb-set-fn
   #:image-gray-get-fn
   #:image-gray-set-fn
   #:image-type
   #:basic-image
   #:read-image
   #:write-image
   #:define-image-file-reader
   #:image-format-read-supported-p
   #:define-image-file-writer
   #:image-format-write-supported-p
   #:image-rgba-blend-fn
   #:image-rgb-blend-fn
   #:image-gray-blend-fn
   #:image-rgba-xor-blend-fn
   #:image-rgb-xor-blend-fn
   #:image-gray-xor-blend-fn
   #:image-alpha-get-fn
   #:image-alpha-set-fn
   #:image-rgba-set-span-fn
   #:image-rgb-set-span-fn
   #:image-gray-set-span-fn
   #:image-rgba-blend-span-fn
   #:image-rgb-blend-span-fn
   #:image-gray-blend-span-fn
   #:image-rgba-xor-blend-span-fn
   #:image-rgb-xor-blend-span-fn
   #:image-gray-xor-blend-span-fn
   ;; operations
   #:make-image
   #:coerce-image
   #:clone-image
   #:copy-image   
   #:blend-image
   #:crop-image
   #:coerce-alpha-channel
   #:clone-alpha-channel
   #:set-image-color
   #:set-image
   #:fill-image-color
   #:fill-image

   #:do-image
   #:%image-get-fn
   #:%image-set-fn
   #:%image-set-span-fn
   #:%image-blend-fn
   #:%image-xor-blend-fn
   #:%image-blend-span-fn
   #:%image-xor-blend-span-fn
   #:%call-image-rgba-get-fn
   #:%call-image-rgb-get-fn
   #:%call-image-gray-get-fn
   #:%call-set-image-fn
   #:%call-set-image-span-fn
   #:%call-blend-image-fn
   #:%call-blend-image-span-fn

   #:pixeled-design-fn
   #:pixeled-design
   #:pixeled-design-region
   #:pixeled-design-rgba-get-fn
   #:pixeled-design-rgba-get-unsafe-fn
   #:pixeled-uniform-design
   #:make-pixeled-uniform-design
   #:pixeled-uniform-design-red
   #:pixeled-uniform-design-green
   #:pixeled-uniform-design-blue
   #:pixeled-uniform-design-alpha
   
   
      #:*default-server-path*
   #:find-server
   #:find-display-server
   #:server-path
   #:server-event-handler
   #:restart-server
   #:destroy-server
   #:map-over-server

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

   #:root
   #:create-root
   #:destroy-root
   #:root-size
   #:root-pointer-position
   
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
#|
   #:rgb-image
   #:copy-image
   #:image
   #:image-width
   #:image-height
   #:image-pixels
|#
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

(defpackage :cldk-driver-impl
  (:use :common-lisp)
  (:export
   ))

(defpackage :cldk-driver
  (:use :common-lisp)
  (:export
   ;;
   ;; driver
   ;;
   #:driver
   #:driver-options
   #:driver-callback-handler
   #:driver-start
   #:driver-stop
   #:driver-kill
   #:driver-ping
   #:driver-force-output
   #:driver-process-next-event
   #:driver-id
   #:driver-process-next-events
   #:register-driver-object
   #:unregister-driver-object
   #:lookup-driver-object
   #:start-driver
   #:stop-driver
   ;; driver object
   #:driver-object
   #:driver-object-id
   ;; threaded driver
   #:threaded-driver-mixin
   #:with-driver-locked
   #:single-threaded-driver-mixin
   #:multi-threaded-driver-mixin
   #:driver-with-thread-mixin
   #:driver-loop-step
   ;;
   ;; display drivers
   ;;
   #:display-driver
   #:display-driver-object
   #:display-driver-callback-handler
   
   #:driver-screen-index
   ;; display root
   #:driver-root
   #:driver-initialize-root
   #:driver-destroy-root
   #:driver-root-size
   #:driver-root-pointer-position
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
   #:driver-copy-image-to-window
   
   #:driver-grab-pointer
   #:driver-ungrab-pointer

   ;; display cursor
   #:driver-cursor
   #:driver-avaiable-cursor-names 
   #:driver-create-cursor
   #:driver-destroy-cursor
   #:driver-set-window-cursor
   
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



(defpackage :cldk-mirror
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
   #:kerneled-root-mixin
   #:kerneled-buffer-mixin
   #:kerneled-window-mixin
   #:kerneled-buffered-window-mixin
   #:initialize-buffered-window
   
   #:window-obuffer

   #:server-cursor-table
   #:kernel-kwindows

   #:refresh-windows
   ))

(defpackage :cldk-server
  (:use :common-lisp :cldk-driver :cldk-mirror)
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
  (:use :cldk :cldk-driver :cldk-driver-impl :cldk-mirror :cldk-server
        :common-lisp)
  (:nicknames :cldki))
