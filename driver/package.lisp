(in-package :common-lisp-user)

(defpackage :cldk-driver
  (:use :common-lisp)
  (:export

   ;;  driver
   #:driver
   #:driver-options
   #:driver-callback-handler
   #:driver-start
   #:driver-stop
   #:driver-kill
   #:driver-ping
   #:driver-force-output
   #:driver-process-next-event
   ;; callback hanlder
   #:driver-callback-handler
   #:register-driver-object
   #:unregister-driver-object
   #:lookup-driver-object
   ;; driver object
   #:driver-object
   #:driver-object-id

   ;; threaded driver
   #:single-threaded-driver-mixin
   #:multi-threaded-driver-mixin
   #:with-driver-locked
   
   ;; display driver
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
