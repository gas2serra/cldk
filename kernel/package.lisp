(in-package :common-lisp-user)

(defpackage :cldk-kernel
  (:use :common-lisp :cldk-driver)
  (:export
   #:*kernel-mode*
   #:check-kernel-mode
   #:check-user-mode
   #:kerneled-driver-mixin
   #:within-kernel-mode
   #:within-user-mode
   #:kernel-call
   #:kernel-callback

   #:lparallel-kernel-call-mixin
   #:kernel-call-queue
   #:exec-next-kernel-call
   
   #:lparallel-kernel-callback-mixin
   #:kernel-callback-queue
   #:exec-next-kernel-callback
   
   #:empty-lparallel-queue

   #:*default-event-handler*
   #:default-display-callback-handler
   #:display-kernel-mixin
   #:event-handler

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
   #:create-buffer
   #:destroy-buffer
   #:update-buffer

   #:kerneled-window-mixin

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

   #:kerneled-buffered-window-mixin
   #:buffered-window
   #:initialize-buffered-window
   #:create-buffered-window
   #:copy-image-to-buffered-window
   #:flush-buffered-window
   #:window-obuffer

   #:server-cursor-table
   #:kernel-kwindows
   #:screen-num
   #:screen-size
   #:screen-dpi
   #:screen-pointer-position
   #:avaiable-cursor-names
   
   ))
