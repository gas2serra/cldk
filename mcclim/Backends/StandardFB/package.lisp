(in-package :common-lisp-user)

(defpackage :clim-fb
  (:use :clim :clim-lisp :clim-backend :cldk-backend :cldk-render-extensions)
  (:import-from :climi
                #:basic-medium
                #:port-allocate-pixmap
                #:port-deallocate-pixmap
                #:port-lookup-mirror
                #:port-register-mirror
                #:port-unregister-mirror
                #:realize-mirror
                #:destroy-mirror
                #:port-allocate-pixmap
                #:port-deallocate-pixmap
                #:mirrored-sheet-mixin
                #:sheet-with-medium-mixin
                #:permanent-medium-sheet-output-mixin
                #:application-frame
                #:make-pane-1
                #:top-level-sheet-pane
                #:unmanaged-top-level-sheet-pane
                #:menu-frame
                #:note-space-requirements-changed
                #:port-enable-sheet
                #:port-disable-sheet
                #:mirror-transformation
                #:bury-mirror
                #:raise-mirror
                #:destroy-mirror
                #:pointer-position
                #:port-grafts
                #:make-graft
                #:set-sheet-pointer-cursor
                #:port-force-output
                #:frame-managers
                #:pointer-scroll-event
                )

  (:import-from :cldk-render-internals
                #:render-medium-mixin
                #:render-port-mixin
                #:image-pixmap-mixin
                #:image-mirror-mixin
                #:image-mirror-image
                #:%make-image
                #:image-sheet-mixin
                #:image-sheet-mixin
                #:image-pixels
                #:image-pixmap-mixin
                #:image-mirror-mixin
                #:opticl-rgb-image-pixels
                )
  (:import-from :clim-standard
                #:standard-port
                #:standard-event-port-mixin
                #:mirrored-sheet-mixin
                #:%sheet-mirror-region
                #:%sheet-mirror-transformation
                #:standard-graft
                #:standard-pointer
                #:port-pointer-sheet
                #:pointer-grab-sheet 
		)
  
  (:export
   #:fb-medium
   #:fb-port
   #:fb-graft
   #:fb-mirror
   #:fb-pointer
   #:make-medium
   #:make-fb-mirror
   #:fb-pixmap
   #:realize-mirror.tofix
   #:fb-frame-manager
   #:fb-port-server
   #:fb-mirrored-sheet-mixin
   #:fb-frame-manager-mirrored-sheet-mixin-class
   #:tell-window-manager-about-space-requirements
   ))
   

