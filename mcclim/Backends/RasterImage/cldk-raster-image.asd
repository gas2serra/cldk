
(defsystem #:cldk-raster-image
    :depends-on (#:cldk-render
                 #:cldk-render/backend
                 #:cldk-render/render
               #:mcclim-backend-common)
    :serial t
    :components
    ((:file "package")
     (:file "graft")
     (:file "medium")
     (:file "top-level-pane")     
     (:file "port")
     (:file "stream")
     (:file "output-to-png")
     (:file "rgb-port")))
