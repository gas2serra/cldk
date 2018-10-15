(defsystem #:mcclim-cldk
  :depends-on (#:cldk
               #:cldk-backend
                 #:mcclim-backend-common
                 #:mcclim-render
		 #:cldk-mcclim-render
                 #:cldk-mcclim-render/backend)
    :components
    ((:file "package")
     (:file "graft" :depends-on ("package"))
     (:file "event-handler" :depends-on ("package"))
     (:file "port" :depends-on ("package"))
     (:file "frame-manager" :depends-on ("port" "package" "mirrored-sheets"))
     (:file "mirror" :depends-on ("port" "package"))
     (:file "mirrored-sheets" :depends-on ("port" "package" "mirror"))
     (:file "medium" :depends-on ("package"))))

#|
(defsystem #:mcclim-cldk/clx
    :depends-on (#:mcclim-cldk #:cldk-clx #:cldk-clx-backend)
    :components
    ((:module "clx"
              :components
              ((:file "port")))))


(defsystem #:mcclim-cldk/sdl2
    :depends-on (#:mcclim-cldk #:cldk-sdl2 #:cldk-sdl2-backend)
    :components
    ((:module "sdl2"
              :components
              ((:file "port")))))

(defsystem #:mcclim-cldk/xcb
    :depends-on (#:mcclim-cldk #:cldk-xcb #:cldk-xcb-backend)
    :components
    ((:module "xcb"
              :components
              ((:file "port")))))
|#
