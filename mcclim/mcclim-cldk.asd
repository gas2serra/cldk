(defsystem #:mcclim-cldk
    :depends-on (#:cldk
                 #:mcclim-backend-common
		 #:mcclim-render
                 #:mcclim-render/backend)
    :components
    ((:file "package")
     (:file "graft" :depends-on ("package"))
     (:file "event-handler" :depends-on ("package"))
     (:file "port" :depends-on ("package"))
     (:file "frame-manager" :depends-on ("port" "package" "mirrored-sheets"))
     (:file "mirror" :depends-on ("port" "package"))
     (:file "mirrored-sheets" :depends-on ("port" "package" "mirror"))
     (:file "medium" :depends-on ("package"))))

(defsystem #:mcclim-cldk/clx
    :depends-on (#:mcclim-cldk #:cldk-clx)
    :components
    ((:module "clx"
              :components
              ((:file "port")))))

(defsystem #:mcclim-cldk/sdl2
    :depends-on (#:mcclim-cldk #:cldk-sdl2)
    :components
    ((:module "sdl2"
              :components
              ((:file "port")))))
