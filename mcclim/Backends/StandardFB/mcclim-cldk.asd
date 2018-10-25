(defsystem #:mcclim-cldk
  :depends-on (#:cldk
               #:cldk-backend
                 #:mcclim-backend-common
                 #:mcclim-render
		 #:cldk-render
                 #:cldk-render/backend)
    :components
    ((:file "package")
     ;;(:file "event-handler" :depends-on ("package"))
     (:file "port" :depends-on ("package"))
     (:file "frame-manager" :depends-on ("port" "package" "mirrored-sheets"))
     (:file "mirror" :depends-on ("port" "package"))
     (:file "mirrored-sheets" :depends-on ("port" "package" "mirror"))
     (:file "medium" :depends-on ("package"))))
