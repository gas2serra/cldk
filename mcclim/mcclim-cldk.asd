(defsystem #:mcclim-cldk
    :depends-on (#:cldk
                 #:mcclim-single-mirrored-standard
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
