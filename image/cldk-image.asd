(defsystem #:cldk-image
    :description "Support for raster images McCLIM."
    :depends-on (#:cldk-image/core
                 #:cldk-image/two-dim-array
                 #:cldk-image/opticl))

(defsystem #:cldk-image/core
  :depends-on (#:cl-colors)
  :serial t
  :components ((:file "package")
               (:file "color")
               (:file "region")
               (:file "rectangle-set")
               (:file "image")
               (:file "image-prim")
               (:file "pixeled-design")
               (:file "image-ops")
               (:file "image-fast-ops")))

(defsystem #:cldk-image/two-dim-array
  :depends-on (#:cldk-image/core)
  :serial t
  :components ((:module "two-dim-array"
                        :serial t
                        :components
                        ((:file "two-dim-array-image")))))

(defsystem #:cldk-image/opticl
  :depends-on (#:cldk-image/two-dim-array #:opticl)
  :serial t
  :components ((:module "opticl"
                        :serial t
                        :components
                        ((:file "opticl-image")))))
