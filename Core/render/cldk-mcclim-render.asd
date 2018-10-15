(defsystem #:cldk-mcclim-render
    :description "Support for raster images McCLIM."
    :depends-on (#:cldk-mcclim-render/core
                 #:cldk-mcclim-render/two-dim-array
                 #:cldk-mcclim-render/opticl))

(defsystem #:cldk-mcclim-render/core
  :depends-on (#:clim-basic
               #:mcclim-image
               #:mcclim-fonts/truetype)
  :serial t
  :components ((:file "package")
               (:file "color")
               (:file "image")
               (:file "image-prim")
               (:file "pixeled-design")
               (:file "image-ops")
               (:file "recording")
               (:file "compatibility")))

(defsystem #:cldk-mcclim-render/render
    :depends-on (#:cldk-mcclim-render/core)
    :components
    ((:module "render"
              :serial t
              :components
              ((:file "render")
               (:file "fonts")))))

(defsystem #:cldk-mcclim-render/cl-vectors
    :depends-on (#:clim-basic #:mcclim-fonts/truetype  #:cldk-mcclim-render/render #:cldk-mcclim-render/two-dim-array #:cldk-mcclim-render/opticl #:cldk-mcclim-render/render #:mcclim-bezier)
    :components
    ((:module "cl-vectors"
              :serial t
              :components
              ((:file "vectors-paths")
               (:file "vectors")
               (:file "vectors-image-ops")
               (:file "prim-arc")
               (:file "prim-text")
               (:file "vectors-render")))))

(defsystem #:cldk-mcclim-render/two-dim-array
  :depends-on (#:cldk-mcclim-render/core)
  :serial t
  :components ((:module "two-dim-array"
                        :serial t
                        :components
                        ((:file "two-dim-array-image")))))

(defsystem #:cldk-mcclim-render/opticl
  :depends-on (#:cldk-mcclim-render/two-dim-array #:opticl)
  :serial t
  :components ((:module "opticl"
                        :serial t
                        :components
                        ((:file "image-adapter")
                         (:file "opticl-image")
                         (:file "opticl-image-opt")))))

(defsystem #:cldk-mcclim-render/backend
    :depends-on (#:cldk-mcclim-render/cl-vectors #:mcclim-bezier)
    :components
    ((:module "backend"
              :serial t
              :components
              ((:file "mirror")
               (:file "mirrored-sheet")
               (:file "pixmap")
               (:file "medium")
               (:file "fonts")
               (:file "port")))))

(defsystem #:cldk-mcclim-render/clx
    :depends-on (#:mcclim-clx
                 #:cldk-mcclim-render/backend
                 #:cldk-mcclim-render/two-dim-array
                 #:cldk-mcclim-render/opticl)
  :serial t
  :components ((:module "clx"
                        :serial t
                        :components
                        ((:file "clx-image")
                         (:file "clx-extension")))))

(defsystem #:cldk-mcclim-render/imago
    :depends-on (#:cldk-mcclim-render/core
                 #:imago)
  :serial t
  :components ((:module "imago"
                        :serial t
                        :components
                        ((:file "image-adapter")))))

(defsystem #:cldk-mcclim-render/ch-image
    :depends-on (#:cldk-mcclim-render/core
                 #:ch-image)
  :serial t
  :components ((:module "ch-image"
                        :serial t
                        :components
                        ((:file "image-adapter")))))

