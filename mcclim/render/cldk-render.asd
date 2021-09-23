(defsystem #:cldk-render
    :description "Support for raster images McCLIM."
    :depends-on (#:cldk-render/core
                 #:cldk-render/two-dim-array
                 #:cldk-render/opticl))

(defsystem #:cldk-render/core
    :depends-on (#:cldk-core
                 #:clim-basic
;;                 #:mcclim-image
                 #:mcclim-fonts/truetype)
  :serial t
  :components ((:file "package")
               ;;(:file "color")
               (:file "image")
               ;;(:file "image-prim")
               (:file "pixeled-design")
               (:file "image-ops")
               (:file "recording")
               (:file "compatibility")))

(defsystem #:cldk-render/render
    :depends-on (#:cldk-render/core)
    :components
    ((:module "render"
              :serial t
              :components
              ((:file "render")
               (:file "fonts")))))

(defsystem #:cldk-render/cl-vectors
    :depends-on (#:clim-basic #:mcclim-fonts/truetype  #:cldk-render/render #:cldk-render/two-dim-array #:cldk-render/opticl #:cldk-render/render #:mcclim-bezier)
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

(defsystem #:cldk-render/two-dim-array
  :depends-on (#:cldk-render/core)
  :serial t
  :components ((:module "two-dim-array"
                        :serial t
                        :components
                        ((:file "two-dim-array-image")))))

(defsystem #:cldk-render/opticl
  :depends-on (#:cldk-render/two-dim-array #:opticl)
  :serial t
  :components ((:module "opticl"
                        :serial t
                        :components
                        ((:file "image-adapter")
                         (:file "opticl-image")
                         (:file "opticl-image-opt")))))

(defsystem #:cldk-render/backend
    :depends-on (#:cldk-render/cl-vectors #:mcclim-bezier)
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

(defsystem #:cldk-render/clx
    :depends-on (#:mcclim-clx
                 #:cldk-render/backend
                 #:cldk-render/two-dim-array
                 #:cldk-render/opticl)
  :serial t
  :components ((:module "clx"
                        :serial t
                        :components
                        ((:file "clx-image")
                         (:file "clx-extension")))))

(defsystem #:cldk-render/imago
    :depends-on (#:cldk-render/core
                 #:imago)
  :serial t
  :components ((:module "imago"
                        :serial t
                        :components
                        ((:file "image-adapter")))))

(defsystem #:cldk-render/ch-image
    :depends-on (#:cldk-render/core
                 #:ch-image)
  :serial t
  :components ((:module "ch-image"
                        :serial t
                        :components
                        ((:file "image-adapter")))))

