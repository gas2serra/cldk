(in-package #:asdf-user)

;;; Cldk-Examples depends on having at least one backend loaded.
(defsystem #:cldk-examples
    :depends-on (#:mcclim #:mcclim-layouts/tab
                          #:cldk-render
                          #:cldk-raster-image
                          #:mcclim-raster-image
                          #:mcclim-bezier #:closer-mop)
    :components
    ((:file "package")
     (:file "demodemo")
     (:file "drawing-tests")
     (:file "render-image-tests")))
