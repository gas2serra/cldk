

(defpackage :cldk-render
  (:use :cldk)
  (:import-from :cldk
                   ;; colors
   #:octet
   #:octet-mult
   #:octet-blend-function
   #:octet-rgba-blend-function
   #:octet-rgb-blend-function
   #:octet-gray-blend-function
   #:octet-alpha-blend-function
   #:rgba->rgb
   #:rgba->gray
   #:rgba->gray-alpha
   #:rgba->alpha
   #:rgb->rgba
   #:rgb->gray
   #:rgb->alpha
   #:gray->rgba
   #:gray->rgb
   #:gray->alpha
   #:alpha->gray
   ;; image
   #:image
   #:image-width
   #:image-height
   #:image-medium
   #:image-mixin
   #:image-pixels
   #:pixels
   #:rgba-image-mixin
   #:rgb-image-mixin
   #:gray-image-mixin
   #:image-rgba-get-fn
   #:image-rgba-set-fn
   #:image-rgb-get-fn
   #:image-rgb-set-fn
   #:image-gray-get-fn
   #:image-gray-set-fn
   #:image-type
   #:basic-image
   #:read-image
   #:write-image
   #:define-image-file-reader
   #:image-format-read-supported-p
   #:define-image-file-writer
   #:image-format-write-supported-p
   #:image-rgba-blend-fn
   #:image-rgb-blend-fn
   #:image-gray-blend-fn
   #:image-rgba-xor-blend-fn
   #:image-rgb-xor-blend-fn
   #:image-gray-xor-blend-fn
   #:image-alpha-get-fn
   #:image-alpha-set-fn
   #:image-rgba-set-span-fn
   #:image-rgb-set-span-fn
   #:image-gray-set-span-fn
   #:image-rgba-blend-span-fn
   #:image-rgb-blend-span-fn
   #:image-gray-blend-span-fn
   #:image-rgba-xor-blend-span-fn
   #:image-rgb-xor-blend-span-fn
   #:image-gray-xor-blend-span-fn
   ;; operations
   #:make-image
   #:coerce-image
   #:clone-image
   #:copy-image   
   #:blend-image
   #:crop-image
   #:coerce-alpha-channel
   #:clone-alpha-channel
   #:set-image-color
   #:set-image
   #:fill-image-color
   #:fill-image

   #:do-image
   #:%image-get-fn
   #:%image-set-fn
   #:%image-set-span-fn
   #:%image-blend-fn
   #:%image-xor-blend-fn
   #:%image-blend-span-fn
   #:%image-xor-blend-span-fn
   #:%call-image-rgba-get-fn
   #:%call-image-rgb-get-fn
   #:%call-image-gray-get-fn
   #:%call-set-image-fn
   #:%call-set-image-span-fn
   #:%call-blend-image-fn
   #:%call-blend-image-span-fn

   #:pixeled-design-fn
   #:pixeled-design
   #:pixeled-design-region
   #:pixeled-design-rgba-get-fn
   #:pixeled-design-rgba-get-unsafe-fn
   #:pixeled-uniform-design
   #:make-pixeled-uniform-design
   #:pixeled-uniform-design-red
   #:pixeled-uniform-design-green
   #:pixeled-uniform-design-blue
   #:pixeled-uniform-design-alpha
   )
  (:export
      ;;;
   ;;; images
   ;;;
   ;; colors
   #:octet
   #:octet-mult
   #:octet-blend-function
   #:octet-rgba-blend-function
   #:octet-rgb-blend-function
   #:octet-gray-blend-function
   #:octet-alpha-blend-function
   #:rgba->rgb
   #:rgba->gray
   #:rgba->gray-alpha
   #:rgba->alpha
   #:rgb->rgba
   #:rgb->gray
   #:rgb->alpha
   #:gray->rgba
   #:gray->rgb
   #:gray->alpha
   #:alpha->gray
   ;; image
   #:image
   #:image-width
   #:image-height
   #:image-medium
   #:image-mixin
   #:image-pixels
   #:pixels
   #:rgba-image-mixin
   #:rgb-image-mixin
   #:gray-image-mixin
   #:image-rgba-get-fn
   #:image-rgba-set-fn
   #:image-rgb-get-fn
   #:image-rgb-set-fn
   #:image-gray-get-fn
   #:image-gray-set-fn
   #:image-type
   #:basic-image
   #:read-image
   #:write-image
   #:define-image-file-reader
   #:image-format-read-supported-p
   #:define-image-file-writer
   #:image-format-write-supported-p
   #:image-rgba-blend-fn
   #:image-rgb-blend-fn
   #:image-gray-blend-fn
   #:image-rgba-xor-blend-fn
   #:image-rgb-xor-blend-fn
   #:image-gray-xor-blend-fn
   #:image-alpha-get-fn
   #:image-alpha-set-fn
   #:image-rgba-set-span-fn
   #:image-rgb-set-span-fn
   #:image-gray-set-span-fn
   #:image-rgba-blend-span-fn
   #:image-rgb-blend-span-fn
   #:image-gray-blend-span-fn
   #:image-rgba-xor-blend-span-fn
   #:image-rgb-xor-blend-span-fn
   #:image-gray-xor-blend-span-fn
   ;; operations
   #:make-image
   #:coerce-image
   #:clone-image
   #:copy-image   
   #:blend-image
   #:crop-image
   #:coerce-alpha-channel
   #:clone-alpha-channel
   #:set-image-color
   #:set-image
   #:fill-image-color
   #:fill-image

   #:do-image
   #:%image-get-fn
   #:%image-set-fn
   #:%image-set-span-fn
   #:%image-blend-fn
   #:%image-xor-blend-fn
   #:%image-blend-span-fn
   #:%image-xor-blend-span-fn
   #:%call-image-rgba-get-fn
   #:%call-image-rgb-get-fn
   #:%call-image-gray-get-fn
   #:%call-set-image-fn
   #:%call-set-image-span-fn
   #:%call-blend-image-fn
   #:%call-blend-image-span-fn


  #:pixeled-design-fn
   #:pixeled-design
   #:pixeled-design-region
   #:pixeled-design-rgba-get-fn
   #:pixeled-design-rgba-get-unsafe-fn
   #:pixeled-uniform-design
   #:make-pixeled-uniform-design
   #:pixeled-uniform-design-red
   #:pixeled-uniform-design-green
   #:pixeled-uniform-design-blue
   #:pixeled-uniform-design-alpha
   
   ;; colors
   #:color->octets
   #:octets->color
   ;; image
   #:image-width
   #:image-height
   #:image-rgba-get-fn
   #:image-rgba-set-fn
   #:image-rgb-get-fn
   #:image-rgb-set-fn
   #:image-gray-get-fn
   #:image-gray-set-fn
   #:image-type
   #:image-medium
   #:draw-image*
   #:medium-draw-image*
   #:make-image-design
   #:make-image-pattern
   ;; design
   #:make-pixeled-design
   ;; image ops
   #:image-pixels
   #:read-image
   #:write-image
   #:image-format-read-supported-p
   #:image-format-write-supported-p
   #:make-image
   #:coerce-image
   #:clone-image
   #:copy-image
   #:blend-image
   #:crop-image
   #:set-image
   #:fill-image
   #:coerce-alpha-channel
   #:clone-alpha-channel
   #:copy-alpha-channel
   ;; two dimensional array image
   #:two-dim-array-image
   #:rgb-image
   #:rgba-image
   #:gray-image
   ;; opticl image
   #:opticl-image
   #:opticl-rgb-image
   #:opticl-rgba-image
   #:opticl-gray-image
   #:make-opticl-image
   ;; ch image
   #:make-ch-image-adapter
   #:make-opticl-image-adapter
   #:make-imago-image-adapter
   ))

(defpackage :cldk-render-extensions
  (:use :cldk)
  (:export
   ;; colors
   #:octet
   #:color-octet-xor
   #:octet-mult
   #:octet-blend-function
   #:octet-rgba-blend-function
   #:octet-rgb-blend-function
   #:octet-gray-blend-function
   #:octet-alpha-blend-function
   #:color-value->octet
   #:color-octet->value
   #:rgba->rgb
   #:rgba->gray
   #:rgba->gray-alpha
   #:rgba->alpha
   #:rgb->rgba
   #:rgb->gray
   #:rgb->alpha
   #:gray->rgba
   #:gray->rgb
   #:gray->alpha
   #:alpha->gray
   ;; image
;;   #:image
;;   #:image-mixin
;;   #:rgb-image-mixin
;;   #:rgba-image-mixin
;;   #:gray-image-mixin
   #:image-rgba-blend-fn
   #:image-rgb-blend-fn
   #:image-rgb-xor-blend-fn
   #:image-gray-blend-fn
   #:image-gray-alpha-get-fn
   #:image-alpha-get-fn
   #:image-alpha-set-fn
   #:image-alpha-blend-fn
;;   #:basic-image
   #:image-design
   #:image-pattern
   #:*default-image-medium*
   ;; design
   #:pixeled-design
   #:pixeled-uniform-design
   #:pixeled-uniform-design-red
   #:pixeled-uniform-design-green
   #:pixeled-uniform-design-blue
   #:pixeled-uniform-design-alpha
   #:pixeled-flipping-design
   #:pixeled-design-rgba-get-fn
   #:pixeled-design-fn
   #:%make-pixeled-design
   ;; two dimensional array image
   ;; opticl image
   ))

(defpackage :cldk-render-internals
  (:use #:clim #:clim-lisp #:cldk-render #:cldk-render-extensions)
  (:import-from :clim-internals
                #:standard-color
                #:named-color
                #:standard-flipping-ink
                #:%transparent-ink
                #:standard-opacity
                #:opacity-value
                #:pattern
                #:indexed-pattern
                #:rectangular-tile
                #:rectangular-tile-design
                #:transformed-design
                #:transformed-design-design
                #:transformed-design-transformation
                #:with-transformed-position
                #:in-compositum
                #:out-compositum
                #:over-compositum
                #:compositum-ink
                #:compositum-mask
                #:compositum-foreground
                #:compositum-background
                #:def-grecording
                #:defmethod*
                #:output-record-position
                #:defrecord-predicate
                #:with-standard-rectangle*
                #:coordinate=
                #:if-supplied
                ;; backend
                #:destroy-mirror
                #:realize-mirror
                #:mirrored-pixmap
                #:port-register-mirror
                #:port-lookup-mirror
                #:port-lookup-sheet
                #:pixmap-mirror
                #:pixmap-medium)
  (:import-from :mcclim-truetype
                #:truetype-font-size
                #:truetype-font-face
                #:glyph-pixarray
                #:ensure-gethash
                #:invoke-with-truetype-path-restart
                #:*truetype-font-path*
                #:*family-names*
                #:zpb-ttf-font-loader
                #:*zpb-font-lock*
                #:*fontconfig-faces*
                #:*families/faces*
                #:truetype-device-font-name
                #:fontconfig-font-name
                #:make-truetype-device-font-name
                #:make-fontconfig-font-name
                #:truetype-font-family
                #:truetype-font
                #:truetype-face
                #:truetype-font-size
                #:truetype-font-ascent
                #:truetype-font-descent
                #:zpb-ttf-font-units->pixels)
  (:import-from :clim-backend
                #:port-set-mirror-region
                #:port-set-mirror-transformation)
  (:export))
