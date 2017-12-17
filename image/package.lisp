
(defpackage :cldk-image
  (:use)
  (:export
   ;; colors
   #:color->octets
   ;; image
   #:image
   #:image-width
   #:image-height
   #:draw-image*
   #:medium-draw-image*
   #:make-image-design
   #:make-image-pattern
   #:image-pixels
   #:image-rgba-get-fn
   #:image-rgba-set-fn
   #:image-rgba-blend-fn
   #:image-rgb-get-fn
   #:image-rgb-set-fn
   #:image-rgb-blend-fn
   #:image-rgb-xor-blend-fn
   #:image-gray-get-fn
   #:image-gray-set-fn
   #:image-gray-blend-fn
   #:image-gray-alpha-get-fn
   #:image-alpha-get-fn
   #:image-alpha-set-fn
   #:image-alpha-blend-fn
   ;; design
   #:make-pixeled-design
   ;; image ops
   #:read-image
   #:write-image
   #:image-format-read-supported-p
   #:image-format-write-supported-p
   #:make-image
   #:coerce-image
   #:clone-image
   #:copy-image
   #:coerce-alpha-channel
   #:clone-alpha-channel
   #:copy-alpha-channel
   #:blend-image
   #:crop-image
   #:fill-image
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
   #:map-over-rectangle-set-regions
   #:rectangle->rectangle-set
   #:rectangle-set-union
   ))

(defpackage :cldk-image-extensions
  (:use)
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
   ;; image
   #:rgb-image-mixin
   #:rgba-image-mixin
   #:gray-image-mixin
   #:find-image-class
   #:*default-family*
   #:image-family
   #:drawable-image
   #:map-rgb-color
   #:basic-image
   #:image-design
   #:image-pattern
   #:image-pixels-type
   #:image-rgb-get-code
   #:image-rgb-set-code
   #:image-rgb-blend-code
   #:image-rgb-xor-blend-code
   #:image-rgba-get-code
   #:image-rgba-set-code
   #:image-rgba-blend-code
   #:image-gray-get-code
   #:image-gray-set-code
   #:image-gray-alpha-get-code
   #:image-gray-blend-code
   #:image-alpha-get-code
   #:image-alpha-set-code
   #:image-alpha-blend-code
   ;; design
   #:pixeled-design
   #:pixeled-uniform-design
   #:pixeled-uniform-design-red
   #:pixeled-uniform-design-green
   #:pixeled-uniform-design-blue
   #:pixeled-uniform-design-alpha
   #:pixeled-flipping-design
   #:pixeled-rgba-fn
   #:pixeled-design-fn
   #:%make-pixeled-design
   ;; image ops
   #:def-rgb-image-primitives
   #:def-rgba-image-primitives
   #:def-gray-image-primitives
   #:def-fast-rgba-copy-image
   #:def-fast-rgb-copy-image
   #:def-fast-gray-copy-image
   #:def-fast-copy-alpha-channel
   #:def-fast-fill-image-without-stencil
   #:def-fast-fill-image-with-stencil
   ;; two dimensional array image
   #:rgb-image-pixels
   #:rgba-image-pixels
   #:single-channel-image-pixels
   ;; opticl image
   #:opticl-rgb-image-pixels
   #:opticl-rgba-image-pixels
   #:opticl-single-channel-image-pixels
   ))

(defpackage :cldk-image-internals
  (:use #:common-lisp #:cldk-image #:cldk-image-extensions)
  (:export))
