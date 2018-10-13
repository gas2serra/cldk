(in-package :cldk-display-xcb)

(cffi:define-foreign-library libxcb
    (:unix "libxcb.so"))
(cffi:use-foreign-library libxcb)

(cffi:define-foreign-library libxcb-icccm
  (:unix "libxcb-icccm.so"))
(cffi:use-foreign-library libxcb-icccm)

(cffi:define-foreign-library libxcb-image
    (:unix "libxcb-image.so"))
(cffi:use-foreign-library libxcb-image)
