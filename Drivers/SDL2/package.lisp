(in-package :common-lisp-user)

(defpackage :cldk-driver-sdl2
  (:use :cldk-driver :cldk :common-lisp)
  (:export
   #:sdl2-driver
   #:sdl2-driver-root
   #:sdl2-driver-window
   #:sdl2-driver-buffer
   #:surface
   ))
