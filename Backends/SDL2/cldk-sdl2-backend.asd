(in-package :cl-user)
(defpackage #:cldk-asd
  (:use :cl :asdf))
(in-package #:cldk-asd)

(defsystem #:cldk-sdl2-backend
    :version "0.3"
    :author "Alessandro Serra"
    :license "LGPL"
    :depends-on (#:cldk-backend #:cldk-driver-sdl2 #:mcclim-cldk)
    :components
    ((:file "package")
     (:file "buffer" :depends-on ("package" "port"))
     (:file "port" :depends-on ("package")))
    :description "Common Lisp Drawing Kit: SDL2 Backend")

