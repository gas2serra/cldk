(in-package :cl-user)
(defpackage #:cldk-sdl2-asd
  (:use :cl :asdf))
(in-package #:cldk-sdl2-asd)

(defsystem #:cldk-sdl2
  :version "0.3"
  :author "Alessandro Serra"
  :license "LGPL"
  :depends-on (#:cldk/basic #:cldk-driver/sdl2 #:sdl2 #:cffi)
  :components ((:file "package")
               (:file "server" :depends-on ("package"))
               (:file "buffer" :depends-on ("package" "server")))
  :description "SDL2 Backend for Common Lisp Drawing Kit")
