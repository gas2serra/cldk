(in-package :cl-user)
(defpackage #:cldk-sdl2-asd
  (:use :cl :asdf))
(in-package #:cldk-sdl2-asd)

(defsystem #:cldk-sdl2
  :version "0.3"
  :author "Alessandro Serra"
  :license "LGPL"
  :depends-on (#:cldk/basic #:cldk-driver-sdl2 #:cldk-display #:sdl2 #:cffi #:cldk-standard)
  :components
  ((:module "SDL2"
            :components
            ((:file "package")
             (:file "server" :depends-on ("package")))))
  :description "Common Lisp Drawing Kit: SDL2 Backend")
