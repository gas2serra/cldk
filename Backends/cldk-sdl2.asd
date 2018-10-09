(in-package :cl-user)
(defpackage #:cldk-sdl2-asd
  (:use :cl :asdf))
(in-package #:cldk-sdl2-asd)

(defsystem #:cldk-sdl2
  :version "0.3"
  :author "Alessandro Serra"
  :license "LGPL"
  :depends-on (#:cldk/basic #:cldk-display-sdl2 #:cldk-display #:sdl2 #:cffi )
  :components
  ((:module "SDL2"
            :components
            ((:file "package")
             (:file "server" :depends-on ("package"))
             (:file "buffer" :depends-on ("package" "server")))))
  :description "Common Lisp Drawing Kit: SDL2 Backend")
