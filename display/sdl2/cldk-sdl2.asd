(in-package :cl-user)
(defpackage #:cldk-sdl2-asd
  (:use :cl :asdf))
(in-package #:cldk-sdl2-asd)

(defsystem #:cldk-sdl2
  :version "0.3"
  :author "Alessandro Serra"
  :license "LGPL"
  :depends-on (#:cldk/basic #:sdl2 #:cffi)
  :components ((:file "package")
               (:file "keysyms-common" :depends-on ("package"))
               (:file "keysymdef" :depends-on ("package" "keysyms-common"))
               (:file "keysyms" :depends-on ("package" "keysyms-common"))
               (:file "server" :depends-on ("package" "driver"))
               (:file "input" :depends-on ("package"))
               (:file "image" :depends-on ("package"))
               (:file "driver" :depends-on ("package" "input" "image")))
  :description "SDL2 Backend for Common Lisp Drawing Kit")
