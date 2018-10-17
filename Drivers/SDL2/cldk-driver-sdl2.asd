(in-package :cl-user)
(defpackage #:cldk-asd
  (:use :cl :asdf))
(in-package #:cldk-asd)

(defsystem #:cldk-driver-sdl2
  :version "0.3"
  :author "Alessandro Serra (gas2serra@gmail.com)"
  :license "LGPL"
  :depends-on (#:sdl2 #:static-vectors #:cldk-core/driver)
  :components
  ((:file "package")
   (:file "input-keysyms-common"
          :depends-on ("package"))
   (:file "input-keysymdef"
          :depends-on ("input-keysyms-common"))
   (:file "input-keysyms"
          :depends-on ("input-keysyms-common"))
   (:file "input"
          :depends-on ("package"))
   (:file "driver"
          :depends-on ("input"))
   (:file "image" :depends-on ("package" "driver")))
  :description "Common Lisp Drawing Kit: SDL2 Display Driver")
