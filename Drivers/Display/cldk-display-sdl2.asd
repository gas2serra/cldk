(in-package :cl-user)
(defpackage #:cldk-asd
  (:use :cl :asdf))
(in-package #:cldk-asd)

(defsystem #:cldk-display-sdl2
  :version "0.3"
  :author "Alessandro Serra (gas2serra@gmail.com)"
  :license "LGPL"
  :depends-on (#:sdl2 #:cldk-display #:static-vectors)
  :components ((:module "SDL2"
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
                                :depends-on ("input")))))
  :description "Common Lisp Drawing Kit: SDL2 Display Driver")
