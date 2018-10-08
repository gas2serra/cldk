(in-package :cl-user)
(defpackage #:cldk-asd
  (:use :cl :asdf))
(in-package #:cldk-asd)

(defsystem #:cldk-display-sdl2/core
  :version "0.3"
  :author "Alessandro Serra (gas2serra@gmail.com)"
  :license "LGPL"
  :depends-on (#:sdl2)
  :components ((:module "src/display/sdl2"
                        :components
                        ((:file "package")))))

(defsystem #:cldk-display-sdl2/driver
  :version "0.3"
  :author "Alessandro Serra (gas2serra@gmail.com)"
  :license "LGPL"
  :depends-on (#:cldk-display/driver #:cldk-display-sdl2/core)
  :components ((:module "src/display/sdl2"
                        :components
                        ((:file "input-keysyms-common")
                         (:file "input-keysymdef"
                                :depends-on ("input-keysyms-common"))
                         (:file "input-keysyms"
                                :depends-on ("input-keysyms-common"))
                         (:file "input")
                         (:file "driver"
                                :depends-on ("input"))))))

(defsystem #:cldk-display-sdl2/kernel
  :version "0.3"
  :author "Alessandro Serra (gas2serra@gmail.com)"
  :license "LGPL"
  :depends-on (#:cldk-display/kernel #:cldk-display-sdl2/driver)
  :components ((:module "src/display/sdl2"
                        :components
                        ())))

(defsystem #:cldk-display-sdl2
  :version "0.3"
  :author "Alessandro Serra (gas2serra@gmail.com)"
  :license "LGPL"
  :depends-on (:cldk-display-sdl2/driver :cldk-display-sdl2/kernel)
  :components ()
  :description "Common Lisp Drawing Kit")
