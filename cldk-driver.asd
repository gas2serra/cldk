(in-package :cl-user)
(defpackage #:cldk-asd
  (:use :cl :asdf))
(in-package #:cldk-asd)

(defsystem #:cldk-driver
  :version "0.3"
  :author "Alessandro Serra (gas2serra@gmail.com)"
  :license "LGPL"
  :depends-on (#:bordeaux-threads #:log4cl)
  :components ((:module "drivers"
                        :components
                        ((:file "package")
                         (:file "driver"
                                :depends-on ("package"))
                         (:file "threaded-driver"
                                :depends-on ("package" "driver")))))
  :description "Common Lisp Drawing Kit")

(defsystem #:cldk-driver/display
  :version "0.3"
  :author "Alessandro Serra (gas2serra@gmail.com)"
  :license "LGPL"
  :depends-on (#:cldk-driver)
  :components ((:module "drivers"
                        :components
                        ((:file "display-driver")))))
               
(defsystem #:cldk-driver/clx
  :version "0.3"
  :author "Alessandro Serra"
  :license "LGPL"
  :depends-on (#:cldk-driver/display #:clx #:cffi)
  :components ((:module "drivers/clx-display"
                        :components
                        ((:file "package")
                         (:file "keysyms-common"
                                :depends-on ("package"))
                         (:file "keysymdef"
                                :depends-on ("package" "keysyms-common"))
                         (:file "keysyms"
                                :depends-on ("package" "keysyms-common"))
                         (:file "input"
                                :depends-on ("package"))
                         (:file "driver"
                                :depends-on ("package" "input")))))
  :description "CLX Backend for Common Lisp Drawing Kit")

(defsystem #:cldk-driver/sdl2
  :version "0.3"
  :author "Alessandro Serra"
  :license "LGPL"
  :depends-on (#:cldk-driver/display #:sdl2 #:cffi)
  :components ((:module "drivers/sdl2-display"
                        :components
                        ((:file "package")
                         (:file "keysyms-common"
                                :depends-on ("package"))
                         (:file "keysymdef"
                                :depends-on ("package" "keysyms-common"))
                         (:file "keysyms"
                                :depends-on ("package" "keysyms-common"))
                         (:file "input"
                                :depends-on ("package"))
                         (:file "driver"
                                :depends-on ("package" "input")))))
  :description "SDL2 Backend for Common Lisp Drawing Kit")
