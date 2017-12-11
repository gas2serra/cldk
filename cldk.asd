(in-package :cl-user)
(defpackage #:cldk-asd
  (:use :cl :asdf))
(in-package #:cldk-asd)

(defsystem #:cldk/core
  :version "0.3"
  :author "Alessandro Serra (gas2serra@gmail.com)"
  :license "LGPL"
  :depends-on (#:log4cl)
  :components ((:file "package")
               (:module "core"
                        :depends-on ("package")
                        :components
                        ((:file "driver")
                         (:file "server")
                         (:file "kernel"))))
  :description "Common Lisp Drawing Kit")

(defsystem #:cldk/image
  :version "0.3"
  :author "Alessandro Serra (gas2serra@gmail.com)"
  :license "LGPL"
  :depends-on (#:cldk/core)
  :components ((:module "image"
                        :components
                        ()))
  :description "Common Lisp Drawing Kit")

(defsystem #:cldk/display
  :version "0.3"
  :author "Alessandro Serra (gas2serra@gmail.com)"
  :license "LGPL"
  :depends-on (#:cldk/image #:lparallel)
  :components ((:module "display"
                        :components
                        ((:file "package")
                         (:file "rectangle-set" :depends-on ("package"))
                         (:file "image" :depends-on ("package"))
                         (:file "display-server" :depends-on ("package" "display-driver" "display-kernel"))
                         (:file "window" :depends-on ("package" "display-driver" "display-kernel"))
                         (:file "buffered-window" :depends-on ("package" "window" "display-kernel"))
                         (:file "event-handler" :depends-on ("package"))
                         (:file "kernel-events" :depends-on ("package" "display-kernel" "display-driver"))
                         (:file "kernel-objects" :depends-on ("package" "display-kernel"))
                         (:file "display-kernel" :depends-on ("package" "display-driver"))
                         (:file "display-driver" :depends-on ("package" "event-handler" "rectangle-set")))))
  :description "Common Lisp Drawing Kit")

(defsystem #:cldk/basic
  :version "0.3"
  :author "Alessandro Serra (gas2serra@gmail.com)"
  :license "LGPL"
  :depends-on (#:cldk/core #:cldk/image #:cldk/display)
  :components ()
  :description "Common Lisp Drawing Kit")

(defsystem #:cldk
  :version "0.3"
  :author "Alessandro Serra (gas2serra@gmail.com)"
  :license "LGPL"
  :depends-on (#:cldk/basic #:cldk-null)
  :components ()
  :description "Common Lisp Drawing Kit")


