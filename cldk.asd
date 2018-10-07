(in-package :cl-user)
(defpackage #:cldk-asd
  (:use :cl :asdf))
(in-package #:cldk-asd)

(defsystem #:cldk/core
  :version "0.3"
  :author "Alessandro Serra (gas2serra@gmail.com)"
  :license "LGPL"
  :depends-on (#:log4cl #:bordeaux-threads  #:lparallel #:cldk-driver #:cldk-kernel)
  :components ((:file "package" )
               (:module "core"
                        :depends-on ("package")
                        :components
                        (
                         (:file "kernel" :depends-on ("command"))
                         (:file "event-server" :depends-on ("command-server" "server"))
                         (:file "command-server" :depends-on ("command" "server"))
                         (:file "command" :depends-on ())
                         (:file "server" :depends-on ("kernel"))
                         )))
  :description "Common Lisp Drawing Kit")

(defsystem #:cldk/image
  :version "0.3"
  :author "Alessandro Serra (gas2serra@gmail.com)"
  :license "LGPL"
  :depends-on (#:cldk/core)
  :components ((:module "image"
                        :components
                        ((:file "image")
                         (:file "rectangle-set"))))
  :description "Common Lisp Drawing Kit")

(defsystem #:cldk/display
  :version "0.3"
  :author "Alessandro Serra (gas2serra@gmail.com)"
  :license "LGPL"
  :depends-on (#:cldk/image #:cldk-driver/display)
  :components (
               (:module "display"
                        :depends-on ()
                        :components
                        ((:file "display-server" :depends-on ())
                         (:file "window" :depends-on ("display-kernel-call" "event-handler"))
                         (:file "buffer" :depends-on ("display-kernel-call"))
                         (:file "buffered-window" :depends-on ("window" "buffer" "display-kernel-call"))
                         (:file "event-handler" :depends-on ())
                         (:file "display-kernel-callback" :depends-on ("display-server" "event-handler"))
                         (:file "display-kernel-call" :depends-on ("display-server"))
                         )))
  :description "Common Lisp Drawing Kit")

(defsystem #:cldk/basic
  :version "0.3"
  :author "Alessandro Serra (gas2serra@gmail.com)"
  :license "LGPL"
  :depends-on (#:cldk/core #:cldk/image #:cldk/display #:cldk-driver #:cldk-kernel)
  :components ()
  :description "Common Lisp Drawing Kit")



(defsystem #:cldk
  :version "0.3"
  :author "Alessandro Serra (gas2serra@gmail.com)"
  :license "LGPL"
  :depends-on (#:cldk/basic #:cldk-null)
  :components ()
  :description "Common Lisp Drawing Kit")


