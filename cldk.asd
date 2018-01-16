(in-package :cl-user)
(defpackage #:cldk-asd
  (:use :cl :asdf))
(in-package #:cldk-asd)

(defsystem #:cldk/core
  :version "0.3"
  :author "Alessandro Serra (gas2serra@gmail.com)"
  :license "LGPL"
  :depends-on (#:log4cl #:bordeaux-threads  #:lparallel)
  :components ((:file "package")
               (:module "core"
                        :depends-on ("package")
                        :components
                        ((:file "driver")
                         (:file "event-server" :depends-on ("driver" "server"))
                         (:file "single-thread-server" :depends-on ("event-server" "driver" "server"))
                         (:file "kernel" :depends-on ("driver"))
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
  :depends-on (#:cldk/image)
  :components ((:module "display"
                        :components
                        ((:file "display-server" :depends-on ("display-driver" "display-kernel"))
                         (:file "window" :depends-on ("display-driver" "display-kernel" "display-kernel-call" "event-handler"))
                         (:file "buffer" :depends-on ("display-kernel" "display-kernel-call"))
                         (:file "buffered-window" :depends-on ("window" "buffer" "display-kernel" "display-kernel-call"))
                         (:file "event-handler" :depends-on ())
                         (:file "display-kernel-callback" :depends-on ("display-kernel" "display-driver" "event-handler"))
                         (:file "display-kernel-call" :depends-on ("display-kernel" "display-driver"))
                         (:file "display-kernel" :depends-on ("display-driver"))
                         (:file "display-driver" :depends-on ()))))
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


