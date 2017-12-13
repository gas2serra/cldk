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
                         (:file "event-driver" :depends-on ("driver"))
                         (:file "kernel" :depends-on ("driver"))
                         (:file "server" :depends-on ("kernel"))
                         (:file "single-thread-server" :depends-on ("server" "single-thread-kernel"))
                         (:file "single-thread-kernel" :depends-on ("kernel"))
                         (:file "multi-thread-kernel" :depends-on ("kernel"))
                         (:file "event-kernel" :depends-on ("kernel" "event-driver"))
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
                         (:file "buffered-window" :depends-on ("window" "display-kernel" "display-kernel-call"))
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


