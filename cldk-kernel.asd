(in-package :cl-user)
(defpackage #:cldk-asd
  (:use :cl :asdf))
(in-package #:cldk-asd)

(defsystem #:cldk-kernel
  :version "0.3"
  :author "Alessandro Serra (gas2serra@gmail.com)"
  :license "LGPL"
  :depends-on (#:cldk-driver #:lparallel)
  :components ((:module "kernel"
                        :components
                        ((:file "package")
                         (:file "kernel"
                                :depends-on ("package"))
                         (:file "lparallel-kernel"
                                :depends-on ("package" "kernel")))))
  :description "Common Lisp Drawing Kit")

(defsystem #:cldk-kernel/display
  :version "0.3"
  :author "Alessandro Serra (gas2serra@gmail.com)"
  :license "LGPL"
  :depends-on (#:cldk-kernel #:cldk-driver/display)
  :components ((:module "kernel/display"
                        :components
                        ((:file "display-kernel")
                         (:file "buffer" :depends-on ("display-kernel"))
                         (:file "window" :depends-on ("display-kernel" "event-handler"))
                         (:file "buffered-window" :depends-on ("window" "buffer"))
                         (:file "event-handler" :depends-on ("display-kernel"))
                         (:file "display-kernel-callback"
                                :depends-on ("display-kernel" "event-handler"))))))
