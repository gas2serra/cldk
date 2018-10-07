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
