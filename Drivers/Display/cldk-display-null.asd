(in-package :cl-user)
(defpackage #:cldk-asd
  (:use :cl :asdf))
(in-package #:cldk-asd)

(defsystem #:cldk-display-null
  :version "0.3"
  :author "Alessandro Serra (gas2serra@gmail.com)"
  :license "LGPL"
  :depends-on (#:cldk-display)
  :components ((:module "Null"
                        :components
                        ((:file "package")
                         (:file "driver"
                                :depends-on ("package")))))
  :description "Common Lisp Drawing Kit: Null Display Driver")
