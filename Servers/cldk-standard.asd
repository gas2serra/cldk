(in-package :cl-user)
(defpackage #:cldk-null-asd
  (:use :cl :asdf))
(in-package #:cldk-null-asd)

(defsystem #:cldk-standard
    :version "0.3"
    :author "Alessandro Serra"
    :license "LGPL"
    :depends-on (#:cldk #:cldk-display)
    :components
    ((:module "Standard"
              :components
              ((:file "package")
               (:file "server" :depends-on ("package"))
               (:file "display-server" :depends-on ("server")))))
    :description "Common Lisp Drawing Kit: Standard Backend")

