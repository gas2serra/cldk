(in-package :cl-user)
(defpackage #:cldk-null-asd
  (:use :cl :asdf))
(in-package #:cldk-null-asd)

(defsystem #:cldk-null
    :version "0.3"
    :author "Alessandro Serra"
    :license "LGPL"
    :depends-on (#:cldk/basic #:cldk-display-clx)
    :components
    ((:module "Null"
              :components
              ((:file "package")
               (:file "server" :depends-on ("package")))))
    :description "Common Lisp Drawing Kit: Null Backend")

