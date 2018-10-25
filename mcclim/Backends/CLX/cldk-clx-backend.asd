(in-package :cl-user)
(defpackage #:cldk-asd
  (:use :cl :asdf))
(in-package #:cldk-asd)

(defsystem #:cldk-clx-backend
    :version "0.3"
    :author "Alessandro Serra"
    :license "LGPL"
    :depends-on (#:cldk-backend #:cldk-driver-clx #:mcclim-cldk)
    :components
    ((:file "package")
     ;;(:file "buffer" :depends-on ("package"))
     (:file "port" :depends-on ("package")))
    :description "Common Lisp Drawing Kit: CLX Backend")

