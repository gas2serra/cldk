(in-package :cl-user)
(defpackage #:cldk-null-asd
  (:use :cl :asdf))
(in-package #:cldk-null-asd)

(defsystem #:cldk-null
    :version "0.3"
    :author "Alessandro Serra"
    :license "LGPL"
    :depends-on (#:cldk/basic)
    :components ((:file "package")
                 (:file "server" :depends-on ("package" "driver"))
                 (:file "driver" :depends-on ("package")))
    :description "Null Backend for Common Lisp Drawing Kit")

