(in-package :cl-user)
(defpackage #:cldk-clx-asd
  (:use :cl :asdf))
(in-package #:cldk-clx-asd)

(defsystem #:cldk-clx
    :version "0.3"
    :author "Alessandro Serra"
    :license "LGPL"
    :depends-on (#:cldk/basic #:uiop #:clx #:cldk-display-clx #:cldk-display)
    :components ((:file "package")
                 (:file "server" :depends-on ("package"))
                 (:file "buffer" :depends-on ("package" "server")))
    :description "CLX Backend for Common Lisp Drawing Kit")

