(in-package :cl-user)
(defpackage #:cldk-clx-asd
  (:use :cl :asdf))
(in-package #:cldk-clx-asd)

(defsystem #:cldk-clx
    :version "0.3"
    :author "Alessandro Serra"
    :license "LGPL"
    :depends-on (#:uiop #:clx #:cldk #:cldk-display-clx #:cldk-display #:cldk-standard)
    :components
    ((:module "CLX"
              :components
              ((:file "package")
               (:file "server" :depends-on ("package"))
               (:file "buffer" :depends-on ("package" "server")))))
    :description "Common Lisp Drawing Kit: CLX Backend")

