(in-package :cl-user)
(defpackage #:cldk-xcb-asd
  (:use :cl :asdf))
(in-package #:cldk-xcb-asd)

(defsystem #:cldk-xcb
    :version "0.3"
    :author "Alessandro Serra"
    :license "LGPL"
    :depends-on (#:cldk-standard #:cldk-display-xcb)
    :components
    ((:module "XCB"
              :components
              ((:file "package")
               (:file "buffer" :depends-on ("package" "server"))
               (:file "server" :depends-on ("package")))))
    :description "Common Lisp Drawing Kit: XCB Backend")

