(in-package :cl-user)
(defpackage #:cldk-examples-asd
  (:use :cl :asdf))
(in-package #:cldk-examples-asd)

(defsystem cldk-examples
  :version "0.3"
  :author "Alessandro Serra (gas2serra@gmail.com)"
  :license "LGPL"
  :depends-on (:cldk)
  :components ((:file "package"))
  :description "Examples and Tests for Common Lisp Drawing Kit")


