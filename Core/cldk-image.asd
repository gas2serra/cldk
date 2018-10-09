(in-package :cl-user)
(defpackage #:cldk-asd
  (:use :cl :asdf))
(in-package #:cldk-asd)

(defsystem #:cldk-image
  :version "0.3"
  :author "Alessandro Serra (gas2serra@gmail.com)"
  :license "LGPL"
  :depends-on (#:cldk/core #:cldk-display/kernel)
  :components ((:module "image"
                        :components
                        ((:file "image")
                         (:file "rectangle-set"))))
  :description "Common Lisp Drawing Kit")
