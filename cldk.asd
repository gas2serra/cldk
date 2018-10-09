(in-package :cl-user)
(defpackage #:cldk-asd
  (:use :cl :asdf))
(in-package #:cldk-asd)

(defsystem #:cldk/core
  :version "0.3"
  :author "Alessandro Serra (gas2serra@gmail.com)"
  :license "LGPL"
  :depends-on (#:log4cl #:bordeaux-threads)
  :components ((:file "package")))

(defsystem #:cldk/basic
  :version "0.3"
  :author "Alessandro Serra (gas2serra@gmail.com)"
  :license "LGPL"
  :depends-on (:cldk-core)
  :components ())

(defsystem #:cldk
  :version "0.3"
  :author "Alessandro Serra (gas2serra@gmail.com)"
  :license "LGPL"
  :depends-on (:cldk/basic)
  :components ()
  :description "Common Lisp Drawing Kit")
