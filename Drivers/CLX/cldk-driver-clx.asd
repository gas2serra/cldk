(in-package :cl-user)
(defpackage #:cldk-asd
  (:use :cl :asdf))
(in-package #:cldk-asd)

(defsystem #:cldk-driver-clx
  :version "0.3"
  :author "Alessandro Serra (gas2serra@gmail.com)"
  :license "LGPL"
  :depends-on (#:clx #:cldk-core)
  :components
  ((:file "package")
   (:file "input-keysyms-common"
          :depends-on ("package"))
   (:file "input-keysymdef"
          :depends-on ("input-keysyms-common"))
   (:file "input-keysyms"
          :depends-on ("input-keysyms-common"))
   (:file "input"
          :depends-on ("package"))
   (:file "driver"
          :depends-on ("input"))
   (:file "server"
          :depends-on ("driver" "image"))
   (:file "image" :depends-on ("package" "driver")))
  :description "Common Lisp Drawing Kit: CLX driver")
