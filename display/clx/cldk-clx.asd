(in-package :cl-user)
(defpackage #:cldk-clx-asd
  (:use :cl :asdf))
(in-package #:cldk-clx-asd)

(defsystem #:cldk-clx
    :version "0.3"
    :author "Alessandro Serra"
    :license "LGPL"
    :depends-on (#:cldk/basic #:uiop #:clx)
    :components ((:file "package")
                 (:file "server" :depends-on ("package" "driver"))
                 (:file "keysyms-common" :depends-on ("package"))
                 (:file "keysymdef" :depends-on ("package" "keysyms-common"))
                 (:file "keysyms" :depends-on ("package" "keysyms-common"))
                 (:file "input" :depends-on ("package" "keysyms" "keysymdef"))
                 (:file "driver" :depends-on ("package" "input" "keysyms-common")))
    :description "CLX Backend for Common Lisp Drawing Kit")

