(in-package :cl-user)
(defpackage #:cldk-asd
  (:use :cl :asdf))
(in-package #:cldk-asd)

(defsystem #:cldk-display-clx/core
  :version "0.3"
  :author "Alessandro Serra (gas2serra@gmail.com)"
  :license "LGPL"
  :depends-on (#:clx)
  :components ((:module "src/display/clx"
                        :components
                        ((:file "package")))))

(defsystem #:cldk-display-clx/driver
  :version "0.3"
  :author "Alessandro Serra (gas2serra@gmail.com)"
  :license "LGPL"
  :depends-on (#:cldk-display/driver #:cldk-display-clx/core)
  :components ((:module "src/display/clx"
                        :components
                        ((:file "input-keysyms-common")
                         (:file "input-keysymdef"
                                :depends-on ("input-keysyms-common"))
                         (:file "input-keysyms"
                                :depends-on ("input-keysyms-common"))
                         (:file "input")
                         (:file "driver"
                                :depends-on ("input"))))))

(defsystem #:cldk-display-clx/kernel
  :version "0.3"
  :author "Alessandro Serra (gas2serra@gmail.com)"
  :license "LGPL"
  :depends-on (#:cldk-display/kernel #:cldk-display-clx/driver)
  :components ((:module "src/display/clx"
                        :components
                        ())))

(defsystem #:cldk-display-clx
  :version "0.3"
  :author "Alessandro Serra (gas2serra@gmail.com)"
  :license "LGPL"
  :depends-on (:cldk-display-clx/driver :cldk-display-clx/kernel)
  :components ()
  :description "Common Lisp Drawing Kit")
