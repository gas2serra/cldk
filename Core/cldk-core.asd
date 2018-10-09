(in-package :cl-user)
(defpackage #:cldk-asd
  (:use :cl :asdf))
(in-package #:cldk-asd)

(defsystem #:cldk-core/core
  :version "0.3"
  :author "Alessandro Serra (gas2serra@gmail.com)"
  :license "LGPL"
  :depends-on (#:log4cl #:bordeaux-threads))

(defsystem #:cldk-core/driver
  :version "0.3"
  :author "Alessandro Serra (gas2serra@gmail.com)"
  :license "LGPL"
  :depends-on (#:cldk-core/core)
  :components
  ((:module "core"
            :components
            ((:file "driver" :depends-on ())
             (:file "threaded-driver" :depends-on ("driver"))))))

(defsystem #:cldk-core/kernel
  :version "0.3"
  :author "Alessandro Serra (gas2serra@gmail.com)"
  :license "LGPL"
  :depends-on (#:cldk-core/driver #:lparallel)
  :components
  ((:module "core"
            :components
            ((:file "kernel")
             (:file "lparallel-kernel" :depends-on ("kernel"))))))

(defsystem #:cldk-core/server
  :version "0.3"
  :author "Alessandro Serra (gas2serra@gmail.com)"
  :license "LGPL"
  :depends-on (#:cldk-core/kernel)
  :components
  ((:module "core"
            :components
            ((:file "server")))))

(defsystem #:cldk-core
  :version "0.3"
  :author "Alessandro Serra (gas2serra@gmail.com)"
  :license "LGPL"
  :depends-on (#:cldk/core #:cldk-core/driver #:cldk-core/kernel #:cldk-core/server)
  :components ()
  :description "Common Lisp Drawing Kit")
