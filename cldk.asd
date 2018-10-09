(in-package :cl-user)
(defpackage #:cldk-asd
  (:use :cl :asdf))
(in-package #:cldk-asd)

(defsystem #:cldk/core
  :version "0.3"
  :author "Alessandro Serra (gas2serra@gmail.com)"
  :license "LGPL"
  :depends-on (#:log4cl #:bordeaux-threads)
  :components ((:module "src"
                        :components
                        ((:file "package")))))

(defsystem #:cldk/driver
  :version "0.3"
  :author "Alessandro Serra (gas2serra@gmail.com)"
  :license "LGPL"
  :depends-on (#:cldk/core #:lparallel)
  :components ((:module "src/driver"
                        :components
                        ((:file "driver"
                                :depends-on ())
                         (:file "threaded-driver"
                                :depends-on ("driver"))))))

(defsystem #:cldk/kernel
  :version "0.3"
  :author "Alessandro Serra (gas2serra@gmail.com)"
  :license "LGPL"
  :depends-on (#:cldk/driver)
  :components ((:module "src/kernel"
                        :components
                        ((:file "kernel")
                         (:file "lparallel-kernel"
                                :depends-on ("kernel"))))))

(defsystem #:cldk/server
  :version "0.3"
  :author "Alessandro Serra (gas2serra@gmail.com)"
  :license "LGPL"
  :depends-on (#:cldk/kernel)
  :components ((:module "src/server"
                        :components
                        ((:file "server")))))

(defsystem #:cldk/image
  :version "0.3"
  :author "Alessandro Serra (gas2serra@gmail.com)"
  :license "LGPL"
  :depends-on (#:cldk/core #:cldk-display/kernel)
  :components ((:module "image"
                        :components
                        ((:file "image")
                         (:file "rectangle-set"))))
  :description "Common Lisp Drawing Kit")

(defsystem #:cldk/basic
  :version "0.3"
  :author "Alessandro Serra (gas2serra@gmail.com)"
  :license "LGPL"
  :depends-on (:cldk/server #:cldk/image)
  :components ())

(defsystem #:cldk
  :version "0.3"
  :author "Alessandro Serra (gas2serra@gmail.com)"
  :license "LGPL"
  :depends-on (:cldk/basic #:cldk-null)
  :components ()
  :description "Common Lisp Drawing Kit")
