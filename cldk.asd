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

(defsystem #:cldk/display
  :version "0.3"
  :author "Alessandro Serra (gas2serra@gmail.com)"
  :license "LGPL"
  :depends-on (#:cldk/image #:cldk-display/kernel)
  :components (
               (:module "display"
                        :depends-on ()
                        :components
                        ((:file "display-server" :depends-on ())
                         (:file "window" :depends-on ("display-kernel-call"))
                         (:file "buffer" :depends-on ("display-kernel-call"))
                         (:file "buffered-window" :depends-on ("window" "buffer" "display-kernel-call"))
                         (:file "display-kernel-callback" :depends-on ("display-server"))
                         (:file "display-kernel-call" :depends-on ("display-server"))
                         )))
  :description "Common Lisp Drawing Kit")

(defsystem #:cldk/core2
  :version "0.3"
  :author "Alessandro Serra (gas2serra@gmail.com)"
  :license "LGPL"
  :depends-on (#:log4cl #:bordeaux-threads  #:lparallel #:cldk/driver #:cldk/kernel )
  :components (
               (:module "core"
               
                        :components
                        (
                         (:file "kernel")
                         (:file "event-server" :depends-on ("command-server" "server"))
                         (:file "command-server" :depends-on ("server"))
                         (:file "server" :depends-on ("kernel"))
                         )))
  :description "Common Lisp Drawing Kit")

(defsystem #:cldk/basic
  :version "0.3"
  :author "Alessandro Serra (gas2serra@gmail.com)"
  :license "LGPL"
  :depends-on (:cldk/kernel #:cldk/image #:cldk/display #:cldk/core2)
  :components ())

(defsystem #:cldk
  :version "0.3"
  :author "Alessandro Serra (gas2serra@gmail.com)"
  :license "LGPL"
  :depends-on (:cldk/basic #:cldk-null)
  :components ()
  :description "Common Lisp Drawing Kit")
