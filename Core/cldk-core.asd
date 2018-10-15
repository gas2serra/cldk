(in-package :cl-user)
(defpackage #:cldk-asd
  (:use :cl :asdf))
(in-package #:cldk-asd)

(defsystem #:cldk-core/core
  :version "0.3"
  :author "Alessandro Serra (gas2serra@gmail.com)"
  :license "LGPL"
  :depends-on (#:log4cl #:bordeaux-threads #:lparallel))

(defsystem #:cldk-core/driver
  :version "0.3"
  :author "Alessandro Serra (gas2serra@gmail.com)"
  :license "LGPL"
  :depends-on (#:cldk-core/core)
  :components
  ((:module "driver"
            :components
            ((:file "driver" :depends-on ())
             (:file "threaded-driver" :depends-on ("driver"))
             (:file "kernel" :depends-on ("driver"))
             (:file "lparallel-kernel" :depends-on ("kernel"))
             (:file "display-driver" :depends-on ("driver"))))))

(defsystem #:cldk-core/mirror
  :version "0.3"
  :author "Alessandro Serra (gas2serra@gmail.com)"
  :license "LGPL"
  :depends-on (#:cldk-core/driver )
  :components ((:module "mirror"
                        :components
                        ((:file "display-kernel")
                         (:file "buffer" :depends-on ("display-kernel"))
                         (:file "root" :depends-on ("display-kernel"))
                         (:file "window" :depends-on ("display-kernel" "event-handler"))
                         (:file "buffered-window" :depends-on ("window" "buffer"))
                         (:file "event-handler" :depends-on ("display-kernel"))
                         (:file "display-kernel-callback"
                                :depends-on ("display-kernel" "event-handler"))))))
(defsystem #:cldk-core/server
  :version "0.3"
  :author "Alessandro Serra (gas2serra@gmail.com)"
  :license "LGPL"
  :depends-on (#:cldk-core/mirror)
  :components
  ((:module "core"
            :components
            ((:file "server")))))

(defsystem #:cldk-core
  :version "0.3"
  :author "Alessandro Serra (gas2serra@gmail.com)"
  :license "LGPL"
  :depends-on (#:cldk/core #:cldk-core/driver #:cldk-core/mirror #:cldk-core/server)
  :components ()
  :description "Common Lisp Drawing Kit")
