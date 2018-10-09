(in-package :cl-user)
(defpackage #:cldk-asd
  (:use :cl :asdf))
(in-package #:cldk-asd)

(defsystem #:cldk-display/core
  :version "0.3"
  :author "Alessandro Serra (gas2serra@gmail.com)"
  :license "LGPL"
  :depends-on (#:log4cl #:bordeaux-threads))

(defsystem #:cldk-display/driver
  :version "0.3"
  :author "Alessandro Serra (gas2serra@gmail.com)"
  :license "LGPL"
  :depends-on (#:cldk-core/driver #:cldk-display/core)
  :components
  ((:module "display"
            :components
            ((:file "display-driver")))))

(defsystem #:cldk-display/kernel
  :version "0.3"
  :author "Alessandro Serra (gas2serra@gmail.com)"
  :license "LGPL"
  :depends-on (#:cldk-core/kernel #:cldk-display/driver)
  :components
  ((:module "display"
            :components
            ((:file "display-kernel")
             (:file "buffer" :depends-on ("display-kernel"))
             (:file "window" :depends-on ("display-kernel" "event-handler"))
             (:file "buffered-window" :depends-on ("window" "buffer"))
             (:file "event-handler" :depends-on ("display-kernel"))
             (:file "display-kernel-callback"
                    :depends-on ("display-kernel" "event-handler"))))))

(defsystem #:cldk-display/server
  :version "0.3"
  :author "Alessandro Serra (gas2serra@gmail.com)"
  :license "LGPL"
  :depends-on (#:cldk-image #:cldk-display/kernel)
  :components
  ((:module "display"
            :components
            ((:file "display-server" :depends-on ()))))
  :description "Common Lisp Drawing Kit")

(defsystem #:cldk-display
  :version "0.3"
  :author "Alessandro Serra (gas2serra@gmail.com)"
  :license "LGPL"
  :depends-on (#:cldk-display/driver #:cldk-display/kernel :cldk-display/server)
  :components ()
  :description "Common Lisp Drawing Kit: Display")
