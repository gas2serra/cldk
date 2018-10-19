(in-package :cl-user)
(defpackage #:cldk-asd
  (:use :cl :asdf))
(in-package #:cldk-asd)

(defsystem #:cldk-backend
    :version "0.3"
    :author "Alessandro Serra"
    :license "LGPL"
    :depends-on (#:cldk #:cldk-core #:cldk-image
                        #:mcclim #:mcclim-backend-common)
    :components
    ((:file "package")
     (:file "graft" :depends-on ("package" "port"))
     (:file "frame-manager" :depends-on ("package"))
     (:file "mirror" :depends-on
            ("package" "port" "mirrored-sheet"))
     (:file "buffered-mirror" :depends-on
            ("package" "port" "mirrored-sheet" "mirror"))
     (:file "mirrored-sheet" :depends-on ("package"))
     (:file "port" :depends-on ("package"))
     (:file "server" :depends-on ("package"))
     (:file "display-server" :depends-on ("server" "package")))
    :description "Common Lisp Drawing Kit: Standard Backend")

