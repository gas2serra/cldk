(in-package :cl-user)
(defpackage #:cldk-asd
  (:use :cl :asdf))
(in-package #:cldk-asd)

(defsystem #:cldk-display-xcb
  :version "0.3"
  :author "Alessandro Serra (gas2serra@gmail.com)"
  :license "LGPL"
  :depends-on (#:cldk-display #:cffi #:cffi-libffi #:static-vectors)
  :components ((:module "XCB"
                        :components
                        ((:file "package")
                         (:file "cffi"
                                :depends-on ("package"))
                         (:file "cffi-libs"
                                :depends-on ("package"))
                         (:file "cffi-xcb"
                                :depends-on ("cffi" "cffi-libs"))
                         (:file "cffi-icccm"
                                :depends-on ("cffi"))
                         (:file "cffi-events"
                                :depends-on ("cffi"))
                         (:file "cffi-image"
                                :depends-on ("cffi" "cffi-xcb"))
                         (:file "driver"
                                :depends-on ("driver-input" "cffi-xcb" "cffi-icccm" "cffi-image" "package"))
                         (:file "driver-input" :depends-on ("cffi-events")))))
  :description "Common Lisp Drawing Kit: XCB Display Driver")
