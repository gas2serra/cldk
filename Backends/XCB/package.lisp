(in-package :common-lisp-user)

(defpackage :cldk-xcb-backend
  (:use :cldk-backend :cldk :cldk-kernel :cldk-driver
        :cldk-driver-xcb :common-lisp)
  (:export
   #:cldk-xcb-port-mixin
   ))
