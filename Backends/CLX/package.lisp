(in-package :common-lisp-user)

(defpackage :cldk-clx-backend
  (:use :cldk-backend :cldk :cldk-kernel :cldk-driver
        :cldk-driver-clx :common-lisp)
  (:export
   #:cldk-clx-port-mixin
   ))
