(in-package :common-lisp-user)

(defpackage :cldk-display-null
  (:use :cldk :cldk-driver :common-lisp)
  (:export
   #:clx-driver
   #:clx-driver-window
   #:clx-driver-buffer
   ))
