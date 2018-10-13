(in-package :common-lisp-user)

(defpackage :cldk-display-null
  (:use :cldk :cldk-driver :common-lisp)
  (:export
   #:null-driver
   #:null-driver-window
   #:null-driver-buffer
   ))
