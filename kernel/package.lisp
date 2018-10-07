(in-package :common-lisp-user)

(defpackage :cldk-kernel
  (:use :common-lisp :cldk-driver)
  (:export
   #:*kernel-mode*
   #:check-kernel-mode
   #:check-user-mode
   #:kerneled-driver-mixin
   #:within-kernel-mode
   #:within-user-mode
   #:kernel-call
   #:kernel-callback

   #:lparallel-kernel-call-mixin
   #:kernel-call-queue
   #:exec-next-kernel-call
   
   #:lparallel-kernel-callback-mixin
   #:kernel-callback-queue
   #:exec-next-kernel-callback
   
   #:empty-lparallel-queue
   ))
