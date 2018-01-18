(in-package :cldk-internals)

(defclass buffer (k-buffer-mixin image)
  ())

(defmacro <kbuffer+ (buffer fn &rest args)
  `(<call+ (server ,buffer)
           ,fn ,buffer ,@args))

(defmacro <kbuffer- (buffer fn &rest args)
  `(<call- (server ,buffer)
           ,fn ,buffer ,@args))

(defmethod initialize-instance  :after ((buffer buffer) &key width height
                                                          &allow-other-keys)
  (<kbuffer+ buffer #'k-initialize-buffer width height))

(defgeneric create-buffer (server width height))

(defgeneric destroy-buffer (buffer)
  (:method ((buffer buffer))
    (<kbuffer+ buffer #'k-destroy-buffer)))
