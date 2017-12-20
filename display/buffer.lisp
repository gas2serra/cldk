(in-package :cldk-internals)

(defclass buffer (server-object k-buffer-mixin image)
  ())

(defmacro <kbuffer+ (buffer fn &rest args)
  `(<call+ (server ,buffer)
           ,fn ,buffer ,@args))

(defmacro <kbuffer- (buffer fn &rest args)
  `(<call- (server ,buffer)
           ,fn ,buffer ,@args))

(defmethod initialize-instance  :after ((buffer buffer) &key width height
                                                          &allow-other-keys)
  #+nil (<kbuffer+ buffer #'k-initialize-buffer width height))

(defgeneric create-buffer (server width height))

(defgeneric destroy-buffer (buffer)
  (:method ((buffer buffer))
    ))
