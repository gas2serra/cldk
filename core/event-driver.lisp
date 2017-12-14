(in-package :cldk-internals)

(defclass event-driver (driver)
  ())

(defgeneric driver-process-next-event (driver kernel))
