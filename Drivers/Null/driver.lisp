(in-package :cldk-driver-null)

(defclass null-driver (display-driver)
  ())

(defmethod driver-start ((driver null-driver))
  (log:trace driver))

(defmethod driver-stop ((driver null-driver))
  (log:trace driver))

(defmethod driver-kill ((driver null-driver))
  (log:trace driver))

(defmethod driver-ping ((driver null-driver))
  (log:trace driver)
  t)

(defmethod driver-force-output ((driver null-driver))
  (log:trace driver))

;;; events
(defmethod driver-process-next-event ((driver null-driver))
  (log:trace driver)
  nil)

;;; screens

(defmethod driver-screen-size ((driver null-driver) units)
  (log:trace driver))

(defmethod driver-screen-pointer-position ((driver null-driver))
  (log:trace driver))

;;; window

(defclass null-driver-window (driver-window)
  ())

(defmethod driver-object-id ((window null-driver-window))
  window)

(defmethod driver-initialize-window ((driver null-driver) win name pretty-name x y
                                 width height mode)
  )

(defmethod driver-destroy-window ((driver null-driver) window)
  (log:trace window))

(defmethod driver-show-window ((driver null-driver) window)
  (log:trace window))

(defmethod driver-hide-window ((driver null-driver) window)
  (log:trace window))

(defmethod driver-window-position ((driver null-driver) window)
  (log:trace window))

(defmethod driver-window-size ((driver null-driver) window)
  (log:trace window))
 
(defmethod driver-set-window-position ((driver null-driver) window x y)
  (log:trace window x y))

(defmethod driver-set-window-size ((driver null-driver) window width height)
  (log:trace window width height))

(defmethod driver-set-window-hints ((driver null-driver) window x y width height max-width max-height
                                    min-width min-height)
  (log:trace window width height))

(defmethod driver-bury-window ((driver null-driver) window)
  (log:trace window))

(defmethod driver-raise-window ((driver null-driver) window)
  (log:trace window))

(defmethod driver-window-pointer-position ((driver null-driver) window)
  (log:trace window))

;;; cursors
(defclass null-cursor (driver-cursor)
  ())

(defvar *null-cursor-mapping*  
  '())

(defmethod driver-avaiable-cursor-names ((driver null-driver))
  (mapcar #'car *null-cursor-mapping*))

(defmethod driver-create-cursor ((driver null-driver) named-cursor)
  (log:trace named-cursor)
  (make-instance 'null-cursor))

(defmethod driver-destroy-cursor ((driver null-driver) cursor)
  (log:trace cursor))

(defmethod driver-set-window-cursor ((driver null-driver) window cursor)
  (log:trace window cursor))

;;; buffer
(defclass null-buffer (driver-buffer)
  ())

(defmethod driver-initialize-buffer ((driver null-driver) buffer width height)
  (log:trace driver)
  (make-instance 'null-buffer))

(defmethod driver-update-buffer ((driver null-driver) buffer width height)
  (log:trace driver)
  (make-instance 'null-buffer))

(defmethod driver-destroy-buffer ((driver null-driver) buffer)
  (log:trace driver))

(defmethod driver-copy-buffer-to-window ((driver null-driver) buffer x y width height
                                         window to-x to-y)
  (log:trace driver)
  nil)

