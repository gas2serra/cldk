(in-package :cldk-null)

(defclass null-driver (display-driver)
  ())

(defmethod driver-start ((driver null-driver))
  (log:info driver))

(defmethod driver-stop ((driver null-driver))
  (log:info driver))

(defmethod driver-force-output ((driver null-driver))
  (log:info driver))

(defmethod driver-process-next-event ((driver null-driver) event-handler &key timeout)
  (log:info driver)
  (sleep timeout))

(defmethod driver-screen-num ((driver null-driver))
  1)

(defmethod driver-screen-size ((driver null-driver) screen-index units)
  (log:info driver))

(defmethod driver-screen-dpi ((driver null-driver) screen-index)
  (log:info driver))

(defmethod driver-screen-pointer-position ((driver null-driver))
  (log:info driver))

;;; window

(defclass null-window (driver-window)
  ())

(defmethod driver-object-id ((window null-window))
  window)

(defmethod driver-create-window ((driver null-driver) name pretty-name x y width height mode)
  (log:info name x y width height mode)
  (make-instance 'null-window))

(defmethod driver-destroy-window ((driver null-driver) window)
  (log:info window))

(defmethod driver-show-window ((driver null-driver) window)
  (log:info window))

(defmethod driver-hide-window ((driver null-driver) window)
  (log:info window))

(defmethod driver-window-position ((driver null-driver) window)
  (log:info window))

(defmethod driver-window-size ((driver null-driver) window)
  (log:info window))
 
(defmethod driver-set-window-position ((driver null-driver) window x y)
  (log:info window x y))

(defmethod driver-set-window-size ((driver null-driver) window width height)
  (log:info window width height))

(defmethod driver-set-window-size-hints ((driver null-driver) window width height max-width max-height
                                         min-width min-height)
  (log:info window width height))

(defmethod driver-bury-window ((driver null-driver) window)
  (log:info window))

(defmethod driver-raise-window ((driver null-driver) window)
  (log:info window))

(defmethod driver-window-pointer-position ((driver null-driver) window)
  (log:info window))

;;; cursors
(defclass null-cursor (driver-cursor)
  ())

(defmethod driver-create-cursor ((driver null-driver) named-cursor)
  (log:info named-cursor)
  (make-instance 'null-cursor))

(defmethod driver-destroy-cursor ((driver null-driver) cursor)
  (log:info cursor))

(defmethod driver-set-window-cursor ((driver null-driver) window cursor)
  (log:info window cursor))

;;; buffer
(defclass null-buffer (driver-buffer)
  ())

(defmethod driver-create-buffer ((driver null-driver) width height)
  (make-instance 'null-buffer :data nil :width width :height height))
                   
(defmethod driver-destroy-buffer ((driver null-driver) buffer)
  (with-slots (cldki::data) buffer
    (setf cldki::data nil)))

(defmethod driver-copy-buffer-to-window ((driver null-driver) buffer x y width height
                                         window to-x to-y)
  nil)

(defmethod driver-copy-image-to-buffer ((driver null-driver) image x y width height buffer)
  )
