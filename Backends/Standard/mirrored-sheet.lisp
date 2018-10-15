(in-package :cldk-backend)

(defclass cldk-mirrored-sheet-mixin ()
  ())

#|
(defclass cldk-mirrored-sheet-mixin ()
  ((state :initform :notadopted
          :accessor cldk-mirrored-sheet-state)))

(defmethod clim:destroy-mirror :before ((port cldk-port-mixin)
                                        (sheet cldk-mirrored-sheet-mixin))
  (cldk:destroy-window (clim:sheet-direct-mirror sheet)))


(defmethod clim:allocate-space :around ((sheet cldk-mirrored-sheet-mixin) width height)
  (multiple-value-bind (w h x y) (climi::frame-geometry* (clim:pane-frame sheet))
    (declare (ignore w h))
    (if (eql (cldk-mirrored-sheet-state sheet) :configuring)
        (call-next-method)
        (with-slots (climi::space-requirement) sheet
          (when climi::space-requirement
            (cldk:set-window-hints (clim:sheet-direct-mirror sheet)
                                   :x x :y y
                                   :width (round width)
                                   :height (round height)
                                   :max-width (min 65535 (round (clim:space-requirement-max-width climi::space-requirement)))
                                   :max-height (min 65535 (round (clim:space-requirement-max-height climi::space-requirement)))
                                   :min-width (round (clim:space-requirement-min-width climi::space-requirement))
                                   :min-height (round (clim:space-requirement-min-height climi::space-requirement))))))))

(defmethod (setf clim:sheet-region) (region (sheet cldk-mirrored-sheet-mixin))
  (if (eql (cldk-mirrored-sheet-state sheet) :configuring)
      (call-next-method)
      (progn
        (cldk:set-window-size (clim:sheet-direct-mirror sheet)
                              (floor (clim:bounding-rectangle-max-x region))
                              (floor (clim:bounding-rectangle-max-y region)))
        #+nil (cldk:set-window-position (clim:sheet-direct-mirror sheet)
                                  (floor (clim:bounding-rectangle-min-x region))
                                  (floor (clim:bounding-rectangle-min-y region))))))


(defmethod clim:handle-event :around ((pane cldk-mirrored-sheet-mixin)
                                 (event clim:window-configuration-event))
  (setf (cldk-mirrored-sheet-state pane)
        :configuring)
   (call-next-method)
   (setf (cldk-mirrored-sheet-state pane)
         nil))

(defmethod clim-standard::%update-mirror-geometry :around ((sheet cldk-mirrored-sheet-mixin))
  (break)
  (when (or
         (not (eq (cldk-mirrored-sheet-state sheet) :notadopted))
         (typep sheet 'climi::unmanaged-top-level-sheet-pane))
    (call-next-method)))
  
|#
