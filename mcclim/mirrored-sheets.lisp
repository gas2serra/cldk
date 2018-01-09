(in-package :clim-fb)


(defclass fb-mirrored-sheet-mixin (image-sheet-mixin
                                   standard-single-mirrored-sheet-mixin)
  ((state :initform :notadopted
          :accessor fb-mirrored-sheet-state)))

(defmethod destroy-mirror :before ((port fb-port) (sheet fb-mirrored-sheet-mixin))
  (with-slots (gcontext clx-image) (sheet-mirror sheet)
    (cldk:destroy-window (sheet-direct-mirror sheet))))

(defclass fb-pixmap (image-pixmap-mixin permanent-medium-sheet-output-mixin basic-pane)
  ())

(defmethod allocate-space :after ((sheet fb-mirrored-sheet-mixin) width height)
  (multiple-value-bind (w h x y) (climi::frame-geometry* (pane-frame sheet))
    (declare (ignore w h))
    (log:info "***ALLOC> ~A ~A ~A" (fb-mirrored-sheet-state sheet) (list width height) (list x y))  
    (when (and (sheet-direct-mirror sheet)
               (eq (fb-mirrored-sheet-state sheet) :configuring))
      (with-slots (space-requirement) sheet
        (cldk:set-window-hints (sheet-direct-mirror sheet)
                               :x x :y y
                               :width (round width)
                               :height (round height)
                               :max-width (min 65535 (round (space-requirement-max-width space-requirement)))
                               :max-height (min 65535 (round (space-requirement-max-height space-requirement)))
                               :min-width (round (space-requirement-min-width space-requirement))
                               :min-height (round (space-requirement-min-height space-requirement)))))))

(defmethod handle-event :around ((sheet fb-mirrored-sheet-mixin)
                                 (event window-configuration-event))
  (setf (fb-mirrored-sheet-state sheet)
        :configuring)
  (call-next-method)
  (setf (fb-mirrored-sheet-state sheet)
        nil))

  
(defmethod clim-standard::%update-mirror-geometry :around ((sheet fb-mirrored-sheet-mixin))
  (log:info "***UPDATE> ~A" (fb-mirrored-sheet-state sheet))
  (when (or
         (not (eq (fb-mirrored-sheet-state sheet) :notadopted))
         (typep sheet 'climi::unmanaged-top-level-sheet-pane))
    (call-next-method)))
  
