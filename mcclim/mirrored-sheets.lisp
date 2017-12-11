(in-package :clim-fb)


(defclass fb-mirrored-sheet-mixin (image-sheet-mixin
                                   standard-single-mirrored-sheet-mixin)
  ())

(defmethod destroy-mirror :before ((port fb-port) (sheet fb-mirrored-sheet-mixin))
  (with-slots (gcontext clx-image) (sheet-mirror sheet)
    (cldk:destroy-window (sheet-direct-mirror sheet))))

(defclass fb-pixmap (image-pixmap-mixin permanent-medium-sheet-output-mixin basic-pane)
  ())

(defmethod allocate-space :after ((sheet fb-mirrored-sheet-mixin) width height)
  (when (sheet-direct-mirror sheet)
    (with-slots (space-requirement) sheet
      (cldk:set-window-hints (sheet-direct-mirror sheet)
                             :width (round width)
                             :height (round height)
                             :max-width (min 65535 (round (space-requirement-max-width space-requirement)))
                             :max-height (min 65535 (round (space-requirement-max-height space-requirement)))
                             :min-width (round (space-requirement-min-width space-requirement))
                             :min-height (round (space-requirement-min-height space-requirement))))))
