(in-package :cldk-mcclim-render-internals)

(deftype clim-rgb-image-data () '(simple-array (unsigned-byte 32) (* *)))
#|
(defmethod %make-pixeled-design ((ink mcclim-image::rgb-pattern))
  (let* ((img (slot-value ink 'mcclim-image::image)))
    (make-pixeled-image-design :image
                               (make-instance 'rgb-image
                                              :width (mcclim-image::image-width img)
                                              :height (mcclim-image::image-height img)
                                              :pixels (mcclim-image::image-data img)))))
(defmethod coerce-image ((image basic-image)
                         (type (eql :traditional-rgb)) &optional medium)
  (if (typep image 'mcclim-image::rgb-image)
      image
      (let ((img (coerce-image image :rgb :two-dim-array)))
        (make-instance 'mcclim-image::rgb-image
                       :width (image-width img)
                       :height (image-height img)
                       :data (image-pixels img)))))

(defmethod coerce-image ((image mcclim-image::rgb-image)
                         type &optional medium)
  (declare (ignore medium))
  (make-instance 'rgb-image
                 :width (mcclim-image::image-width image)
                 :height (mcclim-image::image-height image)
                 :pixels (mcclim-image::image-data image)))
|#
