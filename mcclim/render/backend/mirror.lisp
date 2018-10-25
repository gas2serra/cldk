
(in-package :cldk-render-internals)

(defclass image-mirror-mixin ()
  ((image-medium :initarg :image-medium :initform :opticl)
   (image-type :initarg :image-type :initform :rgb)
   (image :initform nil :reader image-mirror-image)
   (image-lock :initform (climi::make-lock "image"))
   (resize-image-p :initform t :reader image-mirror-resize-image-p)
   (dirty-region :initform nil)
   ))

(defmethod (setf image-mirror-image) (img (mirror image-mirror-mixin))
  (when img
    (with-slots (image resize-image-p) mirror
      (setf resize-image-p nil)
      (setf image img))))

(defmacro mirror-rendering (mirror &body code)
  `(when (image-mirror-image ,mirror)
     (with-slots (image-lock) ,mirror
       (climi::with-lock-held (image-lock)
         (let ((reg (progn
                      ,@code)))
           (when reg
             (clim:with-bounding-rectangle* (min-x min-y max-x max-y)
                 reg
               (%notify-image-updated ,mirror
                                      (make-rectangle* (floor min-x) (floor min-y)
                                                       (ceiling max-x) (ceiling max-y))))))))))
;;;
;;; protocols
;;;

(defgeneric %make-image (mirror sheet))
(defgeneric %set-image-region (mirror region))
(defgeneric %create-mirror-image (mirror width height))
(defgeneric %notify-image-updated (mirror region))
(defgeneric %mirror-force-output (mirror))

;;;
;;; implementation
;;;

(defmethod %make-image ((mirror image-mirror-mixin) sheet)
  (with-slots (image resize-image-p) mirror
    (clim:with-bounding-rectangle* (min-x min-y max-x max-y)
	(sheet-region sheet)
      (let ((width (ceiling (- max-x min-x)))
	    (height (ceiling (- max-y min-y))))
	(%create-mirror-image mirror (1+ width) (1+ height))))))

(defmethod %set-image-region ((mirror image-mirror-mixin) region)
  (with-slots (image resize-image-p) mirror
    (clim:with-bounding-rectangle* (min-x min-y max-x max-y)
      region
      (let ((width (1+ (ceiling (- max-x min-x))))
	    (height (1+ (ceiling (- max-y min-y)))))
	(if (and resize-image-p
                 (or (null image)
                     (/= width (image-width image))
                     (/= height (image-height image))))
	    (%create-mirror-image mirror width height)
	    nil)))))

(defmethod %create-mirror-image ((mirror image-mirror-mixin) width height)
  (with-slots (image image-medium image-type) mirror
    (setf image (make-image image-medium image-type width height )))
  (with-slots (dirty-region) mirror
    (setf dirty-region nil)))

(defmethod %notify-image-updated ((mirror image-mirror-mixin) region)
  (when region
    (with-slots (dirty-region) mirror
      (if dirty-region
          (setf dirty-region (region-union dirty-region region))
          (setf dirty-region region)))))

(defmethod %mirror-force-output ((mirror image-mirror-mixin))
  nil)
