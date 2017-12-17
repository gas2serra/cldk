(in-package :cldk-internals)

;;;
;;; Two dimensional array of pixels
;;;
(defclass sdl2-image (basic-image buffer-image-mixin)
  ())

;;;
;;; RGBA
;;;
(deftype sdl2-rgb-image-pixels () 'cffi-sys:foreign-pointer)

(defclass sdl2-rgb-image (sdl2-image rgb-image-mixin)
  ((pixels :type sdl2-rgb-image-pixels)))

(defmethod image-rgb-set-fn ((image sdl2-rgb-image) &key (dx 0) (dy 0))
  (let ((pixels (image-pixels image))
        (width (image-width image)))
    (declare (type sdl2-rgb-image-pixels pixels))
    (lambda (x y r g b)
      (cffi-sys:%mem-set
       (dpb r (byte 8 0)
            (dpb g (byte 8 8)
                 (dpb b (byte 8 16)
                      (dpb 255 (byte 8 24) 0))))
       pixels :UNSIGNED-INT (* 4
                               (+
                                (* (+ y dy) width)
                                (+ x dx)))))))

(defmethod initialize-instance :after ((image sdl2-rgb-image)
                                       &key)
  #+nil (let ((width (image-width image))
        (height (image-height image)))
    (when (and width height (not (slot-boundp image 'pixels)))
      (setf (slot-value image 'pixels)
            (make-array (list height width)
                        :element-type '(unsigned-byte 32)
                        :initial-element #xFFFFFFFF)))))
#|
(eval-when (:execute :load-toplevel :compile-toplevel)
  (cldk-image-internals::def-rgba-image-primitives sdl2-rgba-image sdl2-rgba-image-pixels
                            image-var pixels-var x-var y-var red-var green-var blue-var alpha-var
                            `nil
                            `(cffi-sys:%mem-set
                              (dpb ,red-var (byte 8 0)
                                   (dpb ,green-var (byte 8 8)
                                        (dpb ,blue-var (byte 8 16)
                                             (dpb 255 (byte 8 24) 0))))
                              ,pixels-var :UNSIGNED-INT (* 4
                                                          (+
                                                           (* ,y-var (cldk-image:image-width ,image-var))
                                                           ,x-var)))
                            `nil))

(cldk-image-internals::def-rgba-image-functions sdl2-rgba-image)
|#

;;;
;;; Configuration & Optimization
;;;

;;(def-fast-rgba-copy-image rgba-image sdl2-rgba-image)
