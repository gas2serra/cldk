(in-package :cldk-mcclim-render-internals)

;;;
;;; Font utilities.
;;;

(defmethod text-size ((medium render-medium-mixin) string
                      &key text-style (start 0) end)
  (unless text-style
    (setf text-style (medium-text-style medium)))
  (let ((text-font (text-style-to-font (port medium) text-style)))
    (render-text-size (medium-render medium)
                      string
                      text-style
                      text-font
                      :start start
                      :end end)))

(defmethod text-style-ascent (text-style (medium render-medium-mixin))
  (let ((xfont (text-style-to-font (port medium) text-style)))
    (font-ascent xfont)))

(defmethod text-style-descent (text-style (medium render-medium-mixin))
  (let ((xfont (text-style-to-font (port medium) text-style)))
    (font-descent xfont)))

(defmethod text-style-height (text-style (medium render-medium-mixin))
  (+ (text-style-ascent text-style medium)
     (text-style-descent text-style medium)))

(defmethod text-style-width (text-style (medium render-medium-mixin))
  (let ((xfont (text-style-to-font (port medium) text-style)))
    (font-glyph-width xfont #\m)))

(defmethod climi::text-bounding-rectangle*
    ((medium render-medium-mixin) string &key text-style (start 0) end)
  (unless text-style
    (setf text-style (medium-text-style medium)))
  (let ((text-font (text-style-to-font (port medium) text-style)))
    (render-text-bounding-rectangle* (medium-render medium)
                                     string
                                     text-style
                                     text-font
                                     :start start
                                     :end end)))
