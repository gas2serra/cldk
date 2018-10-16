;;(ql:quickload :cldk-null)
(require :mcclim-cldk)
(require :cldk-clx-backend)

(ql:quickload :cldk-xcb)
(ql:quickload :cldk-clx)
(ql:quickload :cldk-sdl2)

(defparameter *backend* '(:xcb))
