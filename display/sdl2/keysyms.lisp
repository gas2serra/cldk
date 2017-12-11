(in-package :cldk-sdl2)

(defparameter mod-mappings (list (cons sdl2-ffi::+kmod-alt+ :alt)
                                 (cons sdl2-ffi::+kmod-caps+ :caps)
                                 (cons sdl2-ffi::+kmod-ctrl+ :ctrl)
                                 (cons sdl2-ffi::+kmod-gui+ :gui)
                                 (cons sdl2-ffi::+kmod-lalt+ :left-alt)
                                 (cons sdl2-ffi::+kmod-lctrl+ :left-ctrl)
                                 (cons sdl2-ffi::+kmod-lgui+ :left-gui)
                                 (cons sdl2-ffi::+kmod-lshift+ :left-shift)
                                 (cons sdl2-ffi::+kmod-mode+ :mode)
                                 (cons sdl2-ffi::+kmod-num+ :num)
                                 (cons sdl2-ffi::+kmod-ralt+ :right-alt)
                                 (cons sdl2-ffi::+kmod-rctrl+ :right-ctrl)
                                 (cons sdl2-ffi::+kmod-rgui+ :right-gui)
                                 (cons sdl2-ffi::+kmod-rshift+ :right-shift)
                                 (cons sdl2-ffi::+kmod-shift+ :shift)))


(defun sdl2-event-to-key-name-and-modifiers (driver event-key code state)
  (let ((shift? (logtest (logior sdl2-ffi::+kmod-lshift+ sdl2-ffi::+kmod-rshift+) state)))
    (let ((keysym-name (keysym-to-keysym-name code))
          (shifted-keysym-name (keysym-to-keysym-name (logior code #x100))))
      (if (and shift? (not (null shifted-keysym-name)))
          (setf keysym-name shifted-keysym-name))
      (let ((char
             (and (symbolp keysym-name)
                  (aref (symbol-name :|a|) 0))))
        (values
         char
         state
         keysym-name)))))
