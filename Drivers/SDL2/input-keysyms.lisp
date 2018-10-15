(in-package :cldk-driver-sdl2)

(defparameter mod-mappings (list (cons sdl2-ffi::+kmod-lalt+ :ALT-LEFT)
                                 (cons sdl2-ffi::+kmod-lctrl+ :CONTROL-LEFT)
                                 (cons sdl2-ffi::+kmod-lgui+ :SUPER-LEFT)
                                 (cons sdl2-ffi::+kmod-lshift+ :SHIFT-LEFT)
                                 (cons sdl2-ffi::+kmod-ralt+ :ALT-RIGHT)
                                 (cons sdl2-ffi::+kmod-rctrl+ :CONTROL-RIGHT)
                                 (cons sdl2-ffi::+kmod-rgui+ :SUPER-RIGHT)
                                 (cons sdl2-ffi::+kmod-rshift+ :SHIFT-RIGHT)))

(defparameter *keysm->modifier*
  `((:ALT-LEFT ,+meta-key+)
    (:ALT-RIGHT ,+meta-key+)
    (:CONTROL-LEFT ,+control-key+)
    (:CONTROL-RIGHT ,+control-key+)
    (:SHIFT-LEFT ,+shift-key+)
    (:SHIFT-RIGHT ,+shift-key+)
    (:SUPER-LEFT ,+super-key+)
    (:SUPER-RIGHT ,+hyper-key+)))

(defvar *key-modifiers* nil)
 
(defun map-keysym->modifier (keysym)
  (cdr (assoc keysym *keysm->modifier*)))

(defun decode-sdl2-mod-state (state)
  (let ((mods 0))
    (dolist (mod mod-mappings)
      (when (> (logand state (car mod)) 0)
        (let ((m (map-keysym->modifier (cdr mod))))
          (when m
            (setf mods (logior (car m) mods))))))
    mods))


(defun sdl2-event-to-key-name-and-modifiers (driver event-key code state)
  (let ((shift? (logtest (logior sdl2-ffi::+kmod-lshift+ sdl2-ffi::+kmod-rshift+) state)))
    (multiple-value-bind (keysym-name alpha-p)
        (keysym-to-keysym-name code)
      (let ((char
             (and (symbolp keysym-name)
                  (< code 255)
                  (code-char code))))
        (values
         char
         alpha-p
         (decode-sdl2-mod-state state)
         keysym-name)))))
