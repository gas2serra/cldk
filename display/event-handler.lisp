(in-package :cldk-internals)

;;; Constants dealing with events
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +pointer-left-button+   #x01)
  (defconstant +pointer-middle-button+ #x02)
  (defconstant +pointer-right-button+  #x04)
  (defconstant +pointer-wheel-up+      #x08)
  (defconstant +pointer-wheel-down+    #x10)
  (defconstant +pointer-wheel-left+    #x20)
  (defconstant +pointer-wheel-right+   #x40)
  (defconstant +pointer-x1-button+     #x80)
  (defconstant +pointer-x2-button+     #x100)
  
  (defconstant +shift-key+             #x0100)
  (defconstant +control-key+           #x0200)
  (defconstant +meta-key+              #x0400)
  (defconstant +super-key+             #x0800)
  (defconstant +hyper-key+             #x1000)
  (defconstant +alt-key+               #x2000))

;;; event handler

(defclass event-handler ()
  ((cur-x :initform nil
          :reader event-handler-cur-x)
   (cur-y :initform nil
          :reader event-handler-cur-y)
   (cur-root-x :initform nil
               :reader event-handler-cur-root-x)
   (cur-root-y :initform nil
               :reader event-handler-cur-root-y)
   (cur-modifier-state :initform 0
                       :reader event-handler-modifier-state)
   (cur-pressed-buttons :initform 0
                        :reader event-handler-pressed-buttons)))

(defun button2keyword (button)
  (let ((button-mapping #.(vector :left
                                  :middle
                                  :right
                                  :wheel-up
                                  :wheel-down
                                  :wheel-left
                                  :wheel-right
                                  :x1
                                  :x2)))
        (dotimes (i 9)
          (when (logbitp i button)
            (return (aref button-mapping i))))))

(defun buttons2keywords (button)
  (let ((button-mapping #.(vector :left
                                  :middle
                                  :right
                                  :wheel-up
                                  :wheel-down
                                  :wheel-left
                                  :wheel-right
                                  :x1
                                  :x2))
        (ks nil))
    (dotimes (i 9)
      (when (logbitp i button)
        (setf ks (cons (aref button-mapping i) ks))))
    ks))

(defun modifiers2keywords (button)
  (let ((button-mapping #.(vector :shift
                                  :control
                                  :meta
                                  :super
                                  :hyper
                                  :alt))
        (ks nil))
    (dotimes (i 6)
      (when (logbitp (+ 8 i) button)
        (setf ks (cons (aref button-mapping i) ks))))
    ks))

(defgeneric handler-button-event (handler kind pointer button
                                  window timestamp)
  (:method ((handler event-handler) kind pointer button
            window timestamp)
    #+nil (log:info "button: ~A ~A ~A ~A ~A"
              kind pointer (list button (button2keyword button))
              window timestamp)
    #+nil (with-slots (cur-x cur-y cur-root-x cur-root-y cur-pressed-buttons cur-modifier-state) handler
      (log:info "pos: ~A ~A"
                (list cur-x cur-y) (list cur-root-x cur-root-y))
      (log:info (modifiers2keywords cur-modifier-state)
                (buttons2keywords cur-pressed-buttons))))
  (:method :before ((handler event-handler) kind pointer button 
                   window timestamp)
           (with-slots (cur-pressed-buttons cur-modifier-state) handler
             (if (eq kind :press)
               nil ;;(setf cur-pressed-buttons (logior button cur-pressed-buttons))
               nil)));;(setf cur-pressed-buttons (logxor button cur-pressed-buttons)))))

  (:method :after ((handler event-handler) kind pointer button 
                   window timestamp)
           (with-slots (cur-pressed-buttons cur-modifier-state) handler
             (if (eq kind :press)
               (setf cur-pressed-buttons (logior button cur-pressed-buttons))
               (setf cur-pressed-buttons (logxor button cur-pressed-buttons))))))


(defgeneric handler-wheel-event (handler kind pointer
                                 window timestamp)
  (:method ((handler event-handler) kind pointer window timestamp)
    #+nil (with-slots (cur-x cur-y cur-root-x cur-root-y cur-pressed-buttons cur-modifier-state) handler
      (log:info "wheel: ~A ~A ~A ~A"
                (list kind (button2keyword kind)) pointer window timestamp)
      (log:info "pos: ~A ~A"
                (list cur-x cur-y) (list cur-root-x cur-root-y))
      (log:info (modifiers2keywords cur-modifier-state)
                (buttons2keywords cur-pressed-buttons)))))

(defgeneric handler-motion-event (handler pointer x y
                                  root-x root-y
                                  window timestamp)
  (:method ((handler event-handler) pointer x y
            root-x root-y window timestamp)
    #+nil (log:info "motion: ~A ~A ~A ~A ~A"
              pointer (list x y) (list root-x root-y)
              window timestamp)
    #+nil (with-slots (cur-pressed-buttons) handler
      (log:info "buttons: ~A " cur-pressed-buttons)))
  (:method :after ((handler event-handler) pointer x y
                   root-x root-y window timestamp)
           (with-slots (cur-x cur-y cur-root-x cur-root-y) handler
             (setf cur-x x
                   cur-y y
                   cur-root-x root-x
                   cur-root-y root-y))))

(defgeneric handler-key-event (handler kind keyname character modifiers
                               window timestamp)
  (:method ((handler event-handler) kind keyname character modifiers
            window timestamp)
    #+nil (log:info "key: ~A ~A ~A ~A ~A ~A"
              kind keyname character (list modifiers (modifiers2keywords modifiers))
              window timestamp))
  (:method :after ((handler event-handler) kind keyname character modifiers
                   window timestamp)
           (with-slots (cur-modifier-state) handler
             (setf cur-modifier-state modifiers))))
    

(defgeneric handler-enter-leave-event (handler kind pointer win time)
  (:method ((handler event-handler) kind pointer
            window timestamp)
    #+nil (log:info "enter/leave: ~A ~A ~A ~A"
              kind pointer
              window timestamp)
    (when (eql kind :leave)
      (with-slots (cur-x cur-y cur-root-x cur-root-y) handler
        (setf cur-x nil
              cur-y nil
              cur-root-x nil
              cur-root-y nil)))))

(defgeneric handler-configure-event (handler win x y w h time)
  (:method ((handler event-handler) win x y w h time)
    ))

(defgeneric handler-repaint-event (handler win x y w h time)
  (:method ((handler event-handler) win x y w h time)
    ))

(defgeneric handler-destroy-event (handler win time)
  (:method ((handler event-handler) win time)
    ))

(defgeneric handler-wm-delete-event (handler win time)
  (:method ((handler event-handler) win time)
    ))

(setf *default-event-handler* (make-instance 'event-handler))
