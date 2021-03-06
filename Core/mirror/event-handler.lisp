(in-package :cldk-internals)


;;; event handler

(defclass event-handler (display-driver-callback-handler)
  ((cur-x :initform nil
          :reader event-handler-cur-x)
   (cur-y :initform nil
          :reader event-handler-cur-y)
   (cur-root-x :initform nil
               :reader event-handler-cur-root-x)
   (cur-root-y :initform nil
               :reader event-handler-cur-root-y)
   (modifiers :initform 0
              :reader event-handler-modifiers)
   (pressed-buttons :initform 0
                    :reader event-handler-pressed-buttons)))

(defgeneric handle-button-event (handler kind pointer button
                                  window timestamp)
  (:method ((handler event-handler) kind pointer button
            window timestamp)
    )
  (:method :before ((handler event-handler) kind pointer button 
                   window timestamp)
           (with-slots (pressed-buttons modifiers) handler
             (when (eql kind :release)
               (setf pressed-buttons (logxor button pressed-buttons)))))
  (:method :after ((handler event-handler) kind pointer button 
                   window timestamp)
           (with-slots (pressed-buttons modifiers) handler
             (when (eql kind :press)
               (setf pressed-buttons (logior button pressed-buttons))))))


(defgeneric handle-scroll-event (handler pointer dx dy
                                 window timestamp)
  (:method ((handler event-handler) pointer dx dy window timestamp)
    ))

(defgeneric handle-motion-event (handler pointer x y
                                  root-x root-y
                                  window timestamp)
  (:method ((handler event-handler) pointer x y
            root-x root-y window timestamp)
    )
  (:method :after ((handler event-handler) pointer x y
                   root-x root-y window timestamp)
           (with-slots (cur-x cur-y cur-root-x cur-root-y) handler
             (setf cur-x x
                   cur-y y
                   cur-root-x root-x
                   cur-root-y root-y))))

(defgeneric handle-key-event (handler kind keyname character modifiers
                               window timestamp)
  (:method ((handler event-handler) kind keyname character modifiers
            window timestamp)
    )
  (:method :before ((handler event-handler) kind keyname character mmodifiers
                    window timestamp)
           (with-slots (modifiers) handler
             (setf modifiers mmodifiers))))
    
(defgeneric handle-enter-event (handler pointer x y root-x root-y win time)
  (:method ((handler event-handler) pointer
            x y root-x root-y
            window timestamp)
    )
  (:method :before ((handler event-handler) pointer x y
                    root-x root-y window timestamp)
           (with-slots (cur-x cur-y cur-root-x cur-root-y) handler
             (setf cur-x x
                   cur-y y
                   cur-root-x root-x
                   cur-root-y root-y))))

(defgeneric handle-leave-event (handler pointer win time)
  (:method ((handler event-handler) pointer
            window timestamp)
    )
  (:method :after ((handler event-handler) pointer
                   window timestamp)
           (with-slots (cur-x cur-y cur-root-x cur-root-y) handler
             (setf cur-x nil
                   cur-y nil
                   cur-root-x nil
                   cur-root-y nil))))

(defgeneric handle-configure-event (handler win x y w h time)
  (:method ((handler event-handler) win x y w h time)
    ))

(defgeneric handle-repaint-event (handler win x y w h time)
  (:method ((handler event-handler) win x y w h time)
    ))

(defgeneric handle-destroy-event (handler win time)
  (:method ((handler event-handler) win time)
    ))

(defgeneric handle-wm-delete-event (handler win time)
  (:method ((handler event-handler) win time)
    ))

;;;
;;; pretty print
;;;

(defun button2keyword (button)
  (let ((button-mapping #.(vector :left
                                  :middle
                                  :right
                                  :x1
                                  :x2)))
        (dotimes (i 9)
          (when (logbitp i button)
            (return (aref button-mapping i))))))

(defun buttons2keywords (button)
  (let ((button-mapping #.(vector :left
                                  :middle
                                  :right
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

;;;
;;; log event handler
;;;
(defparameter *events-to-log-list* '(:delete :repaint :configure
                                     :enter-leave :button :scroll :motion :key
                                     :modifiers))

(defclass log-event-handler (event-handler)
  ((to-log :initform :all
           :accessor event-handler-events-to-log)))


(defmethod handle-button-event ((handler log-event-handler) kind
                                pointer button win timestamp)
  (with-slots (to-log cur-x cur-y cur-root-x cur-root-y) handler
    (when (or (eql to-log :all) (member :button to-log))
      (log:info "win:~A k:~A p:~A b:~A p:~A[~A]" win kind pointer
                (list button (button2keyword button))
                (list cur-x cur-y) (list cur-root-x cur-root-y))
      (when (or (eql to-log :all) (member :modifiers to-log))
        (with-slots (modifiers pressed-buttons) handler
          (log:info "keys:~A buts:~A"
                    (modifiers2keywords modifiers)
                    (buttons2keywords pressed-buttons)))))))

(defmethod handle-scroll-event ((handler log-event-handler) pointer dx dy
                               win timestamp)
  (with-slots (to-log) handler
    (when (or (eql to-log :all) (member :scroll to-log))
      (log:info "win:~A  p:~A w:~A" win pointer 
                (list dx dy))
      (when (or (eql to-log :all) (member :modifiers to-log))
        (with-slots (modifiers pressed-buttons) handler
          (log:info "keys:~A buts:~A"
                    (modifiers2keywords modifiers)
                    (buttons2keywords pressed-buttons)))))))

(defmethod handle-motion-event ((handler log-event-handler) pointer x y
                                root-x root-y
                                win timestamp)
  (with-slots (to-log) handler
    (when (or (eql to-log :all) (member :motion to-log))
      (log:info "win:~A  p:~A p:~A[~A]" win pointer
                (list x y) (list root-x root-y))
      (when (and (listp to-log) (member :modifiers to-log))
        (with-slots (modifiers pressed-buttons) handler
          (log:info "keys:~A buts:~A"
                    (modifiers2keywords modifiers)
                    (buttons2keywords pressed-buttons)))))))

(defmethod handle-key-event ((handler log-event-handler) kind
                             keyname character modifiers
                             win timestamp)
  (with-slots (to-log) handler
    (when (or (eql to-log :all) (member :key to-log))
      (log:info "win:~A  k:~A name:~A char:~A mod:~A" win kind keyname character
                (list modifiers (modifiers2keywords modifiers)))
      (when (or (eql to-log :all) (member :modifiers to-log))
        (with-slots (modifiers pressed-buttons) handler
          (log:info "keys:~A buts:~A"
                    (modifiers2keywords modifiers)
                    (buttons2keywords pressed-buttons)))))))
  
(defmethod handle-enter-event ((handler log-event-handler) pointer
                               x y x-root y-root
                               win timestamp)
  (with-slots (to-log cur-x cur-y cur-root-x cur-root-y) handler
    (when (or (eql to-log :all) (member :enter-leave to-log))
      (log:info "win:~A  p:~A p:~A[~A]" win pointer
                (list cur-x cur-y) (list cur-root-x cur-root-y)))))

(defmethod handle-leave-event ((handler log-event-handler) pointer
                               win timestamp)
  (with-slots (to-log cur-x cur-y cur-root-x cur-root-y) handler
    (when (or (eql to-log :all) (member :enter-leave to-log))
      (log:info "win:~A  p:~A p:~A[~A]" win pointer
                (list cur-x cur-y) (list cur-root-x cur-root-y)))))

(defmethod handle-configure-event ((handler log-event-handler) win x y w h
                                   timestamp)
  (with-slots (to-log) handler
    (when (or (eql to-log :all) (member :configure to-log))
      (log:info "win:~A  x:~A y:~A  w:~A h:~A" win x y w h))))

(defmethod handle-repaint-event ((handler log-event-handler) win x y w h
                                 timestamp)
  (with-slots (to-log) handler
    (when (or (eql to-log :all) (member :repaint to-log))
      (log:info "win:~A  x:~A y:~A  w:~A h:~A" win x y w h))))

(defmethod handle-wm-delete-event ((handler log-event-handler) win timestamp)
  (with-slots (to-log) handler
    (when (or (eql to-log :all) (member :delete to-log))
      (log:info "win:~A" win))))

(setf *default-event-handler* (make-instance 'event-handler))

;;;
;;;
;;;

(defmacro <e- (kernel fn &rest args)
  `(within-user-mode (,kernel :block-p nil)
                     (funcall ,fn (event-handler ,kernel) ,@args)))

(defmethod driver-cb-window-configuration-event ((handler event-handler) kernel win x y width height time)
  (check-kernel-mode)
  (<e- kernel #'handle-configure-event win x y width height time))

(defmethod driver-cb-repaint-event ((handler event-handler) kernel win x y width height time)
  (check-kernel-mode)
  (<e- kernel #'handle-repaint-event win x y width height time))

(defmethod driver-cb-scroll-event ((handler event-handler) kernel pointer dx dy win timestamp)
  (check-kernel-mode)
  (<e- kernel #'handle-scroll-event pointer dx dy win timestamp))

(defmethod driver-cb-button-event ((handler event-handler) kernel kind pointer button win timestamp)
  (check-kernel-mode)
  (<e- kernel #'handle-button-event kind pointer button win timestamp))

(defmethod driver-cb-motion-event ((handler event-handler) kernel pointer x y root-x root-y
                              win timestamp)
  (check-kernel-mode)
  (<e- kernel #'handle-motion-event pointer x y root-x root-y
       win timestamp))

(defmethod driver-cb-key-event ((handler event-handler) kernel kind keyname character modifiers
                           win timestamp)
  (check-kernel-mode)
  (<e- kernel #'handle-key-event kind keyname character modifiers
       win timestamp))

(defmethod driver-cb-enter-event ((handler event-handler) kernel pointer x y root-x root-y win time)
  (check-kernel-mode)
  (<e- kernel #'handle-enter-event pointer x y root-x root-y win time))

(defmethod driver-cb-leave-event ((handler event-handler) kernel pointer win time)
  (check-kernel-mode)
  (<e- kernel #'handle-leave-event pointer win time))

(defmethod driver-cb-wm-delete-event ((handler event-handler) kernel win time)
  (check-kernel-mode)
  (<e- kernel #'handle-wm-delete-event win time))
