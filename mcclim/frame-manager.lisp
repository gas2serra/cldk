(in-package :clim-fb)

(defclass fb-frame-manager (frame-manager)
  ())

(defgeneric fb-frame-manager-mirrored-sheet-mixin-class (fm)
  (:method ((fm fb-frame-manager))
    'fb-mirrored-sheet-mixin))

(defun find-symbols (name-specs)
  (remove-if #'null (mapcar #'(lambda (x) (find-symbol-from-spec (first x) (rest x))) name-specs)))

(defun find-symbol-from-spec (package-spec name-components)
  (flet ((coerce-name-element (name-elt)
           (typecase name-elt
             (symbol (symbol-name name-elt))
             (sequence (coerce name-elt 'string))
             (t (princ-to-string name-elt)))))    
  (find-symbol
   (apply #'concatenate 'string (mapcar #'coerce-name-element name-components))
   package-spec)))

(defun find-first-defined-class (types)
  (first
   (remove-if #'null 
              (mapcar (lambda (class-name)
                        (find-class class-name nil))
                      types))))

(defun generate-standard-pane-specs (type)
  (let ((mapping (get type 'climi::concrete-pane-class-name)))
    `((,(symbol-package mapping) ,mapping)
      (:climi ,mapping)
      (:climi ,type #:-pane)
      (:climi ,type))))

(defun find-concrete-pane-class (type)
  (if (or (eql (symbol-package type)
               (find-package '#:clim))
          (eql (symbol-package type)
               (find-package '#:climi))
          (eql (symbol-package type)
               (find-package '#:keyword))
	  (get type 'climi::concrete-pane-class-name))
      (find-first-defined-class (find-symbols (generate-standard-pane-specs type)))
      type))

;;; if the pane is a subclass of basic-pane and it is not mirrored we create a new class.
(defun maybe-mirroring (fm port concrete-pane-class)
  (declare (ignore port))
  (when (and (not (subtypep concrete-pane-class 'mirrored-sheet-mixin))
             (subtypep concrete-pane-class 'top-level-sheet-pane))
    (let* ((concrete-pane-class-symbol (if (typep concrete-pane-class 'class)
                                          (class-name concrete-pane-class)
                                          concrete-pane-class))
	   (concrete-mirrored-pane-class (concatenate 'string
						      "FB-"
						      (symbol-name concrete-pane-class-symbol)
						      "-DUMMY"))
	   (concrete-mirrored-pane-class-symbol (find-symbol concrete-mirrored-pane-class
							     :clim-fb))
	   (superclasses (if (subtypep concrete-pane-class 'sheet-with-medium-mixin)
			     (list (fb-frame-manager-mirrored-sheet-mixin-class fm)
                                   'climi::always-repaint-background-mixin
				   concrete-pane-class-symbol)
			     (list (fb-frame-manager-mirrored-sheet-mixin-class fm)
                                   'climi::always-repaint-background-mixin
				   'permanent-medium-sheet-output-mixin
				   concrete-pane-class-symbol))))
      (unless concrete-mirrored-pane-class-symbol
	(setf concrete-mirrored-pane-class-symbol
	      (intern concrete-mirrored-pane-class :clim-fb))
	(eval
	 `(defclass ,concrete-mirrored-pane-class-symbol
	      ,superclasses
	    ()
	    (:metaclass ,(type-of (find-class concrete-pane-class-symbol))))))
      (setf concrete-pane-class (find-class concrete-mirrored-pane-class-symbol))))
  concrete-pane-class)

(defmethod make-pane-1 ((fm fb-frame-manager) (frame application-frame) type &rest args)
  (apply #'make-instance
	 (maybe-mirroring fm (port fm) (find-concrete-pane-class type))
	 :frame frame
	 :manager fm
	 :port (port frame)
	 args))

(defmethod adopt-frame :before ((fm fb-frame-manager) (frame menu-frame))
  ;; Temporary kludge.
  (log:info "====ADOPT MENU ~A" (slot-value frame 'climi::top))
  (when (eq (slot-value frame 'climi::top) nil)
    (multiple-value-bind (x y)
        (cldk:screen-pointer-position (fb-port-server (port fm)))
      (incf x 10)
      (setf (slot-value frame 'climi::left) x
            (slot-value frame 'climi::top) y))))

(defmethod adopt-frame :after ((fm fb-frame-manager) (frame menu-frame))
  (when (sheet-enabled-p (slot-value frame 'climi::top-level-sheet))
    (cldk:show-window (sheet-direct-mirror (slot-value frame 'top-level-sheet)))))

(defmethod tell-window-manager-about-space-requirements ((pane top-level-sheet-pane))
  (multiple-value-bind (w h x y) (climi::frame-geometry* (pane-frame pane))
    (declare (ignore w h))
    (let ((q (compose-space pane)))
      (let ((mirror (sheet-direct-mirror pane)))
        (log:info "***TELL> ~A ~A ~A" (fb-mirrored-sheet-state pane)
                  (list x y)
                  (list
                   (round (space-requirement-width q))
                   (round (space-requirement-height q))))
        ;;(setf x (slot-value (pane-frame pane) 'climi::left))
        ;;(setf y (slot-value (pane-frame pane) 'climi::top))
        (when (fb-mirrored-sheet-state pane)
          (cldk:set-window-hints 
           mirror
           :x x :y y
           :width (round (space-requirement-width q))
           :height (round (space-requirement-height q))
           :max-width (min 65535 (round (space-requirement-max-width q)))
           :max-height (min 65535 (round (space-requirement-max-height q)))
           :min-width (round (space-requirement-min-width q))
           :min-height (round (space-requirement-min-height q))))))))

(defmethod tell-window-manager-about-space-requirements ((pane t))
  ;; hmm
  nil)

(defmethod note-space-requirements-changed :after ((graft fb-graft) pane)
  (tell-window-manager-about-space-requirements pane))
