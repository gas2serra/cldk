(in-package :cldk-demo)

(defparameter *render-image-tests* (make-hash-table :test 'equal))

(defparameter *render-image-width* 510)
(defparameter *render-image-height* 700)
(defparameter *render-image-border-width* 5)

(defparameter *testing-image-directory* (uiop/pathname:merge-pathnames* "Examples/images/" (asdf:system-source-directory (asdf:find-system :mcclim))))
(defparameter *testing-image-files* '("RGBXPLORER8.png"
                                      "White_Balance_RGB.png"
                                      "MicroGrayTest.png"))

(defparameter *testing-image-rgb-file* "RGBXPLORER8.png")
(defparameter *testing-image-bn1-file* "White_Balance_RGB.png")
(defparameter *testing-image-bn2-file* "MicroGrayTest.png")

(defstruct render-image-test name description drawer)

(defmacro define-render-image-test (name arglist description &body body)
  (check-type name string)
  (check-type description string)
  `(setf (gethash ,name *render-image-tests*)
         (make-render-image-test :name ,name
                                 :description ,description
                                 :drawer (lambda ,arglist ,@body))))

(define-application-frame render-image-tests ()
  ((signal-condition-p :initform nil)
   (family-option :initform :default)
   (current-selection :initform nil))
  (:panes
   (output :application
           :width *render-image-width*
           :height *render-image-height*
           :scroll-bars :both
           :display-function #'render-image-display-output)
   (description :application
                :scroll-bar :both)
   (selector :list-pane
             :mode :exclusive
             :name-key #'render-image-test-name
             :items (sort (loop for x being the hash-values of *render-image-tests*
                                collect x) #'string< :key #'render-image-test-name)
             :value-changed-callback #'render-image-update-selection)
   (family-option
    (clim:with-radio-box (:orientation :vertical
				       :value-changed-callback
                                       'render-image-update-family-option)
      (clim:radio-box-current-selection "default")
      "two-dim-array"
      "opticl"
      "medium"))
   (condition-option
    (clim:with-radio-box (:orientation :vertical
				       :value-changed-callback
                                       'render-image-update-condition-option)
      (clim:radio-box-current-selection "message")
      "break")))
  (:layouts
   (default
       (spacing (:thickness 3)
         (horizontally ()
           (vertically ()
             (spacing (:thickness 3)
               (clim-extensions:lowering ()
                 (scrolling (:scroll-bar :vertical ) selector)))
             (labelling (:label "Family")
               family-option)
             (labelling (:label "Condition")
               condition-option))
           (vertically ()
             (spacing (:thickness 3)
               (clim-extensions:lowering ()
                 (horizontally ()
                   (labelling (:label "Stream")
                     (climi::bordering (:border-width *render-image-border-width*)
                       output)))))
             (spacing (:thickness 3)
               (clim-extensions:lowering ()
                 (scrolling (:scroll-bar :vertical :height 200) description)))))))))
   
(defun render-image-update-condition-option (this-gadget selected-gadget)
  (declare (ignore this-gadget))
  (with-slots (signal-condition-p) clim:*application-frame*
    (setf signal-condition-p
	  (string= (clim:gadget-label selected-gadget) "break"))))

(defun render-image-update-family-option (this-gadget selected-gadget)
  (declare (ignore this-gadget))
  (with-slots (family-option) clim:*application-frame*
    (setf family-option
	  (cond ((string= (clim:gadget-label selected-gadget) "default")
                 :default)
                ((string= (clim:gadget-label selected-gadget) "two-dim-array")
                 :two-dim-array)
                ((string= (clim:gadget-label selected-gadget) "opticl")
                 :opticl)
                ((string= (clim:gadget-label selected-gadget) "medium")
                 (clim:sheet-medium (get-frame-pane *application-frame* 'output))))))
  (window-clear (get-frame-pane *application-frame* 'description))
  (redisplay-frame-pane *application-frame* (get-frame-pane *application-frame* 'output) :force-p t))

(defun render-image-update-selection (pane item)
  (declare (ignore pane))
  (with-slots (current-selection) clim:*application-frame*
    (setf current-selection item))
  (window-clear (get-frame-pane *application-frame* 'description))
  (redisplay-frame-pane *application-frame* (get-frame-pane *application-frame* 'output) :force-p t))

(defun render-image-display-output (frame pane)
  (declare (ignore pane))
  (let ((output (get-frame-pane frame 'output))
        (item (slot-value frame 'current-selection)))
    (let ((description (get-frame-pane *application-frame* 'description)))
      (when item
        (with-text-style (description (make-text-style :sans-serif :roman :normal))
          (write-string (render-image-test-description item) description)
          (terpri description))
        (if (slot-value *application-frame* 'signal-condition-p)
            (clim:with-drawing-options (output :clipping-region
                                               (clim:make-rectangle* 0 0 *render-image-width* *render-image-height*))
              (clim:draw-rectangle* output 0 0 *render-image-width* *render-image-height* :filled t
                                    :ink clim:+grey90+)
              (time (funcall (render-image-test-drawer item) output)))
            (handler-case
                (clim:with-drawing-options (output :clipping-region
                                                   (clim:make-rectangle* 0 0 *render-image-width* *render-image-height*))
                  (clim:draw-rectangle* output 0 0 *render-image-width* *render-image-height* :filled t
                                        :ink clim:+grey90+)
                  (time (funcall (render-image-test-drawer item) output)))
              (serious-condition (condition)
                (clim:with-drawing-options (description :ink +red+)
                  (format description "~a~%" condition)))))))))


(defun run-render-image-tests ()
  (run-frame-top-level
   (make-application-frame
    'render-image-tests)))

(defun render-image-test-make-rgba-image (w h)
  (with-slots (family-option) clim:*application-frame*
    (let* ((image (cldk-render:make-image family-option :rgba w h)))
      image)))

(defun render-image-test-make-rgb-image (w h)
  (with-slots (family-option) clim:*application-frame*
    (let* ((image (cldk-render:make-image family-option :rgb w h)))
      image)))

(defun render-image-test-make-gray-image (w h)
  (with-slots (family-option) clim:*application-frame*
    (let* ((image (cldk-render:make-image family-option :gray w h)))
      image)))

(defun render-image-test-read-rgba-image (path)
  (with-slots (family-option) clim:*application-frame*
    (let* ((image (cldk-render:read-image path
                                          :type :rgba
                                          :medium family-option)))
      image)))

(defun render-image-test-read-rgb-image (path)
  (with-slots (family-option) clim:*application-frame*
    (let* ((image (cldk-render:read-image path
                                          :type :rgb
                                          :medium family-option)))
      image)))

(defun render-image-test-read-gray-image (path)
  (with-slots (family-option) clim:*application-frame*
    (let* ((image (cldk-render:read-image path
                                          :type :gray
                                          :medium family-option)))
      image)))

(defun render-image-test-fill-rgba-image (image color)
  (let ((fn (cldk-render:image-rgba-set-fn image))
        (w (cldk-render:image-width image))
        (h (cldk-render:image-height image)))
    (multiple-value-bind (r g b)
        (cldk-render:color->octets color)
      (dotimes (x w)
        (dotimes (y h)
          (funcall fn x y r g b (round (/ (* x y 255) (* w h)))))))
    image))

(defun render-image-test-fill-rgb-image (image color)
  (let ((fn (cldk-render:image-rgb-set-fn image))
        (w (cldk-render:image-width image))
        (h (cldk-render:image-height image)))
    (multiple-value-bind (r g b)
        (cldk-render:color->octets color)
      (dotimes (x w)
        (dotimes (y h)
          (funcall fn x y r g b))))
      image))

(defun render-image-test-fill-gray-image (image color)
  (let ((fn (cldk-render:image-gray-set-fn image))
        (w (cldk-render:image-width image))
        (h (cldk-render:image-height image)))
    (multiple-value-bind (r g b)
        (cldk-render:color->octets color)
      (dotimes (x w)
        (dotimes (y h)
          (funcall fn x y (round (+ r g b) 3)))))
    image))

(define-render-image-test "01) simple fill rgba" (stream)
    "Simple drawing of two dimensional array of pixels: white, black;
red, green, blue;
purple, olive."
  (flet ((draw-rect (color w h)
           (let* ((image (render-image-test-make-rgba-image 90 70)))
             (render-image-test-fill-rgba-image image color)
             (cldk-render:draw-image* stream image w h))))
    (draw-rect +white+ 10 10)
    (draw-rect +black+ 110 10)
    (draw-rect +red+ 10 100)
    (draw-rect +green+ 110 100)
    (draw-rect +blue+ 210 100)
    (draw-rect +purple+ 10 200)
    (draw-rect +olive-drab+ 110 200)))

(define-render-image-test "01) simple fill rgb" (stream)
    "Simple drawing of two dimensional array of pixels: white, black;
red, green, blue;
purple, olive."
  (flet ((draw-rect (color w h)
           (let* ((image (render-image-test-make-rgb-image 90 70)))
             (render-image-test-fill-rgb-image image color)
             (cldk-render:draw-image* stream image w h))))
    (draw-rect +white+ 10 10)
    (draw-rect +black+ 110 10)
    (draw-rect +red+ 10 100)
    (draw-rect +green+ 110 100)
    (draw-rect +blue+ 210 100)
    (draw-rect +purple+ 10 200)
    (draw-rect +olive-drab+ 110 200)))

(define-render-image-test "01) simple fill gray" (stream)
    "Simple drawing of two dimensional array of pixels: white, black;
red, green, blue;
purple, olive."
  (flet ((draw-rect (color w h)
           (let* ((image (render-image-test-make-gray-image 90 70)))
             (render-image-test-fill-gray-image image color)
             (cldk-render:draw-image* stream image w h))))
    (draw-rect +white+ 10 10)
    (draw-rect +black+ 110 10)
    (draw-rect +red+ 10 100)
    (draw-rect +green+ 110 100)
    (draw-rect +blue+ 210 100)
    (draw-rect +purple+ 10 200)
    (draw-rect +olive-drab+ 110 200)))

(define-render-image-test "02) read rgb image" (stream)
    ""
  (let ((path (uiop/pathname:merge-pathnames* *testing-image-rgb-file* *testing-image-directory*)))
    (let ((image (render-image-test-read-rgb-image path)))
      (cldk-render:draw-image* stream image 10 0))))

(define-render-image-test "02) read gray images" (stream)
    ""
  (let ((path (uiop/pathname:merge-pathnames* *testing-image-bn1-file* *testing-image-directory*)))
    (let ((image (render-image-test-read-gray-image path)))
      (cldk-render:draw-image* stream image 10 0)))
  (let ((path (uiop/pathname:merge-pathnames* *testing-image-bn2-file* *testing-image-directory*)))
    (let ((image (render-image-test-read-gray-image path)))
      (cldk-render:draw-image* stream image 10 300))))

(define-render-image-test "03) translate " (stream)
    ""
  (flet ((test (stream transformation)
           (let ((path (uiop/pathname:merge-pathnames* *testing-image-rgb-file* *testing-image-directory*)))
             (let ((image (render-image-test-read-rgb-image path)))
               (cldk-render:draw-image* stream image 0 0
                                        :transformation transformation)))))
    (test stream
          (clim:make-translation-transformation 10 10))
    (test stream 
          (clim:make-translation-transformation 10 360))))

(define-render-image-test "04) clipping " (stream)
    ""
  (flet ((test (transformation clipping-region)
           (let ((path (uiop/pathname:merge-pathnames* *testing-image-rgb-file* *testing-image-directory*)))
             (let ((image (render-image-test-read-rgb-image path)))
               (with-bounding-rectangle* (x1 y1 x2 y2)
                   clipping-region
                 (draw-rectangle* stream x1 y1 x2 y2 :ink +green+ :filled nil))
               (with-bounding-rectangle* (x1 y1 x2 y2)
                   (transform-region transformation clipping-region)
                 (draw-rectangle* stream x1 y1 x2 y2 :ink +blue+ :filled nil))
               (cldk-render:draw-image* stream image 0 0
                                        :transformation transformation
                                        :clipping-region clipping-region)))))
    (test
     (clim:make-translation-transformation 10 10)
     (make-rectangle* 50 50 250 250))
    (test
     (clim:make-translation-transformation 10 360)
     (make-rectangle* 50 50 250 250))))

(define-render-image-test "05) with translation" (stream)
    ""
  (let ((path (uiop/pathname:merge-pathnames* *testing-image-rgb-file* *testing-image-directory*)))
    (with-translation (stream 10 10)
      (let ((image (render-image-test-read-rgb-image path)))
        (cldk-render:draw-image* stream image 10 0)))
    (with-translation (stream (- 10) 360)
      (let ((image (render-image-test-read-rgb-image path)))
        (cldk-render:draw-image* stream image 10 0)))))

(define-render-image-test "06) design draw" (stream)
    ""
  (flet ((test (stream h)
           (let ((path (uiop/pathname:merge-pathnames* *testing-image-rgb-file* *testing-image-directory*)))
             (let ((image (render-image-test-read-rgb-image path)))
               (draw-design stream (cldk-render:make-image-design image) :x 10 :y h)))))
    (test stream 10)
    (test stream 360)))

(define-render-image-test "07) design translate " (stream)
    ""
  (flet ((test (transformation)
           (let ((path (uiop/pathname:merge-pathnames* *testing-image-rgb-file* *testing-image-directory*)))
             (let ((image (render-image-test-read-rgb-image path)))
               (draw-design stream (cldk-render:make-image-design image) :x 0 :y 0
                            :transformation transformation)))))
    (test (clim:make-translation-transformation 10 10))
    (test (clim:make-translation-transformation 10 360))))

(define-render-image-test "08) design clipping " (stream)
    ""
  (flet ((test (transformation clipping-region)
           (let ((path (uiop/pathname:merge-pathnames* *testing-image-rgb-file* *testing-image-directory*)))
             (let ((image (render-image-test-read-rgb-image path)))
               (with-bounding-rectangle* (x1 y1 x2 y2)
                   clipping-region
                 (draw-rectangle* stream x1 y1 x2 y2 :ink +green+ :filled nil))
               (with-bounding-rectangle* (x1 y1 x2 y2)
                   (transform-region transformation clipping-region)
                 (draw-rectangle* stream x1 y1 x2 y2 :ink +blue+ :filled nil))
               (draw-design stream (cldk-render:make-image-design image) :x 0 :y 0
                            :transformation transformation
                            :clipping-region clipping-region)))))
    (test
     (clim:make-translation-transformation 10 10)
     (make-rectangle* 50 50 250 250))
    (test 
     (clim:make-translation-transformation 10 360)
     (make-rectangle* 50 50 250 250))))

(define-render-image-test "09) design with-translation" (stream)
    ""
  (flet ((test (h)
           (let ((path (uiop/pathname:merge-pathnames* *testing-image-rgb-file* *testing-image-directory*)))
             (let ((image (render-image-test-read-rgb-image path)))
               (draw-design stream (cldk-render:make-image-design image) :x 10 :y h)))))
    (with-translation (stream 10 10)
      (test 0))
    (with-translation (stream (- 10) 360)
      (test 0))))


(define-render-image-test "10) alpha" (stream)
    ""
  (flet ((test (w h color)
           (let ((path (uiop/pathname:merge-pathnames* *testing-image-bn2-file* *testing-image-directory*)))
             (let* ((alpha-image
                     (render-image-test-read-gray-image path))
                    (image (render-image-test-make-rgba-image
                            (cldk-render:image-width alpha-image)
                            (cldk-render:image-height alpha-image))))
               (render-image-test-fill-rgba-image image color)
               (cldk-render:copy-alpha-channel alpha-image
                                               0 0
                                               (cldk-render:image-width alpha-image)
                                               (cldk-render:image-height alpha-image)
                                               image
                                               0 0)
               (cldk-render:draw-image* stream image w h)))))
    (test 10 10 +red+)
    (test 10 360 +green+)))

(define-render-image-test "11) blend rgba" (stream)
    ""
  (flet ((test (w h alpha)
           (let ((path (uiop/pathname:merge-pathnames* *testing-image-bn2-file* *testing-image-directory*)))
             (let* ((alpha-image
                     (render-image-test-read-gray-image path))
                    (image (render-image-test-make-rgba-image
                            (cldk-render:image-width alpha-image)
                            (cldk-render:image-height alpha-image)))
                    (bg-image (render-image-test-make-rgba-image
                               (cldk-render:image-width alpha-image)
                               (cldk-render:image-height alpha-image))))
               (render-image-test-fill-rgba-image image +red+)
               (render-image-test-fill-rgba-image bg-image +yellow+)
               (cldk-render:copy-alpha-channel alpha-image
                                               0 0
                                               (cldk-render:image-width alpha-image)
                                               (cldk-render:image-height alpha-image)
                                               image
                                               0 0)
               (cldk-render:blend-image image
                                        0 0
                                        (cldk-render:image-width alpha-image)
                                        (cldk-render:image-height alpha-image)
                                        bg-image
                                        0 0 :alpha alpha)
               (cldk-render:draw-image* stream bg-image w h)))))
    (test
     10 10 255)
    (test 
     10 360 128)))

(define-render-image-test "11) blend rgb" (stream)
    ""
  (flet ((test (w h alpha)
           (let ((path (uiop/pathname:merge-pathnames* *testing-image-bn2-file* *testing-image-directory*)))
             (let* ((alpha-image
                     (render-image-test-read-gray-image path))
                    (image (render-image-test-make-rgba-image
                            (cldk-render:image-width alpha-image)
                            (cldk-render:image-height alpha-image)))
                    (bg-image (render-image-test-make-rgb-image
                               (cldk-render:image-width alpha-image)
                               (cldk-render:image-height alpha-image))))
               (render-image-test-fill-rgba-image image +red+)
               (render-image-test-fill-rgb-image bg-image +yellow+)
               (cldk-render:copy-alpha-channel alpha-image
                                               0 0
                                               (cldk-render:image-width alpha-image)
                                               (cldk-render:image-height alpha-image)
                                               image
                                               0 0)
               (cldk-render:blend-image image
                                        0 0
                                        (cldk-render:image-width alpha-image)
                                        (cldk-render:image-height alpha-image)
                                        bg-image
                                        0 0 :alpha alpha)
               (cldk-render:draw-image* stream bg-image w h)))))
    (test
     10 10 255)
    (test 
     10 360 128)))

(define-render-image-test "11) blend gray" (stream)
    ""
  (flet ((test (w h alpha)
           (let ((path (uiop/pathname:merge-pathnames* *testing-image-bn2-file* *testing-image-directory*)))
             (let* ((alpha-image
                     (render-image-test-read-gray-image path))
                    (image (render-image-test-make-rgba-image
                            (cldk-render:image-width alpha-image)
                            (cldk-render:image-height alpha-image)))
                    (bg-image (render-image-test-make-gray-image
                               (cldk-render:image-width alpha-image)
                               (cldk-render:image-height alpha-image))))
               (render-image-test-fill-rgba-image image +red+)
               (render-image-test-fill-gray-image bg-image +yellow+)
               (cldk-render:copy-alpha-channel alpha-image
                                               0 0
                                               (cldk-render:image-width alpha-image)
                                               (cldk-render:image-height alpha-image)
                                               image
                                               0 0)
               (cldk-render:blend-image image
                                        0 0
                                        (cldk-render:image-width alpha-image)
                                        (cldk-render:image-height alpha-image)
                                        bg-image
                                        0 0 :alpha alpha)
               (cldk-render:draw-image* stream bg-image w h)))))
    (test
     10 10 255)
    (test 
     10 360 128)))

(define-render-image-test "12) crop" (stream)
    ""
  (flet ((test (cx cy cw ch w h)
           (let ((path (uiop/pathname:merge-pathnames* *testing-image-rgb-file* *testing-image-directory*)))
             (let ((image (render-image-test-read-rgba-image path)))
               (cldk-render:draw-image* stream
                                        (cldk-render:crop-image image cx cy cw ch)
                                        w h)))))
    (test 100 100 100 150
          10 10)
    (test 150 150 150 100
          200 10)))

(defun render-image-test-fill-color (stream design cx cy cw ch w h mode)
  (let ((path (uiop/pathname:merge-pathnames* *testing-image-rgb-file* *testing-image-directory*)))
    (let ((image (cldk-render:coerce-image (render-image-test-read-rgb-image path) mode))
          (pd (cldk-render:make-pixeled-design design)))
      (cldk-render:fill-image image pd 
                              nil :x cx :y cy :width cw :height ch)
      (setf (cldk-render-internals::pixeled-design-region pd)
            (clim:make-rectangle* (+ cx cw 10) (+ cy 10) (+ cx cw cw -10) (+ cy ch -10)))
      (cldk-render:fill-image image pd 
                              nil :x (+ cx cw) :y cy  :width cw :height ch)
      (cldk-render:draw-image* stream
                               image
                               w h))))

(define-render-image-test "13) fill color (rgb)" (stream)
    ""
  (flet ((test (design cx cy cw ch w h)
           (render-image-test-fill-color stream design cx cy cw ch w h :rgb)))
    (test +red+
          50 50 100 150
          10 10)
    (test (compose-in +green+ (make-opacity 0.5))
          150 150 150 100
          10 300)))

(define-render-image-test "13) fill color (rgba)" (stream)
    ""
  (flet ((test (design cx cy cw ch w h)
           (render-image-test-fill-color stream design cx cy cw ch w h :rgba)))
    (test +red+
          100 100 100 150
          10 10)
    (test (compose-in +green+ (make-opacity 0.5))
          150 150 150 100
          10 300)))

(define-render-image-test "13) fill color (gray)" (stream)
    ""
  (flet ((test (design cx cy cw ch w h)
           (render-image-test-fill-color stream design cx cy cw ch w h :gray)))
    (test +red+
          100 100 100 150
          10 10)
    (test (compose-in +green+ (make-opacity 0.5))
          150 150 150 100
          10 300)))

(defun render-image-test-fill-stencil (stream design cx cy cw ch w h mode)
  (let ((path (uiop/pathname:merge-pathnames* *testing-image-rgb-file* *testing-image-directory*))
        (path2 (uiop/pathname:merge-pathnames* *testing-image-bn2-file* *testing-image-directory*)))
    (let ((image (cldk-render:coerce-image (render-image-test-read-rgb-image path) mode))
          (stencil (render-image-test-read-gray-image path2))
          (pd (cldk-render:make-pixeled-design design)))
      (cldk-render:fill-image image pd 
                              stencil :x cx :y cy :width cw :height ch
                              :stencil-dx (- cx) :stencil-dy (- cy))
      (setf (cldk-render-internals::pixeled-design-region pd)
            (clim:make-rectangle* (+ cx cw 10) (+ cy 10) (+ cx cw cw -10) (+ cy ch -10)))
      (cldk-render:fill-image image pd 
                              stencil :x (+ cx cw) :y cy :width cw :height ch
                              :stencil-dx (- (+ cx cw)) :stencil-dy (- cy))
      (cldk-render:draw-image* stream
                               image
                               w h))))

(define-render-image-test "14) fill stencil (rgb)" (stream)
    ""
  (flet ((test (design cx cy cw ch w h)
          (render-image-test-fill-stencil stream design cx cy cw ch w h :rgb)))
    (test +red+
          100 100 100 150
          10 10)
    (test (compose-in +green+ (make-opacity 0.7))
          150 150 150 100
          10 300)))

(define-render-image-test "14) fill stencil (rgba)" (stream)
    ""
  (flet ((test (design cx cy cw ch w h)
           (render-image-test-fill-stencil stream design cx cy cw ch w h :rgba)))
    (test +red+
          100 100 100 150
          10 10)
    (test (compose-in +green+ (make-opacity 0.5))
          150 150 150 100
          10 300)))

(define-render-image-test "14) fill stencil (gray)" (stream)
    ""
  (flet ((test (design cx cy cw ch w h)
           (render-image-test-fill-stencil stream design cx cy cw ch w h :gray)))
    (test +red+
          100 100 100 150
          10 10)
    (test (compose-in +green+ (make-opacity 0.5))
          150 150 150 100
          10 300)))

(define-render-image-test "15) output record :draw nil" (stream)
    ""
  (flet ((test (h)
           (let ((path (uiop/pathname:merge-pathnames* *testing-image-rgb-file* *testing-image-directory*)))
             (let ((image (render-image-test-read-rgb-image path)))
               (draw-design stream (cldk-render:make-image-design image) :x 10 :y h)))))
    (clim:with-output-to-output-record (stream)
      (with-output-recording-options (stream :record t :draw nil)
        ;;(test 10)
        (draw-rectangle* stream 10 10 50 50 :ink +green+ :filled t)))))

(define-render-image-test "16) output record moving" (stream)
    ""
  (flet ((test (h)
           (let ((path (uiop/pathname:merge-pathnames* *testing-image-rgb-file* *testing-image-directory*)))
             (let ((image (render-image-test-read-rgb-image path)))
               (draw-design stream (cldk-render:make-image-design image) :x 10 :y h)))))
    (let ((record
           (clim:with-output-to-output-record (stream)
             (with-output-recording-options (stream :record t :draw t)
               (test 10)
               (draw-rectangle* stream 10 10 50 50 :ink +green+ :filled t)))))
      (setf (clim:output-record-position record) (values 10 310))
      (replay record stream))))

(define-render-image-test "17) coerce" (stream)
    ""
  (let* ((path (uiop/pathname:merge-pathnames* *testing-image-rgb-file* *testing-image-directory*))
         (image (cldk-render:crop-image
                 (render-image-test-read-rgba-image path)
                 100 100 100 150)))
    (flet ((test (medium type w h)
             (cldk-render:draw-image* stream
                                      (cldk-render:coerce-image image type medium)
                                      w h)))
      (test (sheet-medium stream) :rgba 10 10)
      (test (sheet-medium stream) :rgb 160 10)
      (test (sheet-medium stream) :gray 310 10)
      (test :opticl :rgba 10 210)
      (test :opticl :rgb 160 210)
      (test :opticl :gray 310 210)
      (test :two-dim-array :rgba 10 410)
      (test :two-dim-array :rgb 160 410)
      (test :two-dim-array :gray 310 410))))

(define-render-image-test "18) clone" (stream)
    ""
  (let* ((path (uiop/pathname:merge-pathnames* *testing-image-rgb-file* *testing-image-directory*))
         (image (cldk-render:crop-image
                 (render-image-test-read-rgba-image path)
                 100 100 100 150)))
    (flet ((test (medium type w h)
             (cldk-render:draw-image* stream
                                      (cldk-render:clone-image image type medium)
                                      w h)))
      (test (sheet-medium stream) :rgba 10 10)
      (test (sheet-medium stream) :rgb 160 10)
      (test (sheet-medium stream) :gray 310 10)
      (test :opticl :rgba 10 210)
      (test :opticl :rgb 160 210)
      (test :opticl :gray 310 210)
      (test :two-dim-array :rgba 10 410)
      (test :two-dim-array :rgb 160 410)
      (test :two-dim-array :gray 310 410))))

(defun render-image-test-set-color (stream design cx cy cw ch w h mode)
  (let ((path (uiop/pathname:merge-pathnames* *testing-image-rgb-file* *testing-image-directory*)))
    (let ((image (cldk-render:coerce-image (render-image-test-read-rgb-image path) mode))
          (pd (cldk-render:make-pixeled-design design)))
      (cldk-render:set-image image pd 
                             :x cx :y cy :width cw :height ch)
      (setf (cldk-render-internals::pixeled-design-region pd)
            (clim:make-rectangle* (+ cx cw 10) (+ cy 10) (+ cx cw cw -10) (+ cy ch -10)))
      (cldk-render:set-image image pd 
                              :x (+ cx cw) :y cy  :width cw :height ch)
      (cldk-render:draw-image* stream
                               image
                               w h))))

(define-render-image-test "19) set color (rgb)" (stream)
    ""
  (flet ((test (design cx cy cw ch w h)
           (render-image-test-set-color stream design cx cy cw ch w h :rgb)))
    (test +red+
          50 50 100 150
          10 10)
    (test (compose-in +green+ (make-opacity 0.5))
          150 150 150 100
          10 300)))

(define-render-image-test "19) set color (rgba)" (stream)
    ""
  (flet ((test (design cx cy cw ch w h)
           (render-image-test-set-color stream design cx cy cw ch w h :rgba)))
    (test +red+
          100 100 100 150
          10 10)
    (test (compose-in +green+ (make-opacity 0.5))
          150 150 150 100
          10 300)))

(define-render-image-test "19) set color (gray)" (stream)
    ""
  (flet ((test (design cx cy cw ch w h)
           (render-image-test-set-color stream design cx cy cw ch w h :gray)))
    (test +red+
          100 100 100 150
          10 10)
    (test (compose-in +green+ (make-opacity 0.5))
          150 150 150 100
          10 300)))


