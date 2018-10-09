(in-package :cldk-internals)

(defparameter *epsilon* 0)

(defclass rectangle-set () 
  ((regions :initarg :regions :reader rectangle-set-regions)
   (bands :initarg :bands
          :reader  rectangle-set-bands)))

(defun rectangle->rectangle-set (x1 y1 x2 y2)
  (make-instance 'rectangle-set
                 :bands (rectangle->xy-bands* x1 y1 x2 y2)))

(defun map-over-bands (fun bands)
  (do ((q bands (cdr q)))
      ((null (cdr q)))
    (funcall fun (caar q) (caadr q) (cdar q))))

(defun map-over-bands-rectangles (fun bands)
  (map-over-bands (lambda (y1 y2 isum)
                    (do ((p isum (cddr p)))
                        ((null p))
                      (funcall fun (car p) y1 (cadr p) y2)))
                  bands))

(defun rectangle->xy-bands* (x1 y1 x2 y2)
  (list (list y1 x1 x2)
        (cons y2 nil)))

(defun rectangle->yx-bands* (x1 y1 x2 y2)
  (list (list x1 y1 y2)
        (cons x2 nil)))

(defun xy-bands->yx-bands (bands)
  ;; Das kann man sicherlich noch viel geschicker machen ...
  (let ((res nil))
    (map-over-bands-rectangles
     (lambda (x1 y1 x2 y2)
       (setf res (bands-union res (rectangle->yx-bands* x1 y1 x2 y2))))
     bands)
    res))

(defun map-over-rectangle-set-regions
    (fun self &key normalize)
  (when self
    (with-slots (bands) self
      (cond ((or (null normalize) (eql normalize :x-banding))
             (map-over-bands-rectangles
              (lambda (x1 y1 x2 y2)
                (funcall fun x1 y1 x2 y2))
              bands))
            ((eql normalize :y-banding)
             (map-over-bands-rectangles
              (lambda (y1 x1 y2 x2)
                (funcall fun x1 y1 x2 y2))
              (xy-bands->yx-bands bands)))
            (t 
             (error "Bad ~S argument to ~S: ~S"
                    :normalize 'map-over-rectangle-set-regions normalize))))))

(defun make-rectangle-set (bands)
  (make-instance 'rectangle-set :bands bands))

(defun rectangle-set-union (xs ys)
  (if (or (null xs) (null ys))
      (or xs ys)
      (make-rectangle-set
       (bands-union (rectangle-set-bands xs)
                    (rectangle-set-bands ys)))))

(defun bands-op (as bs isum-op z0 a b)
  (let ((z1)
        (rest)
        (isum))
    (cond ((and (null as) (null bs))
           (if z0
               (list (cons z0 nil))
             nil))
          (t
           (progn
             (setq z1 (cond ((null as) (caar bs))
                            ((null bs) (caar as))
                            (t (min (caar as) (caar bs)))))
             (setq rest (bands-op (if (and as (= (- z1 (caar as)) 0)) (cdr as) as)
                                  (if (and bs (= (- z1 (caar bs)) 0)) (cdr bs) bs)
                                  isum-op
                                  z1
                                  (if (and as (= (- z1 (caar as)) 0)) (cdar as) a)
                                  (if (and bs (= (- z1 (caar bs)) 0)) (cdar bs) b))))
           (setq isum (funcall isum-op a b))
           
             (if z0  
                 (if (and rest (equal isum (cdar rest)))
                     (cons (cons z0 isum)
                           (cdr rest))
                   (cons (cons z0 isum)
                         rest))
               rest))) ))

(defun canon-empty-bands (x)
  (cond ((null (cdr x)) nil)
        (t x)))

(defun bands-union (as bs)
  (canon-empty-bands (bands-op as bs #'isum-union* nil nil nil)))

(defun isum-union* (xs ys)
  (let ((res (isum-op xs ys boole-ior 0 0 nil)))
    res))

(defun isum-op (as bs boole-op in-a in-b x0)
  (let (x)
    (cond ((and (null as) (null bs))
           nil)
          (t
           (cond ((null bs)
                  (setq in-a (- 1 in-a))
                  (setq x (pop as)))

                 ((null as)
                  (setq in-b (- 1 in-b))
                  (setq x (pop bs)))

                 ((< (- (first as) (first bs)) (- *epsilon*))
                  (setq in-a (- 1 in-a))
                  (setq x (pop as)))
                 
                 ((< (- (first bs) (first as)) (- *epsilon*))
                  (setq in-b (- 1 in-b))
                  (setq x (pop bs)))

                 (t
                  (setq in-a (- 1 in-a)
                        in-b (- 1 in-b))
                  (if x0
                      (setq x (max (pop bs)
                                   (pop as)))
                      (setq x (min (pop bs)
                                   (pop as))))))
           (cond ((zerop (boole boole-op in-a in-b))
                  (if x0 
                      (list* x0 x (isum-op as bs boole-op in-a in-b nil))
                    (isum-op as bs boole-op in-a in-b x0)))
                 (t
                  (if (null x0)
                      (isum-op as bs boole-op in-a in-b x)
                    (isum-op as bs boole-op in-a in-b x0))))))))
;;;;;
;;;;;
;;;;;

#|
(defparameter *rects* (list 
                       '(0 0 20 30) 
                       '(30 0 50 40)
                       ;;'(50 0 80 20)))
                       ))
(defparameter rs nil)
(dolist (rect *rects*)
  (setf rs (rectangle-set-union rs (rectangle->rectangle-set (elt rect 0) (elt rect 1) (elt rect 2) (elt rect 3)))))
(map-over-rectangle-set-regions #'(lambda (x1 y1 x2 y2) (format t "~A" (list x1 y1 x2 y2))) rs)

(0 0 20 20)(30 0 80 20)(0 20 20 30)(30 20 50 30)(30 30 50 40)

(0 0 20 20)(30 0 50 20)(50 0 80 20)(0 20 20 30)(30 20 50 30)(30 30 50 40)
|#

