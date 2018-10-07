
(in-package :cldk-driver-sdl2)


(defvar *keysym-name-table*
  (make-hash-table :test #'eql))

;;; This hash table maps a keysym name to the corresponding keysym.
(defvar *keysym-table*
  (make-hash-table :test #'eq))

(defun define-keysym (name value &optional (alpha-p nil))
  (pushnew (list name alpha-p) (gethash value *keysym-name-table* nil))
  (setf (gethash name *keysym-table*) value))

(defun keysym-to-keysym-name (value)
  (values-list (car (last (gethash value *keysym-name-table*)))))

(defun keysym-name-to-keysym (value)
  (gethash value *keysym-table*))

