
(in-package :cldk-sdl2)


(defvar *keysym-name-table*
  (make-hash-table :test #'eql))

;;; This hash table maps a keysym name to the corresponding keysym.
(defvar *keysym-table*
  (make-hash-table :test #'eq))

(defun define-keysym (name value)
  (pushnew name (gethash value *keysym-name-table* nil))
  (setf (gethash name *keysym-table*) value))

(defun keysym-to-keysym-name (value)
  (car (last (gethash value *keysym-name-table*))))

(defun keysym-name-to-keysym (value)
  (gethash value *keysym-table*))

