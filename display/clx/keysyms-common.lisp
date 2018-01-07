;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLIM-XCOMMON; -*-
;;; --------------------------------------------------------------------------------------
;;;     Title: X11 keysym handling
;;;   Created: 2002-02-11
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;   License: LGPL (See file COPYING for details).
;;; --------------------------------------------------------------------------------------
;;;  (c) copyright 2002 by Gilbert Baumann

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA  02111-1307  USA.

;;; Support and port mixin for X based backends, handling keycode to
;;; keysym and character mapping, and handling of modifiers.

(in-package :cldk-clx)

;;; This hash table maps a keysym to a list of keysym names for that
;;; keysym.
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
