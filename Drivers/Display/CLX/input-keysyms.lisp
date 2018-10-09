;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLIM-CLX; -*-
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

(in-package :cldk-display-clx)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +shift-bit+      #b00000001)
  (defconstant +lock-bit+       #b00000010)
  (defconstant +control-bit+    #b00000100)
  (defparameter *meta-bit*      #b00001000)
  (defparameter *hyper-bit*     #b00100000)
  (defparameter *super-bit*     #b01000000)
  (defparameter *alt-bit*       #b10000000))

(defparameter *clx-modifier->keyname*
  (list (cons +shift-key+ nil)
        (cons +control-key+ nil)
        (cons +meta-key+ nil)
        (cons +super-key+ nil)
        (cons +hyper-key+ nil)
        (cons +alt-key+ nil)))

(defparameter *modifier->keyname* nil)
 
(defun load-mapping (display)
  (let ((mm (xlib::get-display-modifier-mapping display)))
    (setf *clx-modifier->keyname* nil)
    (dolist (l mm)
      (let ((k (car l))
            (v (cdr l)))
        (let ((keys (assoc v *clx-modifier->keyname*)))
          (unless (eql k 0)
            (if keys
                (rplacd keys (cons (keysym-to-keysym-name k) (cdr keys)))
                (setf *clx-modifier->keyname* (cons (list v (keysym-to-keysym-name k))
                                                    *clx-modifier->keyname*)))))))
    (setf *modifier->keyname*
          (list (cons +shift-key+ (cdr (assoc +shift-bit+ *clx-modifier->keyname*)))
                (cons +control-key+ (cdr (assoc +control-bit+ *clx-modifier->keyname*))) 
                (cons +meta-key+ (cdr (assoc *meta-bit* *clx-modifier->keyname*)))
                (cons +super-key+ (cdr (assoc *super-bit* *clx-modifier->keyname*)))
                (cons +hyper-key+ (cdr (assoc *hyper-bit* *clx-modifier->keyname*)))))))

(defun decode-x-modifier-state (state)
  (let ((mapping #.(vector +shift-key+
                           0
                           +control-key+
                           +meta-key+
                           0
                           +hyper-key+
                           +super-key+))
        (res 0)
        (caps-lock? (logbitp 1 state))
        (mode-switch? (logbitp 7 state)))
    (dotimes (i (length mapping))
      (when (logbitp i state)
        (setf res (logior res (aref mapping i)))))
    (values res caps-lock? mode-switch?)))

(defun decode-x-modifiers-state (state)
  (let ((modifiers
         (logior (if (plusp (logand state +shift-bit+)) +shift-key+ 0)
                 (if (plusp (logand state +control-bit+)) +control-key+ 0)
                 (if (plusp (logand state *meta-bit*)) +meta-key+ 0)
                 (if (plusp (logand state *hyper-bit*)) +hyper-key+ 0)
                 (if (plusp (logand state *super-bit*)) +super-key+ 0))))
    (values modifiers
            (plusp (logand state +lock-bit+))
            (plusp (logand state *alt-bit*)))))

(defun update-modifiers (modifiers event-key keysym-name)
  (let ((keysym-modifier (loop for modifier-keysyms in *modifier->keyname*
                            if (member keysym-name (cdr modifier-keysyms))
                            return (car modifier-keysyms))))
    (cond ((and keysym-modifier (eq event-key :key-press))
	   (logior modifiers keysym-modifier))
	  ((and keysym-modifier (eq event-key :key-release))
	   (logandc2 modifiers keysym-modifier))
	  (t modifiers))))

(defun decode-x-key (event-key keycode state)
  (multiple-value-bind (clim-modifiers caps-lock? mode-switch?)
      (decode-x-modifier-state state)
    (let* ((display (clx-driver-display *clx-kernel*))
           (shift? (logtest +shift-key+ clim-modifiers))
           (shifted-keysym (xlib:keycode->keysym display keycode
                                                 (+ 1 (if mode-switch?
                                                          2 0))))
           (unshifted-keysym (xlib:keycode->keysym display keycode
                                                   (if mode-switch?
                                                       2 0)))
           (keysym-char (xlib:keysym->character display unshifted-keysym
                                                (if mode-switch? 2 0)))
           (alpha-char? (and (characterp keysym-char)
                             (alpha-char-p keysym-char)))
           (keysym
            (if shift?
                ;; Shift + caps lock cancel themselves for alphabetic chars
                (if (and caps-lock? alpha-char?)
                    unshifted-keysym
                    shifted-keysym)
                (if (and caps-lock? alpha-char?)
                    shifted-keysym
                    unshifted-keysym))))
      (let* ((keysym-name (keysym-to-keysym-name keysym))
             (char (xlib:keysym->character display keysym
                                           (+ (if shift?
                                                  1 0)
                                              (if mode-switch?
                                                  2 0))))
             (modifiers (if (and char (characterp char))
                            clim-modifiers
                            (update-modifiers
                             clim-modifiers
                             event-key
                             keysym-name)))
                             
             ;; Cache might be updated at this step.
             #+nil (modifiers (x-keysym-to-modifiers
                         driver
                         event-key
                         char
                         (keysym-to-keysym-name keysym)
                         state)))
        #+nil (log:info modifiers)
        (values char
                ;; We filter away the shift state if there is a
                ;; difference between the shifted and unshifted
                ;; keysym. This is so eg. #\A will not look like "#\A
                ;; with a Shift modifier", as this makes gesture
                ;; processing more difficult.
                (if (= shifted-keysym unshifted-keysym)
                    modifiers
                    (logandc2 modifiers +shift-key+))
                keysym-name)))))
