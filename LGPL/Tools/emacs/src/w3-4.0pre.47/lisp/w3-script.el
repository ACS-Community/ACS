;;; w3-script.el --- Scripting support
;; Author: wmperry
;; Created: 1998/12/01 22:12:11
;; Version: 1.1.1.1
;; Keywords: hypermedia, scripting

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1997, 1998 Free Software Foundation, Inc.
;;;
;;; This file is part of GNU Emacs.
;;;
;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA 02111-1307, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl)
(require 'w3-elisp)
(require 'w3-jscript)

;; Event Handlers
;; onclick              ; It was clicked on
;; onchange		; Text area was changed
;; onselect		; Menu choice changed
;; onmouseover		; Mouse is over us
;; onmouseout		; Mouse left us
;; onblur		; We lost focus
;; onfocus		; We gained focus
;; onload		; We got loaded
;; onunload		; We got unloaded
;; onreset		; Form got reset
;; onsubmit		; From is about to be submitted
;; onabort		; User cancelled loading an image
;; onerror		; Error occurred loading an image

(defgroup w3-scripting nil
  "When, where, how, and why to enable client-side scripting."
  :group 'w3)

(defcustom w3-do-scripting nil
  "*Whether to handle client-side scripting or not.
If you are adventurous, set this to `t'"
  :group 'w3-scripting
  :type 'boolean)

(defvar w3-current-scripting-language 'elisp)
(make-variable-buffer-local 'w3-current-scripting-language)

(put 'form 'w3-event-handlers
     '(onclick onchange onselect onblur onfocus onreset onsubmit))

(put 'mouse 'w3-event-handlers '(onmouseover onmouseout))

(put 'misc 'w3-event-handlers '(onload onunload))

(put 'all 'w3-event-handlers (append (get 'form 'w3-event-handlers)
				     (get 'mouse 'w3-event-handlers)))

(defun w3-script-find-event-handlers (pt type)
  (if w3-do-scripting
      (let* ((html-stack (get-text-property pt 'html-stack))
	     (args nil)
	     (rval nil)
	     (cur nil))
	(while html-stack
	  (setq args (cdr (pop html-stack)))
	  (while (setq cur (pop args))
	    (if (memq (car cur) (get type 'w3-event-handlers))
		(setq rval (cons cur rval)))))
	(nreverse rval))))

(defun w3-script-evaluate-form (f)
  (if w3-do-scripting
      (case w3-current-scripting-language
	(elisp
	 (let ((st 0)
	       (form nil)
	       (max (length f)))
	   (condition-case ()
	       (while (and (< st max) (setq form (read-from-string f st)))
		 (setq st (cdr form)
		       form (car form))
		 (w3-elisp-safe-eval form))
	     (error nil))))
	(otherwise
	 (message "Unimplemented scripting language: %S"
		  w3-current-scripting-language)))))

(provide 'w3-script)
