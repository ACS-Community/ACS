;;; w3-elisp.el --- Scripting support for emacs-lisp
;; Author: wmperry
;; Created: 1998/12/01 22:12:10
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

(mapcar
 (function
  (lambda (x)
    (put x 'w3-safe t)))
 '(;; Any safe functions for untrusted scripts should go here.
   ;; Basic stuff
   message
   format garbage-collect progn prog1 prog2 progn-with-message
   while current-time current-time-string
   plist-member plist-to-alist plist-get
   assoc memq member function lambda point

   ;; Device querying
   device-pixel-height device-type device-color-cells
   device-mm-height device-class device-bitplanes
   device-on-window-system-p device-pixel-width
   device-mm-width device-baud-rate

   ;; Frame querying
   frame-type frame-name frame-device frame-parameters
   frame-height frame-pixel-width frame-pixel-height
   frame-width frame-property

   ;; Window querying
   window-frame window-height window-width
   window-pixel-width window-pixel-height

   ;; Buffer querying
   buffer-name buffer-substring buffer-substring-no-properties
   buffer-size buffer-string
   
   ;; Text properties, read-only
   get-text-property text-properties-at text-property-bounds
   text-property-not-all

   ;; URL loading stuff
   url-insert-file-contents url-view-url

   ;; Interfacing to W3
   w3-fetch w3-refresh-buffer w3-view-this-url

   ;; All the XEmacs event manipulation functions
   event-live-p event-glyph-extent event-glyph-y-pixel event-x-pixel
   event-type event-glyph event-button event-over-text-area-p
   event-glyph-x-pixel event-buffer event-device event-properties
   event-process event-timestamp event-modifier-bits event-console
   event-window-y-pixel event-window event-window-x-pixel event-point
   event-function event-over-toolbar-p event-matches-key-specifier-p
   event-over-glyph-p event-frame event-x event-channel event-y
   event-screen event-to-character event-over-border-p
   event-toolbar-button event-closest-point event-object event-key
   event-modifiers event-y-pixel event-over-modeline-p
   event-modeline-position
   )
 )

(defsubst w3-elisp-safe-function (func args)
  (let ((validator (get func 'w3-safe)))
    (cond
     ((eq t validator) t)		; Explicit allow
     ((eq nil validator) nil)		; Explicit deny
     ((fboundp validator)		; Function to call
      (funcall validator func args))
     ((boundp validator)		; Variable to check
      (symbol-value validator))
     (t nil))))				; Fallback to unsafe

(defun w3-elisp-safe-expression (exp)
  "Return t if-and-only-if EXP is safe to evaluate."
  (cond
   ((and (listp exp) (not (listp (cdr exp)))) ; A cons cell
    t)
   ((or					; self-quoters
     (vectorp exp)
     (numberp exp)
     (symbolp exp)
     (stringp exp)
     (keymapp exp))
    t)
   ((listp exp)				; Function call - check arguments
    (if (w3-elisp-safe-function (car exp) (cdr exp))
	(let ((args (cdr exp))
	      (rval t))
	  (while args
	    (if (not (w3-elisp-safe-expression (pop args)))
		(setq args nil
		      rval nil)))
	  rval)))
   ;; How to handle the insane # of native types?
   (t nil)))

(defun w3-elisp-safe-eval (form)
  (if (w3-elisp-safe-expression form)
      (condition-case ()
	  (eval form)
	(error nil))))

(provide 'w3-elisp)
