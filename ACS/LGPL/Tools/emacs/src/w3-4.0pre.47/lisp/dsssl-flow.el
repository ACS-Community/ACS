;;; dsssl-flow.el --- DSSSL flow objects
;; Author: wmperry
;; Created: 1998/12/18 02:19:24
;; Version: 1.1.1.2
;; Keywords: 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1996, 1997 by William M. Perry <wmperry@cs.indiana.edu>
;;; Copyright (c) 1997 - 1999 by Free Software Foundation, Inc.
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

(defstruct flow-object
  (type 'unknown :read-only t)		; Name of this flow object
  (properties nil)
  (children nil)
  (parent nil)
  )

(defstruct dsssl-flow-processor
  (name 'unknown :read-only t)		; Name of this processing backend
  (init nil)				; initialize the backend
  (handler nil)				; handle a single flow object
  (sizeof nil)				; get size of a single flow object
  (clean nil)				; cleanup instance of backend
  )

(defvar dsssl-flow-active-faces nil)
(defvar dsssl-flow-active-voices nil)
(make-variable-buffer-local 'dsssl-flow-active-faces)
(make-variable-buffer-local 'dsssl-flow-active-voices)

(defun dsssl-flow-display (flows processor)
  (let ((handler (dsssl-flow-processor-handler processor))
	(flow-stack (list flows))
	(content nil)
	(node nil)
	(last-object nil)
	)
    (while flow-stack
      (setq content (pop flow-stack))
      (dsssl-flow-progress-meter)
      ;; Handle the element's content
      (while content
	(dsssl-flow-progress-meter)
	(if (stringp (car content))
	    (dsssl-flow-handle-string-content (pop content))
	  (setq node (pop content))
	  ;; todo: collect all information about this flow object for faster
	  ;; lookup later.
	  (push (dsssl-flow-face-for-element node) dsssl-flow-active-faces)
	  (push (dsssl-flow-voice-for-element node) dsssl-flow-active-voices))
	  (case (flow-object-type node)
	    ;; Core DSSL components  basic flow object classes
	    (sequence			; 12.6.1
	     )
	    (display-group		; 12.6.2
	     )
	    (paragraph			; 12.6.6
	     )
	    (paragraph-break		; 12.6.7
	     )
	    (external-graphic		; 12.6.15
	     )
	    ;; DSSSL options required in DSSSL online
	    ;; Simple page flow object class
	    (simple-page-sequence	; 12.6.3
	     )
	    ;; Table flow object classes
	    (table			; 12.6.27.1
	     )
	    (table-part			; 12.6.27.2
	     )
	    (table-column		; 12.6.27.3
	     )
	    (table-row			; 12.6.27.5
	     )
	    (table-border		; 12.6.27.7
	     )
	    (table-cell			; 12.6.27.6
	     ;; Do we need to handle table-cell at this level, or is that
	     ;; something that the display backend needs to handle, and we
	     ;; just query that in the `table-row' processor?
	     )
	    ;; Online display flow object classes
	    (vertical-scroll		; 12.6.28.1
	     )
	    (multi-mode			; 12.6.28.2
	     )
	    (marginalia			; 12.6.28.4
	     )
	    ;; Emacs/W3 specific flow objects
	    (applet			; Wow, Java
	     )
	    (script			; Scripts
	     )
	    (form-element		; Any form element
	     )
	    ;; pinhead, flame, and cookie can now all be handled by
	    ;; a stud-muffing DSSSL stylesheet - hooray!

	    ;; Generic formatting - all things that can be fully specified
	    ;; by a CSS stylesheet.
	    (otherwise
	     ;; handle the content
	     (dsssl-flow-handle-content node)))))))

(provide 'dsssl-flow)
