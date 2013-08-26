;;; w3-print.el --- Printing support for emacs-w3
;; Author: wmperry
;; Created: 1998/12/18 02:19:49
;; Version: 1.1.1.2
;; Keywords: faces, help, printing, hypermedia

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1993 - 1996 by William M. Perry <wmperry@cs.indiana.edu>
;;; Copyright (c) 1996 - 1999 Free Software Foundation, Inc.
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
(defvar w3-postscript-print-function 'ps-print-buffer-with-faces
  "*Name of the function to use to print a buffer as PostScript.
This should take no arguments, and act on the current buffer.
Possible values include:
ps-print-buffer-with-faces   - print immediately
ps-spool-buffer-with-faces   - spool for later")

(require 'mule-sysdp)

;;;###autoload
(defun w3-print-this-url (&optional url format)
  "Print out the current document (in LaTeX format)"
  (interactive)
  (if (not url) (setq url (url-view-url t)))
  (let* ((completion-ignore-case t)
	 (format (or format
		     (completing-read
		      "Format: "
		      '(("HTML Source")		; The raw HTML code
			("Formatted Text") 	; Plain ASCII rendition
			("PostScript")		; Pretty PostScript
			("LaTeX'd")		; LaTeX it, then print
			)
		      nil t))))
    (save-excursion
      (cond
       ((equal "HTML Source" format)
	(if w3-current-source
	    (let ((x w3-current-source))
	      (set-buffer (get-buffer-create url-working-buffer))
	      (erase-buffer)
	      (insert x))
	  (url-retrieve url))
	(lpr-buffer))
       ((or (equal "Formatted Text" format)
	    (equal "" format))
	(lpr-buffer))
       ((equal "PostScript" format)
	(funcall w3-postscript-print-function))
       ((equal "LaTeX'd" format)
 	(w3-parse-tree-to-latex w3-current-parse url)
	(save-window-excursion
	  (mule-write-region-no-coding-system
	   (point-min) (point-max)
	   (expand-file-name "w3-tmp.latex"
			     w3-temporary-directory) nil 5)
	  (shell-command
	   (format
	    "cd %s ; latex w3-tmp.latex ; latex w3-tmp.latex ; %s w3-tmp.dvi ; rm -f w3-tmp*"
	    w3-temporary-directory
	    w3-print-command))
	  (kill-buffer "*Shell Command Output*")))))))

;;;###autoload
(defun w3-print-url-under-point ()
  "Print out the url under point (in LaTeX format)"
  (interactive)
  (w3-print-this-url (w3-view-this-url t)))

(provide 'w3-print)
