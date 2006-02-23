;;; w3-style.el --- Emacs/W3 binding style sheet mechanism
;; Author: wmperry
;; Created: 1998/12/18 02:19:51
;; Version: 1.1.1.2
;; Keywords: faces, hypermedia

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A style sheet mechanism for emacs-w3
;;;
;;; This will eventually be able to under DSSSL[-lite] as well as the
;;; experimental W3C mechanism
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'font)
(require 'w3-keyword)
(require 'cl)
(require 'css)



;;;###autoload
(defun w3-handle-style (&optional plist)
  (let ((url (or (plist-get plist 'href)
		 (plist-get plist 'src)
		 (plist-get plist 'uri)))
	(media (intern (downcase (or (plist-get plist 'media) "all"))))
	(type (downcase (or (plist-get plist 'notation) "text/css")))
	(url-working-buffer " *style*")
	(stylesheet nil)
	(defines nil)
	(cur-sheet w3-current-stylesheet)
	(string (plist-get plist 'data)))
    (if (not (memq media (css-active-device-types)))
	nil				; Not applicable to us!
      (save-excursion
	(set-buffer (get-buffer-create url-working-buffer))
	(erase-buffer)
	(setq url-be-asynchronous nil)
	(cond
	 ((member type '("experimental" "arena" "w3c-style" "css" "text/css"))
	  (setq stylesheet (css-parse url string cur-sheet)))
	 (t
	  (w3-warn 'html "Unknown stylesheet notation: %s" type))))
      (setq w3-current-stylesheet stylesheet))))

;;;###autoload
(defun w3-display-stylesheet (&optional sheet)
  "Display the stylesheet for the current document."
  (interactive)
  (setq sheet (or sheet w3-current-stylesheet w3-user-stylesheet))
  (if (not sheet)
      (error "No stylesheet available!"))
  (css-display sheet))

(provide 'w3-style)
