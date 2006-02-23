;;; url-cid.el --- Content-ID URL loader
;; Author: wmperry
;; Created: 1999/08/05 20:21:27
;; Version: 1.3
;; Keywords: comm, data, processes

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1998 - 1999 Free Software Foundation, Inc.
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

(require 'url-vars)
(require 'url-parse)
(require 'widget)

(condition-case ()
    (require 'mm-decode)
  (error nil))

(defun url-cid-gnus (cid)
  (set-buffer (get-buffer-create url-working-buffer))
  (let ((content-type nil)
 	(encoding nil)
 	(part nil)
 	(data nil))
    (setq part (mm-get-content-id cid))
    (if (not part)
	(message "Unknown CID encountered: %s" cid)
      (setq data (save-excursion
		   (set-buffer (mm-handle-buffer part))
		   (buffer-string))
	    content-type (mm-handle-type part)
	    encoding (symbol-name (mm-handle-encoding part)))
      (if (= 0 (length content-type)) (setq content-type "text/plain"))
      (if (= 0 (length encoding)) (setq encoding "8bit"))
      (if (listp content-type)
	  (setq content-type (car content-type)))
      (setq url-current-content-length (length data)
	    url-current-mime-type content-type
	    url-current-mime-encoding encoding
	    url-current-mime-headers (list (cons "content-type" content-type)
					   (cons "content-encoding" encoding)))
      (and data (insert data)))))

(defun url-cid (url)
  (if (not (string-match "^cid:\\(.*\\)" url))
      (message "Malformed CID URL: %s" url)
    (setq url (url-unhex-string (match-string 1 url)))
    (cond
     ((fboundp 'mm-get-content-id)
      ;; Using Pterodactyl Gnus or later
      (url-cid-gnus url))
     (t
      (message "Unable to handle CID URL: %s" url)))))
