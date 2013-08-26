;;; url-nfs.el --- NFS URL interface
;; Author: wmperry
;; Created: 1998/12/18 02:19:45
;; Version: 1.1.1.2
;; Keywords: comm, data, processes

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1996 by William M. Perry <wmperry@cs.indiana.edu>
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

(require 'url-vars)
(require 'url-parse)
(require 'cl)

(defvar url-nfs-automounter-directory-spec
  "file:/net/%h%f"
  "*How to invoke the NFS automounter.  Certain % sequences are recognized.

%h -- the hostname of the NFS server
%n -- the port # of the NFS server
%u -- the username to use to authenticate
%p -- the password to use to authenticate
%f -- the filename on the remote server
%% -- a literal %

Each can be used any number of times.")

(defun url-nfs-unescape (format host port user pass file)
  (save-excursion
    (set-buffer (get-buffer-create " *nfs-parse*"))
    (erase-buffer)
    (insert format)
    (goto-char (point-min))
    (while (re-search-forward "%\\(.\\)" nil t)
       (let ((escape (aref (match-string 1) 0)))
	 (replace-match "" t t)
	 (case escape
	   (?% (insert "%"))
	   (?h (insert host))
	   (?n (insert (or port "")))
	   (?u (insert (or user "")))
	   (?p (insert (or pass "")))
	   (?f (insert (or file "/"))))))
    (buffer-string)))

(defun url-nfs (url)
  (let* ((urlobj (url-generic-parse-url url))
	 (host (url-host urlobj))
	 (port (string-to-int (url-port urlobj)))
	 (pass (url-password urlobj))
	 (user (url-user urlobj))
	 (file (url-filename urlobj)))
    (url-retrieve (url-nfs-unescape url-nfs-automounter-directory-spec
				    host port user pass file))))
    
