;;; mm.el,v --- Mailcap parsing routines, and MIME handling
;; Author: wmperry
;; Created: 1996/05/28 02:46:51
;; Version: 1.96
;; Keywords: mail, news, hypermedia

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1994, 1995, 1996 by William M. Perry <wmperry@cs.indiana.edu>
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
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generalized mailcap parsing and access routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Data structures
;;; ---------------
;;; The mailcap structure is an assoc list of assoc lists.
;;; 1st assoc list is keyed on the major content-type
;;; 2nd assoc list is keyed on the minor content-type (which can be a regexp)
;;;
;;; Which looks like:
;;; -----------------
;;; (
;;;  ("application"
;;;   ("postscript" . <info>)
;;;  )
;;;  ("text"
;;;   ("plain" . <info>)
;;;  )
;;; )
;;;
;;; Where <info> is another assoc list of the various information
;;; related to the mailcap RFC.  This is keyed on the lowercase
;;; attribute name (viewer, test, etc).  This looks like:
;;; (("viewer" . viewerinfo)
;;;  ("test"   . testinfo)
;;;  ("xxxx"   . "string")
;;; )
;;;
;;; Where viewerinfo specifies how the content-type is viewed.  Can be
;;; a string, in which case it is run through a shell, with
;;; appropriate parameters, or a symbol, in which case the symbol is
;;; funcall'd, with the buffer as an argument.
;;;
;;; testinfo is a list of strings, or nil.  If nil, it means the
;;; viewer specified is always valid.  If it is a list of strings,
;;; these are used to determine whether a viewer passes the 'test' or
;;; not.
;;;
;;; The main interface to this code is:
;;;
;;; To set everything up:
;;;
;;;  (mm-parse-mailcaps [path])
;;;
;;;  Where PATH is a unix-style path specification (: separated list
;;;  of strings).  If PATH is nil, the environment variable MAILCAPS
;;;  will be consulted.  If there is no environment variable, then a
;;;  default list of paths is used.
;;;
;;; To retrieve the information:
;;;  (mm-mime-info st [nd] [request])
;;;
;;;  Where st and nd are positions in a buffer that contain the
;;;  content-type header information of a mail/news/whatever message.
;;;  st can optionally be a string that contains the content-type
;;;  information.
;;;
;;;  Third argument REQUEST specifies what information to return.  If
;;;  it is nil or the empty string, the viewer (second field of the
;;;  mailcap entry) will be returned.  If it is a string, then the
;;;  mailcap field corresponding to that string will be returned
;;;  (print, description, whatever).  If a number, then all the
;;;  information for this specific viewer is returned.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variables, etc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-and-compile
  (require 'cl)
  (require 'devices))

(defconst mm-version (let ((x "1.96"))
		       (if (string-match "Revision: \\([^ \t\n]+\\)" x)
			   (substring x (match-beginning 1) (match-end 1))
			 x))
  "Version # of MM package")

(defvar mm-parse-args-syntax-table
  (copy-syntax-table emacs-lisp-mode-syntax-table)
  "A syntax table for parsing sgml attributes.")

(modify-syntax-entry ?' "\"" mm-parse-args-syntax-table)
(modify-syntax-entry ?` "\"" mm-parse-args-syntax-table)
(modify-syntax-entry ?{ "(" mm-parse-args-syntax-table)
(modify-syntax-entry ?} ")" mm-parse-args-syntax-table)

(defvar mm-mime-data-default
  '(
    ("multipart"   . (
		      ("alternative". (("viewer" . mm-multipart-viewer)
				       ("type"   . "multipart/alternative")))
		      ("mixed"      . (("viewer" . mm-multipart-viewer)
				       ("type"   . "multipart/mixed")))
		      (".*"         . (("viewer" . mm-save-binary-file)
				       ("type"   . "multipart/*")))
		      )
     )
    ("application" . (
		      ("x-x509-ca-cert" . (("viewer" . ssl-view-site-cert)
					   ("test" . (fboundp 'ssl-view-site-cert))
					   ("type" . "application/x-x509-ca-cert")))
		      ("x-x509-user-cert" . (("viewer" . ssl-view-user-cert)
					     ("test" . (fboundp 'ssl-view-user-cert))
					     ("type" . "application/x-x509-user-cert")))
		      ("octet-stream" . (("viewer" . mm-save-binary-file)
					 ("type" ."application/octet-stream")))
		      ("dvi"        . (("viewer" . "open %s")
				       ("type"   . "application/dvi")
				       ("test"   . (eq (device-type) 'ns))))
		      ("dvi"        . (("viewer" . "xdvi %s")
				       ("test"   . (eq (device-type) 'x))
				       ("needsx11")
				       ("type"   . "application/dvi")))
		      ("dvi"        . (("viewer" . "dvitty %s")
				       ("test"   . (not (getenv "DISPLAY")))
				       ("type"   . "application/dvi")))
		      ("emacs-lisp" . (("viewer" . mm-maybe-eval)
				       ("type"   . "application/emacs-lisp")))
;		      ("x-tar"      . (("viewer" . tar-mode)
;				       ("test"   . (fboundp 'tar-mode))
;				       ("type"   . "application/x-tar")))
		      ("x-tar"      . (("viewer" . mm-save-binary-file)
				       ("type"   . "application/x-tar")))
		      ("x-latex"    . (("viewer" . tex-mode)
				       ("test"   . (fboundp 'tex-mode))
				       ("type"   . "application/x-latex")))
		      ("x-tex"      . (("viewer" . tex-mode)
				       ("test"   . (fboundp 'tex-mode))
				       ("type"   . "application/x-tex")))
		      ("latex"      . (("viewer" . tex-mode)
				       ("test"   . (fboundp 'tex-mode))
				       ("type"   . "application/latex")))
		      ("tex"        . (("viewer" . tex-mode)
				       ("test"   . (fboundp 'tex-mode))
				       ("type"   . "application/tex")))
		      ("texinfo"    . (("viewer" . texinfo-mode)
				       ("test"   . (fboundp 'texinfo-mode))
				       ("type"   . "application/tex")))
 		      ("zip"        . (("viewer" . mm-save-binary-file)
 				       ("type"   . "application/zip")
 				       ("copiousoutput")))
		      ("pdf"        . (("viewer" . "acroread %s")
				       ("type"   . "application/pdf")))
		      ("postscript" . (("viewer" . "open %s")
				       ("type"   . "application/postscript")
				       ("test"   . (eq (device-type) 'ns))))
		      ("postscript" . (("viewer" . "ghostview %s")
				       ("type" . "application/postscript")
				       ("test"   . (eq (device-type) 'x))
				       ("needsx11")))
		      ("postscript" . (("viewer" . "ps2ascii %s")
				       ("type" . "application/postscript")
				       ("test" . (not (getenv "DISPLAY")))
				       ("copiousoutput")))
		      ))
    ("audio"       . (
		      ("x-mpeg" . (("viewer" . "maplay %s")
				   ("type"   . "audio/x-mpeg")))
		      (".*" . (("viewer" . mm-play-sound-file)
			       ("test"   . (or (featurep 'nas-sound)
					       (featurep 'native-sound)))
			       ("type"   . "audio/*")))
		      (".*" . (("viewer" . "showaudio")
			       ("type"   . "audio/*")))
		      ))
    ("message"     . (
		      ("rfc-*822" . (("viewer" . vm-mode)
				     ("test"   . (fboundp 'vm-mode))
				     ("type"   . "message/rfc-822")))
		      ("rfc-*822" . (("viewer" . w3-mode)
				     ("test"   . (fboundp 'w3-mode))
				     ("type"   . "message/rfc-822")))
		      ("rfc-*822" . (("viewer" . view-mode)
				     ("test"   . (fboundp 'view-mode))
				     ("type"   . "message/rfc-822")))
		      ("rfc-*822" . (("viewer" . fundamental-mode)
				     ("type"   . "message/rfc-822")))
		      ))
    ("image"       . (
		      ("x-xwd" . (("viewer"  . "xwud -in %s")
				  ("type"    . "image/x-xwd")
				  ("compose" . "xwd -frame > %s")
				  ("test"    . (eq (device-type) 'x))
				  ("needsx11")))
		      ("x11-dump" . (("viewer" . "xwud -in %s")
				     ("type" . "image/x-xwd")
  				     ("compose" . "xwd -frame > %s")
				     ("test"   . (eq (device-type) 'x))
				     ("needsx11")))
		      ("windowdump" . (("viewer" . "xwud -in %s")
				       ("type" . "image/x-xwd")
    				       ("compose" . "xwd -frame > %s")
				       ("test"   . (eq (device-type) 'x))
				       ("needsx11")))
		      (".*" . (("viewer" . "open %s")
			       ("type"   . "image/*")
			       ("test"   . (eq (device-type) 'ns))))
		      (".*" . (("viewer" . "xv -perfect %s")
			       ("type" . "image/*")
			       ("test"   . (eq (device-type) 'x))
			       ("needsx11")))
		      ))
    ("text"        . (
		      ("plain" . (("viewer"  . w3-mode)
				  ("test"    . (fboundp 'w3-mode))
				  ("type"    . "text/plain")))
		      ("plain" . (("viewer"  . view-mode)
				  ("test"    . (fboundp 'view-mode))
				  ("type"    . "text/plain")))
		      ("plain" . (("viewer"  . fundamental-mode)
				  ("type"    . "text/plain")))
		      ("enriched" . (("viewer" . enriched-decode-region)
				     ("test"   . (fboundp
						  'enriched-decode-region))
				     ("type"   . "text/enriched")))
		      ("html"  . (("viewer" . w3-prepare-buffer)
				  ("test"   . (fboundp 'w3-prepare-buffer))
				  ("type"   . "text/html")))
		      ))
    ("video"       . (
		      ("mpeg" . (("viewer" . "mpeg_play %s")
				 ("type"   . "video/mpeg")
				 ("test"   . (eq (device-type) 'x))
				 ("needsx11")))
		      ))
    ("x-world"     . (
		      ("x-vrml" . (("viewer"  . "webspace -remote %s -URL %u")
				   ("type"    . "x-world/x-vrml")
				   ("description"
				    "VRML document")))))
    ("archive"     . (
		      ("tar"  . (("viewer" . tar-mode)
				 ("type" . "archive/tar")
				 ("test" . (fboundp 'tar-mode))))
		      ))
    )
  "*The mailcap structure is an assoc list of assoc lists.
1st assoc list is keyed on the major content-type
2nd assoc list is keyed on the minor content-type (which can be a regexp)

Which looks like:
-----------------
(
 (\"application\"
  (\"postscript\" . <info>)
 )
 (\"text\"
  (\"plain\" . <info>)
 )
)

Where <info> is another assoc list of the various information
related to the mailcap RFC.  This is keyed on the lowercase
attribute name (viewer, test, etc).  This looks like:
((\"viewer\" . viewerinfo)
 (\"test\"   . testinfo)
 (\"xxxx\"   . \"string\")
)

Where viewerinfo specifies how the content-type is viewed.  Can be
a string, in which case it is run through a shell, with
appropriate parameters, or a symbol, in which case the symbol is
funcall'd, with the buffer as an argument.

testinfo is a list of strings, or nil.  If nil, it means the
viewer specified is always valid.  If it is a list of strings,
these are used to determine whether a viewer passes the 'test' or
not.")

(defvar mm-mime-data ()
  "Parsed mailcap entries.
It has the same format as `mm-mime-data-default'.")

(defvar mm-content-transfer-encodings
  '(("base64"     . base64-decode-region)
    ("7bit"       . ignore)
    ("8bit"       . ignore)
    ("binary"     . ignore)
    ("x-compress" . ("uncompress" "-c"))
    ("x-gzip"     . ("gzip" "-dc"))
    ("compress"   . ("uncompress" "-c"))
    ("gzip"       . ("gzip" "-dc"))
    ("x-hqx"      . ("mcvert" "-P" "-s" "-S"))
    ("quoted-printable" . mm-decode-quoted-printable)
    )
  "*An assoc list of content-transfer-encodings and how to decode them.")

(defvar mm-download-directory nil
  "*Where downloaded files should go by default.")

(defvar mm-temporary-directory (or (getenv "TMPDIR") "/tmp")
  "*Where temporary files go.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A few things from w3 and url, just in case this is used without them
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mm-generate-unique-filename (&optional fmt)
  "Generate a unique filename in mm-temporary-directory"
  (if (not fmt)
      (let ((base (format "mm-tmp.%d" (user-real-uid)))
	    (fname "")
	    (x 0))
	(setq fname (format "%s%d" base x))
	(while (file-exists-p
		(expand-file-name fname mm-temporary-directory))
	  (setq x (1+ x)
		fname (concat base (int-to-string x))))
	(expand-file-name fname mm-temporary-directory))
    (let ((base (concat "mm" (int-to-string (user-real-uid))))
	  (fname "")
	  (x 0))
      (setq fname (format fmt (concat base (int-to-string x))))
      (while (file-exists-p
	      (expand-file-name fname mm-temporary-directory))
	(setq x (1+ x)
	      fname (format fmt (concat base (int-to-string x)))))
      (expand-file-name fname mm-temporary-directory))))

(if (and (fboundp 'copy-tree)
	 (subrp (symbol-function 'copy-tree)))
    (fset 'mm-copy-tree 'copy-tree)
  (defun mm-copy-tree (tree)
    (if (consp tree)
	(cons (mm-copy-tree (car tree))
	      (mm-copy-tree (cdr tree)))
      (if (vectorp tree)
	  (let* ((new (copy-sequence tree))
		 (i (1- (length new))))
	    (while (>= i 0)
	      (aset new i (mm-copy-tree (aref new i)))
	      (setq i (1- i)))
	    new)
	tree))))

(require 'mule-sysdp)

(if (not (fboundp 'w3-save-binary-file))
    (defun mm-save-binary-file ()
      ;; Ok, this is truly fucked.  In XEmacs, if you use the mouse to select
      ;; a URL that gets saved via this function, read-file-name will pop up a
      ;; dialog box for file selection.  For some reason which buffer we are in
      ;; gets royally screwed (even with save-excursions and the whole nine
      ;; yards).  SO, we just keep the old buffer name around and away we go.
      (let ((old-buff (current-buffer))
	    (file (read-file-name "Filename to save as: "
				  (or mm-download-directory "~/")
				  (file-name-nondirectory (url-view-url t))
				  nil
				  (file-name-nondirectory (url-view-url t))))
	    (require-final-newline nil))
	(set-buffer old-buff)
	(mule-write-region-no-coding-system (point-min) (point-max) file)
	(kill-buffer (current-buffer))))
  (fset 'mm-save-binary-file 'w3-save-binary-file))

(defun mm-maybe-eval ()
  "Maybe evaluate a buffer of emacs lisp code"
  (if (yes-or-no-p "This is emacs-lisp code, evaluate it? ")
      (eval-buffer (current-buffer))
    (emacs-lisp-mode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The mailcap parser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mm-viewer-unescape (format &optional filename url)
  (save-excursion
    (set-buffer (get-buffer-create " *mm-parse*"))
    (erase-buffer)
    (insert format)
    (goto-char (point-min))
    (while (re-search-forward "%\\(.\\)" nil t)
       (let ((escape (aref (match-string 1) 0)))
	 (replace-match "" t t)
	 (case escape
	   (?% (insert "%"))
	   (?s (insert (or filename "\"\"")))
	   (?u (insert (or url "\"\""))))))
    (buffer-string)))

(defun mm-in-assoc (elt list)
  ;; Check to see if ELT matches any of the regexps in the car elements of LIST
  (let (rslt)
    (while (and list (not rslt))
      (and (car (car list))
	   (string-match (car (car list)) elt)
	   (setq rslt (car list)))
      (setq list (cdr list)))
    rslt))

(defun mm-replace-regexp (regexp to-string)
  ;; Quiet replace-regexp.
  (goto-char (point-min))
  (while (re-search-forward regexp nil t)
    (replace-match to-string t nil)))

(defun mm-parse-mailcaps (&optional path)
  ;; Parse out all the mailcaps specified in a unix-style path string PATH
  (cond
   (path nil)
   ((getenv "MAILCAPS") (setq path (getenv "MAILCAPS")))
   ((memq system-type '(ms-dos ms-windows windows-nt))
    (setq path (mapconcat 'expand-file-name '("~/mail.cap" "~/etc/mail.cap")
			  ";")))
   (t (setq path (mapconcat 'expand-file-name
			    '("~/.mailcap"
			      "/etc/mailcap:/usr/etc/mailcap"
			      "/usr/local/etc/mailcap") ":"))))
  (let ((fnames (reverse
		 (mm-string-to-tokens path
				      (if (memq system-type
						'(ms-dos ms-windows windows-nt))
					  ?;
					?:))))
	fname)
    (while fnames
      (setq fname (car fnames))
      (if (and fname
	       (file-exists-p fname)
	       (file-readable-p fname)
	       (file-regular-p fname))
	  (mm-parse-mailcap (car fnames)))
      (setq fnames (cdr fnames)))))

(defun mm-parse-mailcap (fname)
  ;; Parse out the mailcap file specified by FNAME
  (let (major				; The major mime type (image/audio/etc)
	minor				; The minor mime type (gif, basic, etc)
	save-pos			; Misc saved positions used in parsing
	viewer				; How to view this mime type
	info				; Misc info about this mime type
	)
    (save-excursion
      (set-buffer (get-buffer-create " *mailcap*"))
      (erase-buffer)
      (insert-file-contents fname)
      (set-syntax-table mm-parse-args-syntax-table)
      (mm-replace-regexp "#.*" "")	         ; Remove all comments
      (mm-replace-regexp "\n+" "\n")         ; And blank lines
      (mm-replace-regexp "\\\\[ \t\n]+" " ") ; And collapse spaces
      (mm-replace-regexp "\\+" "\\\\+") ; and +s
      (mm-replace-regexp (concat (regexp-quote "\\") "[ \t]*\n") "")
      (goto-char (point-max))
      (skip-chars-backward " \t\n")
      (delete-region (point) (point-max))
      (goto-char (point-min))
      (while (not (eobp))
	(skip-chars-forward " \t\n")
	(setq save-pos (point)
	      info nil)
	(skip-chars-forward "^/;")
	(downcase-region save-pos (point))
	(setq major (buffer-substring save-pos (point)))
	(skip-chars-forward "/ \t\n")
	(setq save-pos (point))
	(skip-chars-forward "^;")
	(downcase-region save-pos (point))
	(setq minor
	      (cond
	       ((= ?* (or (char-after save-pos) 0)) ".*")
	       ((= (point) save-pos) ".*")
	       (t (buffer-substring save-pos (point)))))
	(skip-chars-forward "; \t\n")
	;;; Got the major/minor chunks, now for the viewers/etc
	;;; The first item _must_ be a viewer, according to the
	;;; RFC for mailcap files (#1343)
	(skip-chars-forward "; \t\n")
	(setq save-pos (point))
	(skip-chars-forward "^;\n")
	(if (= (or (char-after save-pos) 0) ?')
	    (setq viewer (progn
			   (narrow-to-region (1+ save-pos) (point))
			   (goto-char (point-min))
			   (prog1
			       (read (current-buffer))
			     (goto-char (point-max))
			     (widen))))
	  (setq viewer (buffer-substring save-pos (point))))
	(setq save-pos (point))
	(end-of-line)
	(setq info (nconc (list (cons "viewer" viewer)
				(cons "type" (concat major "/"
						     (if (string= minor ".*")
							 "*" minor))))
			  (mm-parse-mailcap-extras save-pos (point))))
	(mm-mailcap-entry-passes-test info)
	(mm-add-mailcap-entry major minor info)))))

(defun mm-parse-mailcap-extras (st nd)
  ;; Grab all the extra stuff from a mailcap entry
  (let (
	name				; From name=
	value				; its value
	results				; Assoc list of results
	name-pos			; Start of XXXX= position
	val-pos				; Start of value position
	done				; Found end of \'d ;s?
	)
    (save-restriction
      (narrow-to-region st nd)
      (goto-char (point-min))
      (skip-chars-forward " \n\t;")
      (while (not (eobp))
	(setq done nil)
	(skip-chars-forward " \";\n\t")
	(setq name-pos (point))
	(skip-chars-forward "^ \n\t=")
	(downcase-region name-pos (point))
	(setq name (buffer-substring name-pos (point)))
	(skip-chars-forward " \t\n")
	(if (/= (or (char-after (point)) 0)  ?=) ; There is no value
	    (setq value nil)
	  (skip-chars-forward " \t\n=")
	  (setq val-pos (point))
	  (if (memq (char-after val-pos) '(?\" ?'))
	      (progn
		(setq val-pos (1+ val-pos))
		(condition-case nil
		    (progn
		      (forward-sexp 1)
		      (backward-char 1))
		  (error (goto-char (point-max)))))
	    (while (not done)
	      (skip-chars-forward "^;")
	      (if (= (or (char-after (1- (point))) 0) ?\\ )
		  (progn
		    (subst-char-in-region (1- (point)) (point) ?\\ ? )
		    (skip-chars-forward ";"))
		(setq done t))))
	  (setq	value (buffer-substring val-pos (point))))
	(setq results (cons (cons name value) results)))
      results)))  

(defun mm-string-to-tokens (str &optional delim)
  "Return a list of words from the string STR"
  (setq delim (or delim ? ))
  (let (results y)
    (mapcar
     (function
      (lambda (x)
	(cond
	 ((and (= x delim) y) (setq results (cons y results) y nil))
	 ((/= x delim) (setq y (concat y (char-to-string x))))
	 (t nil)))) str)
    (nreverse (cons y results))))

(defun mm-mailcap-entry-passes-test (info)
  ;; Return t iff a mailcap entry passes its test clause or no test
  ;; clause is present.
  (let (status				; Call-process-regions return value
	(test (assoc "test" info)); The test clause
	)
    (setq status (and test (mm-string-to-tokens (cdr test))))
    (if (and (assoc "needsx11" info) (not (getenv "DISPLAY")))
	(setq status nil)
      (cond
       ((and (equal (nth 0 status) "test")
	     (equal (nth 1 status) "-n")
	     (or (equal (nth 2 status) "$DISPLAY")
		 (equal (nth 2 status) "\"$DISPLAY\"")))
	(setq status (if (getenv "DISPLAY") t nil)))
       ((and (equal (nth 0 status) "test")
	     (equal (nth 1 status) "-z")
	     (or (equal (nth 2 status) "$DISPLAY")
		 (equal (nth 2 status) "\"$DISPLAY\"")))
	(setq status (if (getenv "DISPLAY") nil t)))
       (test nil)
       (t nil)))
    (and test (listp test) (setcdr test status))))

(defun mm-parse-args (st &optional nd nodowncase)
  ;; Return an assoc list of attribute/value pairs from an RFC822-type string
  (let (
	name				; From name=
	value				; its value
	results				; Assoc list of results
	name-pos			; Start of XXXX= position
	val-pos				; Start of value position
	)
    (save-excursion
      (if (stringp st)
	  (progn
	    (set-buffer (get-buffer-create " *mm-temp*"))
	    (set-syntax-table mm-parse-args-syntax-table)
	    (erase-buffer)
	    (insert st)
	    (setq st (point-min)
		  nd (point-max)))
	(set-syntax-table mm-parse-args-syntax-table))
      (save-restriction
	(narrow-to-region st nd)
	(goto-char (point-min))
	(while (not (eobp))
	  (skip-chars-forward "; \n\t")
	  (setq name-pos (point))
	  (skip-chars-forward "^ \n\t=;")
	  (if (not nodowncase)
	      (downcase-region name-pos (point)))
	  (setq name (buffer-substring name-pos (point)))
	  (skip-chars-forward " \t\n")
	  (if (/= (or (char-after (point)) 0)  ?=) ; There is no value
	      (setq value nil)
	    (skip-chars-forward " \t\n=")
	    (setq val-pos (point)
		  value
		  (cond
		   ((or (= (or (char-after val-pos) 0) ?\")
			(= (or (char-after val-pos) 0) ?'))
		    (buffer-substring (1+ val-pos)
				      (condition-case ()
					  (prog2
					      (forward-sexp 1)
					      (1- (point))
					    (skip-chars-forward "\""))
					(error
					 (skip-chars-forward "^ \t\n")
					 (point)))))
		   (t
		    (buffer-substring val-pos
				      (progn
					(skip-chars-forward "^;")
					(skip-chars-backward " \t")
					(point)))))))
	  (setq results (cons (cons name value) results))
	  (skip-chars-forward "; \n\t"))
	results))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The action routines.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mm-possible-viewers (major minor)
  ;; Return a list of possible viewers from MAJOR for minor type MINOR
  (let ((exact '())
	(wildcard '()))
    (while major
      (cond
       ((equal (car (car major)) minor)
	(setq exact (cons (cdr (car major)) exact)))
       ((string-match (car (car major)) minor)
	(setq wildcard (cons (cdr (car major)) wildcard))))
      (setq major (cdr major)))
    (nconc exact wildcard)))

(defun mm-unescape-mime-test (test type-info)
  (let ((buff (get-buffer-create " *unescape*"))
	save-pos save-chr subst)
    (cond
     ((symbolp test) test)
     ((and (listp test) (symbolp (car test))) test)
     ((or (stringp test)
	  (and (listp test) (stringp (car test))
	       (setq test (mapconcat 'identity test " "))))
      (save-excursion
	(set-buffer buff)
	(erase-buffer)
	(insert test)
	(goto-char (point-min))
	(while (not (eobp))
	  (skip-chars-forward "^%")
	  (if (/= (- (point)
		     (progn (skip-chars-backward "\\\\")
			    (point)))
		  0) ; It is an escaped %
	      (progn
		(delete-char 1)
		(skip-chars-forward "%."))
	    (setq save-pos (point))
	    (skip-chars-forward "%")
	    (setq save-chr (char-after (point)))
	    (cond
	     ((null save-chr) nil)
	     ((= save-chr ?t)
	      (delete-region save-pos (progn (forward-char 1) (point)))
	      (insert (or (cdr (assoc "type" type-info)) "\"\"")))
	     ((= save-chr ?M)
	      (delete-region save-pos (progn (forward-char 1) (point)))
	      (insert "\"\""))
	     ((= save-chr ?n)
	      (delete-region save-pos (progn (forward-char 1) (point)))
	      (insert "\"\""))
	     ((= save-chr ?F)
	      (delete-region save-pos (progn (forward-char 1) (point)))
	      (insert "\"\""))
	     ((= save-chr ?{)
	      (forward-char 1)
	      (skip-chars-forward "^}")
	      (downcase-region (+ 2 save-pos) (point))
	      (setq subst (buffer-substring (+ 2 save-pos) (point)))
	      (delete-region save-pos (1+ (point)))
	      (insert (or (cdr (assoc subst type-info)) "\"\"")))
	     (t nil))))
	(buffer-string)))
     (t (error "Bad value to mm-unescape-mime-test. %s" test)))))

(defun mm-viewer-passes-test (viewer-info type-info)
  ;; Return non-nil iff the viewer specified by VIEWER-INFO passes its
  ;; test clause (if any).
  (let* ((test-info   (assoc "test"   viewer-info))
	 (test (cdr test-info))
	 (viewer (cdr (assoc "viewer" viewer-info)))
	 (default-directory (expand-file-name "~/"))
	 status
	 parsed-test
	)
    (cond
     ((not test-info) t)		; No test clause
     ((not test) nil)			; Already failed test
     ((eq test t) t)			; Already passed test
     ((and (symbolp test)		; Lisp function as test
	   (fboundp test))
      (funcall test type-info))
     ((and (symbolp test)		; Lisp variable as test
	   (boundp test))
      (symbol-value test))
     ((and (listp test)			; List to be eval'd
	   (symbolp (car test)))
      (eval test))
     (t
      (setq test (mm-unescape-mime-test test type-info)
	    test (list shell-file-name nil nil nil shell-command-switch test)
	    status (apply 'call-process test))
      (= 0 status)))))

(defun mm-add-mailcap-entry (major minor info)
  (let ((old-major (assoc major mm-mime-data)))
    (if (null old-major)		; New major area
	(setq mm-mime-data
	      (cons (cons major (list (cons minor info)))
		    mm-mime-data))
      (let ((cur-minor (assoc minor old-major)))
	(cond
	 ((or (null cur-minor)		; New minor area, or
	      (assoc "test" info))	; Has a test, insert at beginning
	  (setcdr old-major (cons (cons minor info) (cdr old-major))))
	 ((and (not (assoc "test" info)); No test info, replace completely
	       (not (assoc "test" cur-minor)))
	  (setcdr cur-minor info))
	 (t
	  (setcdr old-major (cons (cons minor info) (cdr old-major)))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The main whabbo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mm-viewer-lessp (x y)
  ;; Return t iff viewer X is more desirable than viewer Y
  (let ((x-wild (string-match "[*?]" (or (cdr-safe (assoc "type" x)) "")))
	(y-wild (string-match "[*?]" (or (cdr-safe (assoc "type" y)) "")))
	(x-lisp (not (stringp (or (cdr-safe (assoc "viewer" x)) ""))))
	(y-lisp (not (stringp (or (cdr-safe (assoc "viewer" y)) "")))))
    (cond
     ((and x-lisp (not y-lisp))
      t)
     ((and (not y-lisp) x-wild (not y-wild))
      t)
     ((and (not x-wild) y-wild)
      t)
     (t nil))))

(defun mm-mime-info (st &optional nd request)
  "Get the mime viewer command for HEADERLINE, return nil if none found.
Expects a complete content-type header line as its argument.  This can
be simple like text/html, or complex like text/plain; charset=blah; foo=bar

Third argument REQUEST specifies what information to return.  If it is
nil or the empty string, the viewer (second field of the mailcap
entry) will be returned.  If it is a string, then the mailcap field
corresponding to that string will be returned (print, description,
whatever).  If a number, then all the information for this specific
viewer is returned."
  (let (
	major				; Major encoding (text, etc)
	minor				; Minor encoding (html, etc)
	info				; Other info
	save-pos			; Misc. position during parse
	major-info			; (assoc major mm-mime-data)
	major-info-default		; (assoc major mm-mime-data-default)
	minor-info			; (assoc minor major-info)
	test				; current test proc.
	viewers				; Possible viewers
	viewers-default			; Possible W3 default viewers
	passed				; Viewers that passed the test
	passed-default			; W3 viewers that passed the test
	viewer				; The one and only viewer
	)
    (save-excursion
      (cond
       ((null st)
	(set-buffer (get-buffer-create " *mimeparse*"))
	(erase-buffer)
	(insert "text/plain")
	(setq st (point-min)))
       ((stringp st)
	(set-buffer (get-buffer-create " *mimeparse*"))
	(erase-buffer)
	(insert st)
	(setq st (point-min)))
       ((null nd)
	(narrow-to-region st (progn (goto-char st) (end-of-line) (point))))
       (t (narrow-to-region st nd)))
      (goto-char st)
      (skip-chars-forward ": \t\n")
      (buffer-enable-undo)
      (setq viewer
	    (catch 'mm-exit
	      (setq save-pos (point))
	      (skip-chars-forward "^/")
	      (downcase-region save-pos (point))
	      (setq major (buffer-substring save-pos (point)))
	      (setq major-info (cdr (assoc major mm-mime-data))
		    major-info-default (cdr (assoc major
						   mm-mime-data-default)))
	      (if (and (not major-info)
		       (not major-info-default))
		  (throw 'mm-exit nil))
	      (skip-chars-forward "/ \t\n")
	      (setq save-pos (point))
	      (skip-chars-forward "^ \t\n;")
	      (downcase-region save-pos (point))
	      (setq minor (buffer-substring save-pos (point)))
	      (setq viewers (mm-possible-viewers major-info minor)
		    viewers-default (mm-possible-viewers
				     major-info-default minor))
	      (if (and (not viewers)
		       (not viewers-default))
		  (throw 'mm-exit nil))
	      (skip-chars-forward "; \t")
	      (if (eolp)
		  nil				; No qualifiers
		(setq save-pos (point))
		(end-of-line)
		(setq info (mm-parse-args save-pos (point)))
		)
	      (while viewers
		(if (mm-viewer-passes-test (car viewers) info)
		    (setq passed (cons (car viewers) passed)))
		(setq viewers (cdr viewers)))
	      (setq passed (sort (nreverse passed) 'mm-viewer-lessp))
	      (while viewers-default
		(if (mm-viewer-passes-test (car viewers-default) info)
		    (setq passed-default
			  (cons (car viewers-default) passed-default)))
		(setq viewers-default (cdr viewers-default)))
	      (setq passed-default (sort (nreverse passed-default)
					 'mm-viewer-lessp))
	      (if (or (not passed)
		      (and passed-default
			   (not (stringp
				 (or (cdr-safe (assoc
						"viewer" (car passed-default)))
				     "")))))
		  (car passed-default)
		(car passed))))
      (if (and (stringp (cdr (assoc "viewer" viewer)))
	       passed)
	  (setq viewer (car passed)))
      (widen)
      (cond
       ((and (null viewer) (not (equal major "default")))
	(mm-mime-info "default" nil request))
       ((or (null request) (equal request ""))
	(mm-unescape-mime-test (cdr (assoc "viewer" viewer)) info))
       ((stringp request)
	(if (or (string= request "test") (string= request "viewer"))
	    (mm-unescape-mime-test (cdr-safe (assoc request viewer)) info)))
       (t
	;; MUST make a copy *sigh*, else we modify mm-mime-data
	(setq viewer (mm-copy-tree viewer))
	(let ((view (assoc "viewer" viewer))
	      (test (assoc "test" viewer)))
	  (if view (setcdr view (mm-unescape-mime-test (cdr view) info)))
	  (if test (setcdr test (mm-unescape-mime-test (cdr test) info))))
	viewer)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Experimental MIME-types parsing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar mm-mime-extensions
  '(
    (""          . "text/plain")
    (".abs"      . "audio/x-mpeg")
    (".aif"      . "audio/aiff")
    (".aifc"     . "audio/aiff")
    (".aiff"     . "audio/aiff")
    (".ano"      . "application/x-annotator")
    (".au"       . "audio/ulaw")
    (".avi"      . "video/x-msvideo")
    (".bcpio"    . "application/x-bcpio")
    (".bin"      . "application/octet-stream")
    (".cdf"      . "application/x-netcdr")
    (".cpio"     . "application/x-cpio")
    (".csh"      . "application/x-csh")
    (".dvi"      . "application/x-dvi")
    (".el"       . "application/emacs-lisp")
    (".eps"      . "application/postscript")
    (".etx"      . "text/x-setext")
    (".exe"      . "application/octet-stream")
    (".fax"      . "image/x-fax")
    (".gif"      . "image/gif")
    (".hdf"      . "application/x-hdf")
    (".hqx"      . "application/mac-binhex40")
    (".htm"      . "text/html")
    (".html"     . "text/html")
    (".icon"     . "image/x-icon")
    (".ief"      . "image/ief")
    (".jpg"      . "image/jpeg")
    (".macp"     . "image/x-macpaint")
    (".man"      . "application/x-troff-man")
    (".me"       . "application/x-troff-me")
    (".mif"      . "application/mif")
    (".mov"      . "video/quicktime")
    (".movie"    . "video/x-sgi-movie")
    (".mp2"      . "audio/x-mpeg")
    (".mp2a"     . "audio/x-mpeg2")
    (".mpa"      . "audio/x-mpeg")
    (".mpa2"     . "audio/x-mpeg2")
    (".mpe"      . "video/mpeg")
    (".mpeg"     . "video/mpeg")
    (".mpega"    . "audio/x-mpeg")
    (".mpegv"    . "video/mpeg")
    (".mpg"      . "video/mpeg")
    (".mpv"      . "video/mpeg")
    (".ms"       . "application/x-troff-ms")
    (".nc"       . "application/x-netcdf")
    (".nc"       . "application/x-netcdf")
    (".oda"      . "application/oda")
    (".pbm"      . "image/x-portable-bitmap")
    (".pdf"      . "application/pdf")
    (".pgm"      . "image/portable-graymap")
    (".pict"     . "image/pict")
    (".png"      . "image/png")
    (".pnm"      . "image/x-portable-anymap")
    (".ppm"      . "image/portable-pixmap")
    (".ps"       . "application/postscript")
    (".qt"       . "video/quicktime")
    (".ras"      . "image/x-raster")
    (".rgb"      . "image/x-rgb")
    (".rtf"      . "application/rtf")
    (".rtx"      . "text/richtext")
    (".sh"       . "application/x-sh")
    (".sit"      . "application/x-stuffit")
    (".snd"      . "audio/basic")
    (".src"      . "application/x-wais-source")
    (".tar"      . "archive/tar")
    (".tcl"      . "application/x-tcl")
    (".tcl"      . "application/x-tcl")
    (".tex"      . "application/x-tex")
    (".texi"     . "application/texinfo")
    (".tga"      . "image/x-targa")
    (".tif"      . "image/tiff")
    (".tiff"     . "image/tiff")
    (".tr"       . "application/x-troff")
    (".troff"    . "application/x-troff")
    (".tsv"      . "text/tab-separated-values")
    (".txt"      . "text/plain")
    (".vbs"      . "video/mpeg")
    (".vox"      . "audio/basic")
    (".vrml"     . "x-world/x-vrml")
    (".wav"      . "audio/x-wav")
    (".wrl"      . "x-world/x-vrml")
    (".xbm"      . "image/xbm")
    (".xpm"      . "image/x-pixmap")
    (".xwd"      . "image/windowdump")
    (".zip"      . "application/zip")
    (".ai"       . "application/postscript")
    (".jpe"      . "image/jpeg")
    (".jpeg"     . "image/jpeg")
    )
  "*An assoc list of file extensions and the MIME content-types they
correspond to.")

(defun mm-parse-mimetypes (&optional path)
  ;; Parse out all the mimetypes specified in a unix-style path string PATH
  (cond
   (path nil)
   ((getenv "MIMETYPES") (setq path (getenv "MIMETYPES")))
   ((memq system-type '(ms-dos ms-windows windows-nt))
    (setq path (mapconcat 'expand-file-name
			  '("~/mime.typ" "~/etc/mime.typ") ";")))
   (t (setq path (mapconcat 'expand-file-name
			    '("~/.mime-types"
			      "/etc/mime-types:/usr/etc/mime-types"
			      "/usr/local/etc/mime-types"
			      "/usr/local/www/conf/mime-types") ":"))))
  (let ((fnames (reverse
		 (mm-string-to-tokens path
				      (if (memq system-type
						'(ms-dos ms-windows windows-nt))
					  ?;
					?:))))
	fname)
    (while fnames
      (setq fname (car fnames))
      (if (and (file-exists-p fname) (file-readable-p fname))
	  (mm-parse-mimetype-file (car fnames)))
      (setq fnames (cdr fnames)))))

(defun mm-parse-mimetype-file (fname)
  ;; Parse out a mime-types file
  (let (type				; The MIME type for this line
	extns				; The extensions for this line
	save-pos			; Misc. saved buffer positions
	)
    (save-excursion
      (set-buffer (get-buffer-create " *mime-types*"))
      (erase-buffer)
      (insert-file-contents fname)
      (mm-replace-regexp "#.*" "")
      (mm-replace-regexp "\n+" "\n")
      (mm-replace-regexp "[ \t]+$" "")
      (goto-char (point-max))
      (skip-chars-backward " \t\n")
      (delete-region (point) (point-max))
      (goto-char (point-min))
      (while (not (eobp))
	(skip-chars-forward " \t\n")
	(setq save-pos (point))
	(skip-chars-forward "^ \t")
	(downcase-region save-pos (point))
	(setq type (buffer-substring save-pos (point)))
	(while (not (eolp))
	  (skip-chars-forward " \t")
	  (setq save-pos (point))
	  (skip-chars-forward "^ \t\n")
	  (setq extns (cons (buffer-substring save-pos (point)) extns)))
	(while extns
	  (setq mm-mime-extensions
		(cons
		 (cons (if (= (string-to-char (car extns)) ?.)
			   (car extns)
			 (concat "." (car extns))) type) mm-mime-extensions)
		extns (cdr extns)))))))

(defun mm-extension-to-mime (extn)
  "Return the MIME content type of the file extensions EXTN"
  (if (and (stringp extn)
	   (not (eq (string-to-char extn) ?.)))
      (setq extn (concat "." extn)))
  (cdr (assoc (downcase extn) mm-mime-extensions)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Editing/Composition of body parts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mm-compose-type (type)
  ;; Compose a body section of MIME-type TYPE.
  (let* ((info (mm-mime-info type nil 5))
	 (fnam (mm-generate-unique-filename))
	 (comp (or (cdr (assoc "compose" info))))
	 (ctyp (cdr (assoc "composetyped" info)))
	 (buff (get-buffer-create " *mimecompose*"))
	 (typeit (not ctyp))
	 (retval "")
	 (usef nil))
    (setq comp (mm-unescape-mime-test (or comp ctyp) info))
    (while (string-match "\\([^\\\\]\\)%s" comp)
      (setq comp (concat (substring comp 0 (match-end 1)) fnam
			 (substring comp (match-end 0) nil))
	    usef t))
    (call-process shell-file-name nil
		  (if usef nil buff)
		  nil shell-command-switch comp)
    (setq retval
	  (concat
	   (if typeit (concat "Content-type: " type "\r\n\r\n") "")
	   (if usef
	       (save-excursion
		 (set-buffer buff)
		 (erase-buffer)
		 (insert-file-contents fnam)
		 (buffer-string))
	     (save-excursion
	       (set-buffer buff)
	       (buffer-string)))
	   "\r\n"))
    retval))	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Misc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mm-type-to-file (type)
  "Return the file extension for content-type TYPE"
  (rassoc type mm-mime-extensions))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Miscellaneous MIME viewers written in elisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mm-play-sound-file (&optional buff)
  "Play a sound file in buffer BUFF (defaults to current buffer)"
  (setq buff (or buff (current-buffer)))
  (let ((fname (mm-generate-unique-filename "%s.au"))
	(synchronous-sounds t))		; Play synchronously
    (mule-write-region-no-coding-system (point-min) (point-max) fname)
    (kill-buffer (current-buffer))
    (play-sound-file fname)
    (condition-case ()
	(delete-file fname)
      (error nil))))
    
(defun mm-parse-mime-headers (&optional no-delete)
  "Return a list of the MIME headers at the top of this buffer.  If
optional argument NO-DELETE is non-nil, don't delete the headers."
  (let* ((st (point-min))
	 (nd (progn
	       (goto-char (point-min))
	       (skip-chars-forward " \t\n")
	       (if (re-search-forward "^\r*$" nil t)
		   (1+ (point))
		 (point-max))))
	 save-pos
	 status
	 hname
	 hvalu
	 result
	 search
	 )
    (narrow-to-region st (min nd (point-max)))
    (goto-char (point-min))
    (while (not (eobp))
      (skip-chars-forward " \t\n\r")
      (setq save-pos (point))
      (skip-chars-forward "^:\n\r")
      (downcase-region save-pos (point))
      (setq hname (buffer-substring save-pos (point)))
      (skip-chars-forward ": \t ")
      (setq save-pos (point))
      (skip-chars-forward "^\n\r")
      (setq search t)
      (while search
	(skip-chars-forward "^\n\r")
	(save-excursion
	  (skip-chars-forward "\n\r")
	  
	  (setq search
		(string-match "[ \t]"
			      (char-to-string
			       (or (char-after (point)) ?a)))))
	(if search
	    (skip-chars-forward "\n\r")))
      (setq hvalu (buffer-substring save-pos (point))
	    result (cons (cons hname hvalu) result)))
    (or no-delete (delete-region st nd))
    result))

(defun mm-find-available-multiparts (separator &optional buf)
  "Return a list of mime-headers for the various body parts of a 
multipart message in buffer BUF with separator SEPARATOR.
The different multipart specs are put in `mm-temporary-directory'."
  (let ((sep (concat "^--" separator "\r*$"))
	headers
	fname
	results)
    (save-excursion
      (and buf (set-buffer buf))
      (goto-char (point-min))
      (while (re-search-forward sep nil t)
	(let ((st (set-marker (make-marker)
			      (progn
				(forward-line 1)
				(beginning-of-line)
				(point))))
	      (nd (set-marker (make-marker)
			      (if (re-search-forward sep nil t)
				  (1- (match-beginning 0))
				(point-max)))))
	  (narrow-to-region st nd)
	  (goto-char st)
	  (if (looking-at "^\r*$")
	      (insert "Content-type: text/plain\n"
		      "Content-length: " (int-to-string (- nd st)) "\n"))
	  (setq headers (mm-parse-mime-headers)
		fname (mm-generate-unique-filename))
	  (let ((x (or (cdr (assoc "content-type" headers)) "text/plain")))
	    (if (string-match "name=\"*\\([^ \"]+\\)\"*" x)
		(setq fname (expand-file-name
			     (substring x (match-beginning 1)
					(match-end 1))
			     mm-temporary-directory))))
	  (widen)
	  (if (assoc "content-transfer-encoding" headers)
	      (let ((coding (cdr
			     (assoc "content-transfer-encoding" headers)))
		    (cmd nil))
		(setq coding (and coding (downcase coding))
		      cmd (or (cdr (assoc coding
					  mm-content-transfer-encodings))
			      (read-string
			       (concat "How shall I decode " coding "? ")
			       "cat")))
		(if (string= cmd "") (setq cmd "cat"))
		(if (stringp cmd)
		    (shell-command-on-region st nd cmd t)
		  (funcall cmd st nd))
		(or (eq cmd 'ignore) (set-marker nd (point)))))
	  (mule-write-region-no-coding-system st nd fname nil 5)
	  (delete-region st nd)
	  (setq results (cons
			 (cons
			  (cons "mm-filename" fname) headers) results)))))
    results))

(defun mm-format-multipart-as-html (&optional buf type)
  (if buf (set-buffer buf))
  (let* ((boundary (if (string-match
			"boundary[ \t]*=[ \t\"]*\\([^ \"\t\n]+\\)"
			type)
		       (regexp-quote
			(substring type (match-beginning 1) (match-end 1)))))
	 (parts    (mm-find-available-multiparts boundary)))
    (erase-buffer)
    (insert "<html>\n"
	    " <head>\n"
	    "  <title>Multipart Message</title>\n"
	    " </head>\n"
	    " <body>\n"
	    "   <h1> Multipart message encountered </h1>\n"
	    "   <p> I have encountered a multipart MIME message.\n"
	    "       The following parts have been detected.  Please\n"
	    "       select which one you want to view.\n"
	    "   </p>\n"
	    "   <ul>\n"
	    (mapconcat 
	     (function (lambda (x)
			 (concat "    <li> <a href=\"file:"
				 (cdr (assoc "mm-filename" x))
				 "\">"
				 (or (cdr (assoc "content-description" x)) "")
				 "--"
				 (or (cdr (assoc "content-type" x))
				     "unknown type")
				 "</a> </li>")))
	     parts "\n")
	    "   </ul>\n"
	    " </body>\n"
	    "</html>\n"
	    "<!-- Automatically generated by MM v" mm-version "-->\n")))

(defun mm-multipart-viewer ()
  (mm-format-multipart-as-html
   (current-buffer)
   (cdr (assoc "content-type" url-current-mime-headers)))
  (let ((w3-working-buffer (current-buffer)))
    (w3-prepare-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Transfer encodings we can decrypt automatically
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mm-decode-quoted-printable (&optional st nd)
  (interactive)
  (setq st (or st (point-min))
	nd (or nd (point-max)))
  (save-restriction
    (narrow-to-region st nd)
    (save-excursion
      (let ((buffer-read-only nil))
	(goto-char (point-min))
	(while (re-search-forward "=[0-9A-F][0-9A-F]" nil t)
	  (replace-match 
	   (char-to-string 
	    (+
	     (* 16 (mm-hex-char-to-integer 
		    (char-after (1+ (match-beginning 0)))))
	     (mm-hex-char-to-integer
	      (char-after (1- (match-end 0))))))))))
    (goto-char (point-max))))

;; Taken from hexl.el.
(defun mm-hex-char-to-integer (character)
  "Take a char and return its value as if it was a hex digit."
  (if (and (>= character ?0) (<= character ?9))
      (- character ?0)
    (let ((ch (logior character 32)))
      (if (and (>= ch ?a) (<= ch ?f))
	  (- ch (- ?a 10))
	(error (format "Invalid hex digit `%c'." ch))))))


(require 'base64)
(provide 'mm)
