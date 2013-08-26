;;; url-file.el --- File retrieval code
;; Author: wmperry
;; Created: 1999/11/09 14:52:21
;; Version: 1.4
;; Keywords: comm, data, processes

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

(require 'url-vars)
(require 'mule-sysdp)
(require 'w3-sysdp)
(require 'url-parse)

(defun url-insert-possibly-compressed-file (fname &rest args)
  ;; Insert a file into a buffer, checking for compressed versions.
  (let ((compressed nil)
	;;
	;; F*** *U** **C* ***K!!!
	;; We cannot just use insert-file-contents-literally here, because
	;; then we would lose big time with ange-ftp.  *sigh*
	(crypt-encoding-alist nil)
	(jka-compr-compression-info-list nil)
	(jam-zcat-filename-list nil)
	(file-coding-system-for-read mule-no-coding-system)
	(coding-system-for-read mule-no-coding-system))
    (setq compressed 
	  (cond
	   ((file-exists-p fname)
	    (if (string-match "\\.\\(z\\|gz\\|Z\\)$" fname)
		(case (intern (match-string 1 fname))
		  ((z gz)
		   (setq url-current-mime-headers (cons
						   (cons
						    "content-transfer-encoding"
						    "gzip")
						   url-current-mime-headers)))
		  (Z
		   (setq url-current-mime-headers (cons
						   (cons
						    "content-transfer-encoding"
						    "compress")
						   url-current-mime-headers))))
	      nil))
	   ((file-exists-p (concat fname ".Z"))
	    (setq fname (concat fname ".Z")
		  url-current-mime-headers (cons (cons
						  "content-transfer-encoding"
						  "compress")
						 url-current-mime-headers)))
	   ((file-exists-p (concat fname ".gz"))
	    (setq fname (concat fname ".gz")
		  url-current-mime-headers (cons (cons
						  "content-transfer-encoding"
						  "gzip")
						 url-current-mime-headers)))
	   ((file-exists-p (concat fname ".z"))
	    (setq fname (concat fname ".z")
		  url-current-mime-headers (cons (cons
						  "content-transfer-encoding"
						  "gzip")
						 url-current-mime-headers)))
	   (t
	    (error "File not found %s" fname))))
    (setq url-current-mime-encoding
	  (cdr (assoc "content-transfer-encoding" url-current-mime-headers)))
    (apply 'insert-file-contents fname args)
    (set-buffer-modified-p nil)))

(defvar url-dired-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-m" 'url-dired-find-file)
    (if url-running-xemacs
	(define-key map [button2] 'url-dired-find-file-mouse)
      (define-key map [mouse-2] 'url-dired-find-file-mouse))
    map)
  "Keymap used when browsing directories.")

(defvar url-dired-minor-mode nil
  "Whether we are in url-dired-minor-mode")

(make-variable-buffer-local 'url-dired-minor-mode)

(defun url-dired-find-file ()
  "In dired, visit the file or directory named on this line, using Emacs-W3."
  (interactive)
  (let ((filename (dired-get-filename)))
    (cond ((string-match "/\\(.*@.*\\):\\(/.*\\)" filename)
	   (w3-fetch (concat "file://" (match-string 1 filename) (match-string 2 filename))))
	  (t
	   (w3-open-local filename)))))

(defun url-dired-find-file-mouse (event)
  "In dired, visit the file or directory name you click on, using Emacs-W3."
  (interactive "@e")
    (if (event-point event)
	(progn
	  (goto-char (event-point event))
	  (url-dired-find-file))))

(defun url-dired-minor-mode (&optional arg)
  "Minor mode for directory browsing with Emacs-W3."
  (interactive "P")
  (cond
   ((null arg)
    (setq url-dired-minor-mode (not url-dired-minor-mode)))
   ((equal 0 arg)
    (setq url-dired-minor-mode nil))
   (t
    (setq url-dired-minor-mode t))))

(add-minor-mode 'url-dired-minor-mode " URL" url-dired-minor-mode-map)

(defun url-format-directory (dir)
  ;; Format the files in DIR into hypertext
  (kill-buffer (current-buffer))
  (find-file dir)
  (url-dired-minor-mode t))

(defun url-host-is-local-p (host)
  "Return t iff HOST references our local machine."
  (let ((case-fold-search t))
    (or
     (null host)
     (string= "" host)
     (equal (downcase host) (downcase (system-name)))
     (and (string-match "^localhost$" host) t)
     (and (not (string-match (regexp-quote ".") host))
	  (equal (downcase host) (if (string-match (regexp-quote ".")
						   (system-name))
				     (substring (system-name) 0
						(match-beginning 0))
				   (system-name)))))))

(defun url-file-build-continuation (name)
  (list 'url-file-asynch-callback
	name (current-buffer)
	url-current-callback-func url-current-callback-data))

(defun url-file-asynch-callback (x y name buff func args &optional efs)
  (if (featurep 'efs)
      ;; EFS passes us an extra argument
      (setq name buff
	    buff func
	    func args
	    args efs))
  (cond
   ((not name) nil)
   ((not (file-exists-p name)) nil)
   (t
    (if (not buff)
	(setq buff (generate-new-buffer " *url-asynch-file*")))
    (set-buffer buff)
    (url-insert-possibly-compressed-file name)
    (condition-case ()
	(delete-file name)
      (error nil))))
  (if func
      (apply func args)
    (url-sentinel (current-buffer) nil)))

(defun url-cleanup-file (url)
   "Clean up file URLs to remove questionable ftp stuff.
Removes any leading slashes and 'localhost's from the file URL,
and exchanges any | in the drive identifier with a :."
   (if (null (string-match "file:\\(\\(/\\|localhost\\)+\\)+[a-zA-Z]\\([:|]\\)"
			   url))
       url
     (let ((slash-start (match-beginning 1))
	   (slash-end (match-end 1))
	   (bar-start (match-beginning 3))
	   (bar-end (match-end 3)))
       (concat (substring url 0 slash-start)
	       (substring url slash-end bar-start)
	       ":"
	       (substring url bar-end)))))

(defun url-file (url)
  ;; Find a file
  (let* ((urlobj (url-generic-parse-url (url-cleanup-file url)))
	 (user (url-user urlobj))
	 (pass (url-password urlobj))
	 (site (url-host urlobj))
	 (port (url-port urlobj))
	 (site (if port (format "%s#%s" site port)))
	 (file (url-unhex-string (url-filename urlobj)))
	 (dest (url-target urlobj))
	 (filename (if (or user (not (url-host-is-local-p site)))
		       (concat "/" (or user "anonymous") "@" site ":" file)
		     (if (and (memq system-type
				    '(emx ms-dos windows-nt ms-windows))
			      (string-match "^/[a-zA-Z]:/" file))
			 (substring file 1)
		       file)))
	 (pos-index (if url-directory-index-file
			(expand-file-name url-directory-index-file filename)))
	 uncompressed-filename viewer)
    (url-clear-tmp-buffer)
    (and user pass
	 (cond
	  ((featurep 'ange-ftp)
	   (ange-ftp-set-passwd site user pass))
	  ((or (featurep 'efs) (featurep 'efs-auto))
	   (efs-set-passwd site user pass))
	  (t
	   nil)))

    (if (and url-current-object
	     (file-directory-p filename)
	     (not (string-match (format "%c$" directory-sep-char) filename)))
	(url-set-filename url-current-object
			  (format "%s%c" filename directory-sep-char)))

    (if (and pos-index
	     (file-exists-p pos-index)
	     (file-readable-p pos-index))
	(setq filename pos-index))

    (setq uncompressed-filename (if (string-match "\\.\\(gz\\|Z\\|z\\)$" filename)
				    (substring filename 0 (match-beginning 0))
				  filename))
    (setq url-current-mime-type (mm-extension-to-mime
				 (url-file-extension uncompressed-filename)))
    (setq viewer (mm-mime-info url-current-mime-type))

    (cond
     ((file-directory-p filename)
      (if (not (string-match "/$" filename))
	  (setq filename (concat filename "/")))
      (if (not (string-match "/$" file))
	  (setq file (concat file "/")))
      (url-set-filename urlobj file)
      (url-format-directory filename))
     (url-be-asynchronous
      (cond
       ((file-exists-p filename) nil)
       ((file-exists-p (concat filename ".Z"))
	(setq filename (concat filename ".Z")))
       ((file-exists-p (concat filename ".gz"))
	(setq filename (concat filename ".gz")))
       ((file-exists-p (concat filename ".z"))
	(setq filename (concat filename ".z")))
       (t nil))
      (let* ((extension (url-file-extension filename))
	     (new (mm-generate-unique-filename (and (> (length extension) 0)
						    (concat "%s." extension)))))
	(cond
	 ((url-host-is-local-p site)
	  (if (and (file-exists-p filename)
		   (file-readable-p filename))
	      (url-insert-possibly-compressed-file filename))
	  (if (featurep 'efs)
	      (url-file-asynch-callback nil nil nil nil nil
					url-current-callback-func
					url-current-callback-data)
	    (url-file-asynch-callback nil nil nil nil
				      url-current-callback-func
				      url-current-callback-data)))
	 ((featurep 'ange-ftp)
	  (ange-ftp-copy-file-internal filename (expand-file-name new) t
				       nil t
				       (url-file-build-continuation new)
				       t))
	 ((or (featurep 'efs) (featurep 'efs-auto))
	  (autoload 'efs-copy-file-internal "efs")
	  (efs-copy-file-internal filename (efs-ftp-path filename)
				  new (efs-ftp-path new)
				  t nil 0
				  (url-file-build-continuation new)
				  0 nil)))))
     (t
      (let ((errobj nil))
	(if (or url-source		; Need it in a buffer
		(and (symbolp viewer)
		     (not (eq viewer 'w3-default-local-file)))
		(stringp viewer))
	    (condition-case errobj
		(url-insert-possibly-compressed-file filename t)
	      (error
	       (url-save-error errobj)
	       (url-retrieve (concat "www://error/nofile/" file))))))))))

(fset 'url-ftp 'url-file)

(provide 'url-file)
