;;; w3-hot.el --- Main functions for emacs-w3 on all platforms/versions
;; Author: wmperry
;; Created: 1999/03/25 05:30:06
;; Version: 1.2
;; Keywords: faces, help, comm, news, mail, processes, mouse, hypermedia

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
;;; Structure for hotlists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (
;;;  ("name of item1" . "http://foo.bar.com/")    ;; A single item in hotlist
;;;  ("name of item2" . (                         ;; A sublist
;;;                      ("name of item3" . "http://www.ack.com/")
;;;                     ))
;;; )  ; end of hotlist
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'w3-vars)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hotlist Handling Code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar w3-html-bookmarks nil)

(defun w3-hotlist-break-shit ()
  (let ((todo '(w3-hotlist-apropos
		w3-hotlist-delete
		w3-hotlist-rename-entry
		w3-hotlist-append
		w3-use-hotlist
		w3-hotlist-add-document
		w3-hotlist-add-document-at-point
		))
	(cur nil))
    (while todo
      (setq cur (car todo)
	    todo (cdr todo))
      (fset cur
	    (`
	     (lambda (&rest ignore)
	       (interactive)
	       (error "Sorry, `%s' does not work with html bookmarks"
		      (quote (, cur)))))))))

;;;###autoload
(defun w3-read-html-bookmarks (fname)
  "Import an HTML file into the Emacs-w3 format."
  (interactive "fBookmark file: ")
  (if (not (file-readable-p fname))
      (error "Can not read %s..." fname))
  (save-excursion
    (set-buffer (get-buffer-create " *bookmark-work*"))
    (erase-buffer)
    (insert-file-contents fname)
    (let* ((w3-debug-html nil)
	   (bkmarks nil)
	   (parse (w3-parse-buffer (current-buffer))))
      (setq parse w3-last-parse-tree
	    bkmarks (nreverse (w3-grok-html-bookmarks parse))
	    w3-html-bookmarks bkmarks)))
  (w3-hotlist-break-shit))

(eval-when-compile
  (defvar cur-stack nil)
  (defvar cur-title nil)
  (defmacro push-new-menu ()
    '(setq cur-stack (cons (list "") cur-stack)))
  
  (defmacro push-new-item (title href)
    (` (setcar cur-stack (cons (vector (, title) (list 'w3-fetch (, href)) t)
			       (car cur-stack)))))
  ;;(` (setcar cur-stack (cons (cons (, title) (, href)) (car cur-stack)))))
  
  (defmacro finish-submenu ()
    '(let ((x (nreverse (car cur-stack)))
	   (y (pop cur-title)))
       (while (string= y "")
	 (setq y (pop cur-title)))
       (and x (setcar x y))
       (setq cur-stack (cdr cur-stack))
       (if cur-stack
	   (setcar cur-stack (cons x (car cur-stack)))
	 (setq cur-stack (list x)))))
  )

(defun w3-grok-html-bookmarks-internal (tree)
  (let (node tag content args)
    (while tree
      (setq node (car tree)
	    tree (cdr tree)
	    tag (and (listp node) (nth 0 node))
	    args (and (listp node) (nth 1 node))
	    content (and (listp node) (nth 2 node)))
      (cond
       ((eq tag 'hr)
	(setq cur-title '("------")))
       ((eq tag 'title)
	(setq cur-title (list (w3-normalize-spaces (car content))))
	(w3-grok-html-bookmarks-internal content))
       ((memq tag '(dl ol ul))
	(push-new-menu)
	(w3-grok-html-bookmarks-internal content)
	(finish-submenu))
       ((and (memq tag '(dt li p))
	     (stringp (car content)))
	(setq cur-title (cons (w3-normalize-spaces (car content))
			      cur-title)))
       ((and (eq tag 'a)
	     (stringp (car-safe content))
	     (cdr-safe (assq 'href args)))
	(push-new-item (w3-normalize-spaces (car-safe content))
		       (cdr-safe (assq 'href args))))
       (content
	(w3-grok-html-bookmarks-internal content))))))

(defun w3-grok-html-bookmarks (chunk)
  (let (
	cur-title
	cur-stack
	)
    (w3-grok-html-bookmarks-internal chunk)
    (reverse (car cur-stack))))

;;;###autoload
(defun w3-hotlist-apropos (regexp)
  "Show hotlist entries matching REGEXP."
  (interactive "sW3 Hotlist Apropos (regexp): ")
  (or w3-setup-done (w3-do-setup))
  (let ((save-buf (get-buffer "Hotlist")) ; avoid killing this
	(w3-hotlist
	 (apply
	  'nconc
	  (mapcar
	   (function
	    (lambda (entry)
	      (if (or (string-match regexp (car entry))
		      (string-match regexp (car (cdr entry))))
		  (list entry))))
	   w3-hotlist))))
    (if (not w3-hotlist)
	(message "No w3-hotlist entries match \"%s\"" regexp)
      (and save-buf (save-excursion
		      (set-buffer save-buf)
		      (rename-buffer (concat "Hotlist during " regexp))))
      (unwind-protect
	  (let ((w3-reuse-buffers 'no))
	    (w3-show-hotlist)
	    (rename-buffer (concat "Hotlist \"" regexp "\""))
	    (url-set-filename url-current-object (concat "hotlist/" regexp)))
	(and save-buf (save-excursion
			(set-buffer save-buf)
			(rename-buffer "Hotlist")))))))

;;;###autoload
(defun w3-hotlist-refresh ()
  "Reload the default hotlist file into memory"
  (interactive)
  (if (not w3-setup-done) (w3-do-setup))
  (w3-parse-hotlist))

(defun w3-delete-from-alist (x alist)
  ;; Remove X from ALIST, return new alist
  (if (eq (assoc x alist) (car alist)) (cdr alist)
    (delq (assoc x alist) alist)))

;;;###autoload
(defun w3-hotlist-delete ()
  "Deletes a document from your hotlist file"
  (interactive)
  (save-excursion
    (if (not w3-hotlist) (message "No hotlist in memory!")
      (if (not (file-exists-p w3-hotlist-file))
	  (message "Hotlist file %s does not exist." w3-hotlist-file)
	(let* ((completion-ignore-case t)
	       (title (car (assoc (completing-read "Delete Document: "
						   w3-hotlist nil t)
				  w3-hotlist)))
	       (case-fold-search nil)
	       (buffer (get-buffer-create " *HOTW3*")))
	  (and (string= title "") (error "No document specified."))
	  (set-buffer buffer)
	  (erase-buffer)
	  (insert-file-contents w3-hotlist-file)
	  (goto-char (point-min))
	  (if (re-search-forward (concat "^" (regexp-quote title) "\r*$")
				 nil t)
	      (let ((make-backup-files nil)
		    (version-control nil)
		    (require-final-newline t))
		(previous-line 1)
		(beginning-of-line)
		(delete-region (point) (progn (forward-line 2) (point)))
		(write-file w3-hotlist-file)
		(setq w3-hotlist (w3-delete-from-alist title w3-hotlist))
		(kill-buffer (current-buffer))
		(w3-hotindex-delete-entry title))
	    (message "%s was not found in %s" title w3-hotlist-file)))))))

;;;###autoload
(defun w3-hotlist-rename-entry (title)
  "Rename a hotlist item"
  (interactive (list (let ((completion-ignore-case t))
		       (completing-read "Rename entry: " w3-hotlist nil t))))
  (cond					; Do the error handling first
   ((string= title "") (error "No document specified!"))
   ((not w3-hotlist) (error "No hotlist in memory!"))
   ((not (file-exists-p (expand-file-name w3-hotlist-file)))
    (error "Hotlist file %s does not exist." w3-hotlist-file))
   ((not (file-readable-p (expand-file-name w3-hotlist-file)))
    (error "Hotlist file %s exists, but is unreadable." w3-hotlist-file)))
  (save-excursion
    (let ((obj (assoc title w3-hotlist))
	  (used (mapcar 'car w3-hotlist))
	  (buff (get-buffer-create " *HOTW3*"))
	  (new nil)
	  )
      (while (or (null new) (member new used))
	(setq new (read-string "New name: ")))
      (set-buffer buff)
      (erase-buffer)
      (insert-file-contents (expand-file-name w3-hotlist-file))
      (goto-char (point-min))
      (if (re-search-forward (concat "^" (regexp-quote title) "$") nil t)
	  (let ((make-backup-files nil)
		(version-control nil)
		(require-final-newline t))
	    (previous-line 1)
	    (beginning-of-line)
	    (delete-region (point) (progn (forward-line 2) (point)))
	    (insert (format "%s %s\n%s\n" (nth 1 obj) (current-time-string)
			    new))
	    (setq w3-hotlist (cons (list new (nth 1 obj))
				   (w3-delete-from-alist title w3-hotlist)))
	    (write-file w3-hotlist-file)
 	    (w3-hotindex-rename-entry title new)
	    (kill-buffer (current-buffer)))
	(message "%s was not found in %s" title w3-hotlist-file)))))

;;;###autoload
(defun w3-hotlist-append (fname)
  "Append a hotlist to the one in memory"
  (interactive "fAppend hotlist file: ")
  (let ((x w3-hotlist))
    (w3-parse-hotlist fname)
    (setq w3-hotlist (nconc x w3-hotlist))))

(defun w3-hotlist-parse-old-mosaic-format ()
  (let (cur-link cur-alias)
    (while (re-search-forward "^\n" nil t) (replace-match ""))
    (goto-line 3)
    (while (not (eobp))
      (re-search-forward "^[^ ]*" nil t)
      (setq cur-link (buffer-substring (match-beginning 0) (match-end 0)))
      (setq cur-alias (buffer-substring (progn
					  (forward-line 1)
					  (beginning-of-line)
					  (point))
					(progn
					  (end-of-line)
					  (point))))
      (if (not (equal cur-alias ""))
	  (setq w3-hotlist (cons (list cur-alias cur-link) w3-hotlist))))))

;;;###autoload
(defun w3-parse-hotlist (&optional fname)
  "Read in the hotlist specified by FNAME"
  (if (not fname) (setq fname w3-hotlist-file))
  (setq w3-hotlist nil)
  (if (not (file-exists-p fname))
      (message "%s does not exist!" fname)
    (let* ((old-buffer (current-buffer))
	   (buffer (get-buffer-create " *HOTW3*"))
	   (case-fold-search t))
      (set-buffer buffer)
      (erase-buffer)
      (insert-file-contents fname)
      (goto-char (point-min))
      (cond
       ((looking-at "ncsa-xmosaic-hotlist-format-1");; Old-style NCSA Mosaic
	(w3-hotlist-parse-old-mosaic-format))
       ((or (looking-at "<!DOCTYPE")	; Some HTML style, including netscape
	    (re-search-forward "<a[ \n]+href" nil t))
	(w3-read-html-bookmarks fname))
       (t
	(message "Cannot determine format of hotlist file: %s" fname)))
      (set-buffer-modified-p nil)
      (kill-buffer buffer)
      (set-buffer old-buffer))))

;;;###autoload
(defun w3-use-hotlist ()
  "Possibly go to a link in your W3/Mosaic hotlist.
This is part of the emacs World Wide Web browser.  It will prompt for
one of the items in your 'hotlist'.  A hotlist is a list of often
visited or interesting items you have found on the World Wide Web."
  (interactive)
  (if (not w3-setup-done) (w3-do-setup))
  (if (not w3-hotlist) (message "No hotlist in memory!")
    (let* ((completion-ignore-case t)
	   (url (car (cdr (assoc
			   (completing-read "Goto Document: " w3-hotlist nil t)
			   w3-hotlist)))))
      (if (string= "" url) (error "No document specified!"))
      (w3-fetch url))))

;;;###autoload
(defun w3-hotlist-add-document-at-point (pref-arg)
  "Add the document pointed to by the hyperlink under point to the hotlist."
  (interactive "P")
  (let ((url (w3-view-this-url t))
	(widget (widget-at (point)))
	(title nil))
    (or url (error "No link under point."))
    (if (and (widget-get widget :from)
	     (widget-get widget :to))
	(setq title (buffer-substring (widget-get widget :from)
				      (widget-get widget :to))))
    (w3-hotlist-add-document pref-arg (or title url) url)))

;;;###autoload
(defun w3-hotlist-add-document (pref-arg &optional the-title the-url)
  "Add this documents url to the hotlist"
  (interactive "P")
  (save-excursion
    (let* ((buffer (get-buffer-create " *HOTW3*"))
	   (title (or the-title
		      (and pref-arg (read-string "Title: "))
		      (buffer-name)))
	   (make-backup-files nil)
	   (version-control nil)
	   (require-final-newline t)
	   (url (or the-url (url-view-url t))))
      (if (rassoc (list url) w3-hotlist)
	  (error "That item already in hotlist, use w3-hotlist-rename-entry."))
      (set-buffer buffer)
      (erase-buffer)
      (setq w3-hotlist (cons (list title url) w3-hotlist)
	    url (url-unhex-string url))
      (if (not (file-exists-p w3-hotlist-file))
	  (progn
	    (message "Creating hotlist file %s" w3-hotlist-file)
	    (insert "ncsa-xmosaic-hotlist-format-1\nDefault\n\n")
	    (backward-char 1))
	(progn
	  (insert-file-contents w3-hotlist-file)
	  (goto-char (point-max))
	  (backward-char 1)))
      (insert "\n" url " " (current-time-string) "\n" title)
      (write-file w3-hotlist-file)
      (kill-buffer (current-buffer)))))

(provide 'w3-hot)
