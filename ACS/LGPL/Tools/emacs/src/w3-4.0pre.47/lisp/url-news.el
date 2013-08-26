;;; url-news.el --- News Uniform Resource Locator retrieval code
;; Author: wmperry
;; Created: 1998/12/22 20:41:01
;; Version: 1.2
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
(require 'url-parse)

(defgroup url-news nil
  "News related options"
  :group 'url)

(defcustom url-news-use-article-mode nil
  "*Whether to use Gnus' article mode for displaying news articles."
  :type 'boolean
  :group 'url-news)

(defun url-format-news ()
  (url-clear-tmp-buffer)
  (insert "HTTP/1.0 200 Retrieval OK\r\n"
 	  (save-excursion
 	    (set-buffer nntp-server-buffer)
 	    (buffer-string)))
  (url-parse-mime-headers)
  (let* ((from  (cdr (assoc "from" url-current-mime-headers)))
	 (qfrom (if from (url-insert-entities-in-string from) nil))
	 (subj  (cdr (assoc "subject" url-current-mime-headers)))
	 (qsubj (if subj (url-insert-entities-in-string subj) nil))
	 (org   (cdr (assoc "organization" url-current-mime-headers)))
	 (qorg  (if org (url-insert-entities-in-string org) nil))
	 (typ   (or (cdr (assoc "content-type" url-current-mime-headers))
		    "text/plain"))
	 (inhibit-read-only t)
	 (qgrps (mapcar 'car
			(url-split
			 (url-insert-entities-in-string
			  (or (cdr (assoc "newsgroups" 
					  url-current-mime-headers))
			      ""))
			 "[ \t\n,]+")))
	 (qrefs (delete "" 
			(mapcar
			 'url-insert-entities-in-string
			 (mapcar 'car
				 (url-split
				  (or (cdr (assoc "references" 
						  url-current-mime-headers))
				      "")
				  "[ \t,\n<>]+")))))
	 (date  (cdr (assoc "date" url-current-mime-headers))))
    (if (or (not (string-match "text/" typ))
 	    (string-match "text/html" typ))
 	nil				; Let natural content-type take over
      (if (and (fboundp 'gnus-article-mode)
	       url-news-use-article-mode)
	  (progn
	    (kill-buffer (current-buffer))
	    (set-buffer (get-buffer-create "Emacs/W3 News"))
	    (erase-buffer)
	    (insert
	     (save-excursion
	       (set-buffer nntp-server-buffer)
	       (save-restriction
		 (widen)
		 (buffer-string))))
	    (let ((gnus-article-buffer (current-buffer))
		  (gnus-article-current (cons "url"
					      (car (cdr (current-time))))))
	      (gnus-article-mode)
	      (run-hooks 'gnus-article-display-hook))
	    (goto-char (point-min))
	    (display-buffer (current-buffer)))
	(insert "<html>\n"
		" <head>\n"
		"  <title>" qsubj "</title>\n"
		"  <link rev=\"made\" href=\"mailto:" qfrom "\">\n"
		" </head>\n"
		" <body>\n"
		"  <div>\n"
		"   <h1 align=center>" qsubj "</h1>\n"
		"   <p role=\"headers\">\n"
		"    <b>From</b>: " qfrom "<br>\n"
		"    <b>Newsgroups</b>: "
		(mapconcat
		 (function
		  (lambda (grp)
		    (concat "<a href=\"" grp "\">" grp "</a>"))) qgrps ", ")
		"<br>\n"
		(if org
		    (concat
		     "    <b>Organization</b>: <i> " qorg "</i> <br>\n")
		  "")
		"    <b>Date</b>: <date> " date "</date> <br>\n"
		"   </p> <hr>\n"
		(if (null qrefs)
		    ""
		  (concat
		   "   <p>References\n"
		   "    <ol>\n"
		   (mapconcat
		    (function
		     (lambda (ref)
		       (concat "     <li> <a href=\"" ref "\"> " 
			       ref "</a></li>\n")))
		    qrefs "")
		   "    </ol>\n"
		   "   </p>\n"
		   "   <hr>\n"))
		"   <ul plain>\n"
		"    <li><a href=\"newspost:disfunctional\"> "
		"Post to this group </a></li>\n"
		"    <li><a href=\"mailto:" qfrom "\"> Reply to " qfrom
		"</a></li>\n"
		"   </ul>\n"
		"   <hr>"
		"   <pre>\n")
	(let ((s (buffer-substring (point) (point-max))))
	  (delete-region (point) (point-max))
	  (insert (url-insert-entities-in-string s)))
	(goto-char (point-max))
	(setq url-current-mime-type "text/html"
	      url-current-mime-viewer (mm-mime-info url-current-mime-type nil 5))
	(let ((x (assoc "content-type" url-current-mime-headers)))
	  (if x
	      (setcdr x "text/html")
	    (setq url-current-mime-headers (cons (cons "content-type"
						       "text/html")
						 url-current-mime-headers))))
	(insert "\n"
		"   </pre>\n"
		"  </div>\n"
		" </body>\n"
		"</html>\n"
		"<!-- Automatically generated by URL/" url-version
		"-->")))))

(defun url-check-gnus-version ()
  (require 'nntp)
  (condition-case ()
      (require 'gnus)
    (error (setq gnus-version "GNUS not found")))
  (if (or (not (boundp 'gnus-version))
	  (and (fboundp 'gnus-continuum-version)
	       (>= (gnus-continuum-version gnus-version) 5))
	  (string-match "v5.[.0-9]+$" gnus-version)
	  (string-match "Red" gnus-version)
	  (string-match "Quassia" gnus-version))
      nil
    (url-warn 'url (concat
		    "The version of GNUS found on this system is too old and does\n"
		    "not support the necessary functionality for the URL package.\n"
		    "Please upgrade to version 5.x of GNUS.  This is bundled by\n"
		    "default with Emacs 19.30 and XEmacs 19.14 and later.\n\n"
		    "This version of GNUS is: " gnus-version "\n"))
    (fset 'url-news 'url-news-version-too-old))
  (fset 'url-check-gnus-version 'ignore))

(defun url-news-version-too-old (article)
  (set-buffer (get-buffer-create url-working-buffer))
  (setq url-current-mime-headers '(("content-type" . "text/html"))
	url-current-mime-type "text/html")
  (insert "<html>\n"
	  " <head>\n"
	  "  <title>News Error</title>\n"
	  " </head>\n"
	  " <body>\n"
	  "  <h1>News Error - too old</h1>\n"
	  "  <p>\n"
	  "   The version of GNUS found on this system is too old and does\n"
	  "   not support the necessary functionality for the URL package.\n"
	  "   Please upgrade to version 5.x of GNUS.  This is bundled by\n"
	  "   default with Emacs 19.30 and XEmacs 19.14 and later.\n\n"
	  "   This version of GNUS is: " gnus-version "\n"
	  "  </p>\n"
	  " </body>\n"
	  "</html>\n"))

(defun url-news-open-host (host port user pass)
  (if (fboundp 'nnheader-init-server-buffer)
      (nnheader-init-server-buffer))
  (nntp-open-server host (list (string-to-int port)))
  (if (and user pass)
      (progn
	(nntp-send-command "^.*\r?\n" "AUTHINFO USER" user)
	(nntp-send-command "^.*\r?\n" "AUTHINFO PASS" pass)
	(if (not (nntp-server-opened host))
	    (url-warn 'url (format "NNTP authentication to `%s' as `%s' failed"
				   host user))))))

(defun url-news-fetch-article-number (newsgroup article)
  (nntp-request-group newsgroup)
  (nntp-request-article article))

(defun url-news-fetch-message-id (host port message-id)
  (if (eq ?> (aref message-id (1- (length message-id))))
      nil
    (setq message-id (concat "<" message-id ">")))
  (if (nntp-request-article message-id)
      (url-format-news)
    (set-buffer (get-buffer-create url-working-buffer))
    (setq url-current-can-be-cached nil)
    (insert "<html>\n"
	    " <head>\n"
	    "  <title>Error</title>\n"
	    " </head>\n"
	    " <body>\n"
	    "  <div>\n"
	    "   <h1>Error requesting article...</h1>\n"
	    "   <p>\n"
	    "    The status message returned by the NNTP server was:"
	    "<br><hr>\n"
	    "    <xmp>\n"
	    (nntp-status-message)
	    "    </xmp>\n"
	    "   </p>\n"
	    "   <p>\n"
	    "    If you If you feel this is an error, <a href=\""
	    "mailto:" url-bug-address "\">send me mail</a>\n"
	    "   </p>\n"
	    "  </div>\n"
	    " </body>\n"
	    "</html>\n"
	    "<!-- Automatically generated by URL v" url-version " -->\n"
	    )))

(defun url-news-fetch-newsgroup (newsgroup host)
  (if (string-match "^/+" newsgroup)
      (setq newsgroup (substring newsgroup (match-end 0))))
  (if (string-match "/+$" newsgroup)
      (setq newsgroup (substring newsgroup 0 (match-beginning 0))))

  ;; This saves a bogus 'Untitled' buffer by Emacs-W3
  (kill-buffer url-working-buffer)
  
  ;; This saves us from checking new news if GNUS is already running
  (if (or (not (get-buffer gnus-group-buffer))
	  (save-excursion
	    (set-buffer gnus-group-buffer)
	    (not (eq major-mode 'gnus-group-mode))))
      (gnus))
  (set-buffer gnus-group-buffer)
  (goto-char (point-min))
  (gnus-group-read-ephemeral-group newsgroup (list 'nntp host)
				   nil
				   (cons (current-buffer) 'browse)))
  
(defun url-news (article)
  ;; Find a news reference
  (url-check-gnus-version)
  (let* ((urlobj (url-generic-parse-url article))
	 (host (or (url-host urlobj) url-news-server))
	 (port (or (url-port urlobj)
		   (cdr-safe (assoc "news" url-default-ports))))
	 (article-brackets nil)
	 (article (url-filename urlobj)))
    (url-news-open-host host port (url-user urlobj) (url-password urlobj))
    (cond
     ((string-match "@" article)	; Its a specific article
      (url-news-fetch-message-id host port article))
     ((string= article "")		; List all newsgroups
      (gnus)
      (kill-buffer url-working-buffer))
     (t					; Whole newsgroup
      (url-news-fetch-newsgroup article host)))))

(defun url-nntp (url)
  ;; Find a news reference
  (url-check-gnus-version)
  (let* ((urlobj (url-generic-parse-url url))
	 (host (or (url-host urlobj) url-news-server))
	 (port (or (url-port urlobj)
		   (cdr-safe (assoc "nntp" url-default-ports))))
	 (article-brackets nil)
	 (article (url-filename urlobj)))
    (url-news-open-host host port (url-user urlobj) (url-password urlobj))
    (cond
     ((string-match "@" article)	; Its a specific article
      (url-news-fetch-message-id host port article))
     ((string-match "/\\([0-9]+\\)$" article)
      (url-news-fetch-article-number (substring article 0
						(match-beginning 0))
				     (match-string 1 article)))
						
     ((string= article "")		; List all newsgroups
      (gnus)
      (kill-buffer url-working-buffer))
     (t					; Whole newsgroup
      (url-news-fetch-newsgroup article host)))))

(provide 'url-news)
