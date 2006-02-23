;;; url-mail.el --- Mail Uniform Resource Locator retrieval code
;; Author: wmperry
;; Created: 1999/06/28 01:46:57
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

(defmacro url-mailserver-skip-chunk ()
  (` (while (and (not (looking-at "/"))
		 (not (eobp)))
       (forward-sexp 1))))

;;;###autoload
(defun url-mail (&rest args)
  (interactive "P")
  (if (fboundp 'message-mail)
      (apply 'message-mail args)
    (or (apply 'mail args)
	(error "Mail aborted"))))

(defun url-mail-goto-field (field)
  (if (not field)
      (goto-char (point-max))
    (let ((dest nil)
	  (lim nil)
	  (case-fold-search t))
      (save-excursion
	(goto-char (point-min))
	(if (re-search-forward (regexp-quote mail-header-separator) nil t)
	    (setq lim (match-beginning 0)))
	(goto-char (point-min))
	(if (re-search-forward (concat "^" (regexp-quote field) ":") lim t)
	    (setq dest (match-beginning 0))))
      (if dest
	  (progn
	    (goto-char dest)
	    (end-of-line))
	(goto-char lim)
	(insert (capitalize field) ": ")
	(save-excursion
	  (insert "\n"))))))
  
(defun url-mailto (url)
  ;; Send mail to someone
  (if (string-match "mailto:/*\\(.*\\)" url)
      (when (eql ?/ (1- (match-beginning 1)))
	(w3-debug-html :style "mailto URL with slashes before mailbox."))
    (error "Malformed mailto link: %s" url))
  (setq url (substring url (match-beginning 1) nil))
  (if (get-buffer url-working-buffer)
      (kill-buffer url-working-buffer))
  (let (to args source-url subject func headers-start)
    (if (string-match (regexp-quote "?") url)
	(setq headers-start (match-end 0)
	      to (url-unhex-string (substring url 0 (match-beginning 0)))
	      args (url-parse-query-string
		    (substring url headers-start nil) t))
      (setq to (url-unhex-string url)))
    (setq source-url (url-view-url t))
    (if (and url-request-data (not (assoc "subject" args)))
	(setq args (cons (list "subject"
			       (concat "Automatic submission from "
				       url-package-name "/"
				       url-package-version)) args)))
    (if (and source-url (not (assoc "x-url-from" args)))
	(setq args (cons (list "x-url-from" source-url) args)))

    (if (assoc "to" args)
	(push to (cdr (assoc "to" args)))
      (setq args (cons (list "to" to) args)))
    (setq subject (cdr-safe (assoc "subject" args)))
    (if (fboundp url-mail-command) (funcall url-mail-command) (mail))
    (while args
      (if (string= (caar args) "body")
	  (progn
	    (goto-char (point-max))
	    (insert (mapconcat 'identity (cdar args) "\n")))
	(url-mail-goto-field (caar args))
	(setq func (intern-soft (concat "mail-" (caar args))))
	(insert (mapconcat 'identity (cdar args) ", ")))
      (setq args (cdr args)))
    (url-mail-goto-field "X-Mailer")
    (insert url-package-name "/" url-package-version)
    (if (not url-request-data)
	(if subject
	    (url-mail-goto-field nil)
	  (url-mail-goto-field "subject"))
      (if url-request-extra-headers
	  (mapconcat
	   (function
	    (lambda (x)
	      (url-mail-goto-field (car x))
	      (insert (cdr x))))
	   url-request-extra-headers ""))
      (goto-char (point-max))
      (insert url-request-data)
      (mail-send-and-exit nil))))

(defun url-mailserver (url)
  ;; Send mail to someone, much cooler/functional than mailto
  (if (get-buffer url-working-buffer)
      (kill-buffer url-working-buffer))
  (set-buffer (get-buffer-create " *mailserver*"))
  (erase-buffer)
  (insert url)
  (goto-char (point-min))
  (set-syntax-table url-mailserver-syntax-table)
  (skip-chars-forward "^:")		; Get past mailserver
  (skip-chars-forward ":")		; Get past :
  ;; Handle some ugly malformed URLs, but bitch about it.
  (if (looking-at "/")
      (progn
	(url-warn 'url "Invalid mailserver URL... attempting to cope.")
	(skip-chars-forward "/")))
  
  (let ((save-pos (point))
	(url (url-view-url t))
	(rfc822-addr nil)
	(subject nil)
	(body nil))
    (url-mailserver-skip-chunk)
    (setq rfc822-addr (buffer-substring save-pos (point)))
    (forward-char 1)
    (setq save-pos (point))
    (url-mailserver-skip-chunk)
    (setq subject (buffer-substring save-pos (point)))
    (if (not (eobp))
	(progn				; There is some text to use
	  (forward-char 1)		; as the body of the message
	  (setq body (buffer-substring (point) (point-max)))))
    (if (fboundp url-mail-command) (funcall url-mail-command) (mail))
    (url-mail-goto-field "to")
    (insert rfc822-addr)
    (if (and url (not (string= url "")))
	(progn
	  (url-mail-goto-field "X-URL-From")
	  (insert url)))
    (url-mail-goto-field "X-Mailer")
    (insert url-package-name "/" url-package-version)
    (url-mail-goto-field "subject")
    ;; Massage the subject from URLEncoded garbage
    ;; Note that we do not allow any newlines in the subject,
    ;; as recommended by the Internet Draft on the mailserver
    ;; URL - this means the document author cannot spoof additional
    ;; header lines, which is a 'Good Thing'
    (if subject
	(progn
	  (setq subject (url-unhex-string subject))
	  (let ((x (1- (length subject)))
		(y 0))
	    (while (<= y x)
	      (if (memq (aref subject y) '(?\r ?\n))
		  (aset subject y ? ))
	      (setq y (1+ y))))))
    (insert subject)
    (if url-request-extra-headers
	(progn
	  (goto-char (point-min))
	  (insert
	   (mapconcat
	    (function
	     (lambda (x)
	       (url-mail-goto-field (car x))
	       (insert (cdr x))))
	    url-request-extra-headers ""))))
    (goto-char (point-max))
    ;; Massage the body from URLEncoded garbage
    (if body
	(let ((x (1- (length body)))
	      (y 0))
	  (while (<= y x)
	    (if (= (aref body y) ?/)
		(aset body y ?\n))
	    (setq y (1+ y)))
	  (setq body (url-unhex-string body))))
    (and body (insert body))
    (and url-request-data (insert url-request-data))
    (if (and (or body url-request-data)
	     (funcall url-confirmation-func
		      (concat "Send message to " rfc822-addr "? ")))
	(mail-send-and-exit nil))))    

(provide 'url-mail)
