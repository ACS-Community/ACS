;;; url-gopher.el --- Gopher Uniform Resource Locator retrieval code
;; Author: wmperry
;; Created: 1998/12/18 02:19:33
;; Version: 1.1.1.2
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

(defun url-grok-gopher-href (url)
  "Return a list of attributes from a gopher url.  List is of the
type: host port selector-string MIME-type extra-info"
  (let (host				; host name
	port				; Port #
	selector			; String to send to gopher host
	type				; MIME type
	extra				; Extra information
	x				; Temporary storage for host/port
	y				; Temporary storage for selector
	ylen
	)
    (or (string-match "gopher:/*\\([^/]+\\)\\(/*\\)" url)
	(error "Can't understand url %s" url))
    (setq x (url-match url 1)		; The host (and possible port #)
	  ylen (- (length url) (match-end 2))
	  y (if (= ylen 0)		; The selector (and possible type)
		""
		(url-unhex-string (substring url (- ylen)))))

    ;First take care of the host/port/gopher+ information from the url
    ;A + after the port # (host:70+) specifies a gopher+ link
    ;A ? after the port # (host:70?) specifies a gopher+ ask block
    (if (string-match "^\\([^:]+\\):\\([0-9]+\\)\\([?+]*\\)" x)
	(setq host (url-match x 1)
	      port (url-match x 2)
	      extra (url-match x 3))
      (setq host x
	    port "70"
	    extra nil))
    (cond
     ((equal extra "")  (setq extra nil))
     ((equal extra "?") (setq extra 'ask-block))
     ((equal extra "+") (setq extra 'gopher+)))

    ; Next, get the type/get rid of the Mosaic double-typing. Argh.
    (setq x (string-to-char y)		; Get gopher type
	  selector (if (or url-use-hypertext-gopher
			   (< 3 (length y)))
		       y		; Get the selector string
		     (substring y 1 nil))
	  type (cdr (assoc x url-gopher-to-mime)))
    (list host port (or selector "") type extra)))


(defun url-convert-ask-to-form (ask)
  ;; Convert a Gopher+ ASK block into a form.  Returns a string to be
  ;; inserted into a buffer to create the form."
  (let ((form (concat "<form enctype=application/gopher-ask-block\n"
		      "      method=\"GOPHER-ASK\">\n"
		      " <ul plain>\n"))
	(type "")
	(x 0)
	(parms ""))
    (while (string-match "^\\([^:]+\\): +\\(.*\\)" ask)
      (setq parms (url-match ask 2)
	    type (url-strip-leading-spaces (downcase (url-match ask 1)))
	    x (1+ x)
	    ask (substring ask (if (= (length ask) (match-end 0))
				   (match-end 0) (1+ (match-end 0))) nil))
      (cond
       ((string= "note" type) (setq form (concat form parms)))
       ((or (string= "ask" type)
	    (string= "askf" type)
	    (string= "choosef" type))
	(setq parms (url-string-to-tokens parms ?\t)
	      form (format "%s\n<li>%s<input name=\"%d\" value=\"%s\">"
			   form (or (nth 0 parms) "Text:")
			   x (or (nth 1 parms) ""))))
       ((string= "askp" type)
	(setq parms (mapcar 'car (nreverse (url-split parms "\t")))
	      form (format
		    "%s\n<li>%s<input name=\"%d\" type=\"password\" value=\"%s\">"
		    form			   ; Earlier string
		    (or (nth 0 parms) "Password:") ; Prompt
		    x				   ; Name
		    (or (nth 1 parms) "") 	   ; Default value
		    )))
       ((string= "askl" type)
	(setq parms (url-string-to-tokens parms ?\t)
	      form (format "%s\n<li>%s<textarea name=\"%d\">%s</textarea>"
			   form			 ; Earlier string
			   (or (nth 0 parms) "") ; Prompt string
			   x			 ; Name
			   (or (nth 1 parms) "") ; Default value
			   )))
       ((or (string= "select" type)
	    (string= "choose" type))
	(setq parms (url-string-to-tokens parms ?\t)
	      form (format "%s\n<li>%s<select name=\"%d\">" form (car parms) x)
	      parms (cdr parms))
	(if (null parms) (setq parms (list "Yes" "No")))
	(while parms
	  (setq form (concat form "<option>" (car parms) "\n")
		parms (cdr parms)))
	(setq form (concat form "</select>")))))
    (concat form "\n<li><input type=\"SUBMIT\""
	    " value=\"Submit Gopher+ Ask Block\"></ul></form>")))

(defun url-grok-gopher-line ()
  "Return a list of link attributes from a gopher string.  Order is:
title, type, selector string, server, port, gopher-plus?"
  (let (type selector server port gopher+ st nd)
    (beginning-of-line)
    (setq st (point))
    (end-of-line)
    (setq nd (point))
    (save-excursion
      (mapcar (function
	       (lambda (var)
		 (goto-char st)
		 (skip-chars-forward "^\t\n" nd)
		 (set-variable var (buffer-substring st (point)))
		 (setq st (min (point-max) (1+ (point))))))
	      '(type selector server port))
      (setq gopher+ (and (/= (1- st) nd) (buffer-substring st nd)))
      (list type (concat (substring type 0 1) selector) server port gopher+))))

(defun url-format-gopher-link (gophobj)
  ;; Insert a gopher link as an <A> tag
  (let ((title (nth 0 gophobj))
	(ref   (nth 1 gophobj))
	(type  (if (> (length (nth 0 gophobj)) 0)
		   (substring (nth 0 gophobj) 0 1) ""))
	(serv  (nth 2 gophobj))
	(port  (nth 3 gophobj))
	(plus  (nth 4 gophobj))
	(desc  nil))
    (if (and (equal type "")
	     (> (length title) 0))
	(setq type (substring title 0 1)))
    (setq title (and title (substring title 1 nil))
	  title (mapconcat
		 (function
		  (lambda (x)
		    (cond
		     ((= x ?&) "&amp;")
		     ((= x ?<) "&lt;");
		     ((= x ?>) "&gt;");
		     (t (char-to-string x))))) title "")
	  desc (or (cdr (assoc type url-gopher-labels)) "(UNK)"))
    (cond
     ((null ref) "")
     ((equal type "8")
      (format "<LI> %s <A HREF=\"telnet://%s:%s/\">%s</A>\n"
	      desc serv port title))
     ((equal type "T")
      (format "<LI> %s <A HREF=\"tn3270://%s:%s/\">%s</A>\n"
	      desc serv port title))
     (t (format "<LI> %s <A METHODS=%s HREF=\"gopher://%s:%s/%s\">%s</A>\n"
		desc type serv (concat port plus)
		(url-hexify-string ref) title)))))

(defun url-gopher-clean-text (&optional buffer)
  "Decode text transmitted by gopher.
0. Delete status line.
1. Delete `^M' at end of line.
2. Delete `.' at end of buffer (end of text mark).
3. Delete `.' at beginning of line.   (does gopher want this?)"
  (set-buffer (or buffer url-working-buffer))
  ;; Insert newline at end of buffer.
  (goto-char (point-max))
  (if (not (bolp))
      (insert "\n"))
  ;; Delete `^M' at end of line.
  (goto-char (point-min))
  (while (re-search-forward "\r[^\n]*$" nil t)
    (replace-match ""))
;  (goto-char (point-min))
;  (while (not (eobp))
;    (end-of-line)
;    (if (= (preceding-char) ?\r)
;       (delete-char -1))
;    (forward-line 1)
;    )
  ;; Delete `.' at end of buffer (end of text mark).
  (goto-char (point-max))
  (forward-line -1)                     ;(beginning-of-line)
  (while (looking-at "^\\.$")
    (delete-region (point) (progn (forward-line 1) (point)))
    (forward-line -1))
  ;; Replace `..' at beginning of line with `.'.
  (goto-char (point-min))
  ;; (replace-regexp "^\\.\\." ".")
  (while (search-forward "\n.." nil t)
    (delete-char -1))
  )

(defun url-parse-gopher (&optional buffer)
  (save-excursion
    (set-buffer (or buffer url-working-buffer))
    (url-replace-regexp "^\r*$\n" "")
    (url-replace-regexp "^\\.\r*$\n" "")
    (url-gopher-clean-text (current-buffer))
    (goto-char (point-max))
    (skip-chars-backward "\n\r\t ")
    (delete-region (point-max) (point))
    (insert "\n")
    (goto-char (point-min))
    (skip-chars-forward " \t\n")
    (delete-region (point-min) (point))
    (let* ((len (count-lines (point-min) (point-max)))
	   (objs nil)
	   (i 0))
      (while (not (eobp))
	(setq objs (cons (url-grok-gopher-line) objs)
	      i (1+ i))
	(url-lazy-message "Converting gopher listing... %d/%d (%d%%)"
			  i len (url-percentage i len))
						
	(forward-line 1))
      (setq objs (nreverse objs))
      (erase-buffer)
      (insert "<title>"
	      (cond
	       ((or (string= "" (url-filename url-current-object))
		    (string= "1/" (url-filename url-current-object))
		    (string= "1" (url-filename url-current-object)))
		(concat "Gopher root at " (url-host url-current-object)))
	       ((string-match (format "^[%s]+/" url-gopher-types)
			      (url-filename url-current-object))
		(substring (url-filename url-current-object) 2 nil))
	       (t (url-filename url-current-object)))
	      "</title><ol>"
	      (mapconcat 'url-format-gopher-link objs "")
	      "</ol>"))))

(defun url-gopher-retrieve (host port selector &optional wait-for)
  ;; Fetch a gopher object and don't mess with it at all
  (let ((proc (url-open-stream "*gopher*" url-working-buffer
			      host (if (stringp port) (string-to-int port)
				     port)))
	(len nil)
	(parsed nil))
    (url-clear-tmp-buffer)
    (if (> (length selector) 0)
	(setq selector (substring selector 1 nil)))
    (if (not (processp proc))
	nil
      (save-excursion
	(set-process-sentinel proc 'ignore)
	(process-send-string proc (concat selector "\r\n"))
	(while (and (or (not wait-for)
			(progn
			  (goto-char (point-min))
			  (not (re-search-forward wait-for nil t))))
		    (memq (url-process-status proc) '(run open)))
	  (if (not parsed)
	      (cond
	       ((and (eq ?+ (char-after 1))
		     (memq (char-after 2)
			   (list ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9)))
		(setq parsed (copy-marker 2)
		      len (read parsed))
		(delete-region (point-min) parsed))
	       ((and (eq ?+ (char-after 1))
		     (eq ?- (char-after 2)))
		(setq len nil
		      parsed t)
		(goto-char (point-min))
		(delete-region (point-min) (progn
					     (end-of-line)
					     (point))))
	       ((and (eq ?- (char-after 1))
		     (eq ?- (char-after 2)))
		(setq parsed t
		      len nil)
		(goto-char (point-min))
		(delete-region (point-min) (progn
					     (end-of-line)
					     (point))))))
	  (if len (url-lazy-message "Reading... %d of %d bytes (%d%%)"
				    (point-max)
				    len
				    (url-percentage (point-max) len))
	    (url-lazy-message "Read... %d bytes." (point-max)))
	  (url-accept-process-output proc))
	(condition-case ()
	    (url-kill-process proc)
	  (error nil))
	(while (looking-at "\r") (delete-char 1))))))

(defun url-do-gopher-cso-search (descr)
  ;; Do a gopher CSO search and return a plaintext document
  (let ((host (nth 0 descr))
	(port (nth 1 descr))
	(file (nth 2 descr))
	search-type search-term)
    (string-match "search-by=\\([^&]+\\)" file)
    (setq search-type (url-match file 1))
    (string-match "search-term=\\([^&]+\\)" file)
    (setq search-term (url-match file 1))
    (url-gopher-retrieve host port (format "2query %s=%s"
					  search-type search-term) "^[2-9]")
    (goto-char (point-min))
    (url-replace-regexp "^-[0-9][0-9][0-9]:[0-9]*:" "")
    (url-replace-regexp "^[^15][0-9][0-9]:.*" "")
    (url-replace-regexp "^[15][0-9][0-9]:\\(.*\\)" "<H1>\\1</H1>&ensp;<PRE>")
    (goto-char (point-min))
    (insert "<title>Results of CSO search</title>\n"
	    "<h1>" search-type " = " search-term "</h1>\n")
    (goto-char (point-max))
    (insert "</pre>")))

(defun url-do-gopher (descr)
  ;; Fetch a gopher object
  (let ((host (nth 0 descr))
	(port (nth 1 descr))
	(file (nth 2 descr))
	(type (nth 3 descr))
	(extr (nth 4 descr))
	parse-gopher)
    (cond
     ((and				; Gopher CSO search
       (equal type "www/gopher-cso-search")
       (string-match "search-by=" file)) ; With a search term in it
      (url-do-gopher-cso-search descr)
      (setq type "text/html"))
     ((equal type "www/gopher-cso-search") ; Blank CSO search
      (url-clear-tmp-buffer)
      (insert "<html>\n"
	      " <head>\n"
	      "  <title>CSO Search</title>\n"
	      " </head>\n"
	      " <body>\n"
	      "  <div>\n"
	      "   <h1>This is a CSO search</h1>\n"
	      "   <hr>\n"
	      "   <form>\n"
	      "    <ul>\n"
	      "     <li> Search by: <select name=\"search-by\">\n"
	      "                      <option>Name\n"
	      "                      <option>Phone\n"
	      "                      <option>Email\n"
	      "                      <option>Address\n"
	      "                     </select>\n"
	      "     <li> Search for: <input name=\"search-term\">\n"
	      "     <li> <input type=\"submit\" value=\"Submit query\">\n"
	      "    </ul>\n"
	      "   </form>\n"
	      "  </div>\n"
	      " </body>\n"
	      "</html>\n"
	      "<!-- Automatically generated by URL v" url-version " -->\n")
      (setq type "text/html"
	    parse-gopher t))
     ((and
       (equal type "www/gopher-search")	; Ack!  Mosaic-style search href
       (string-match "\t" file))	; and its got a search term in it!
      (url-gopher-retrieve host port file)
      (setq type "www/gopher"
	    parse-gopher t))
     ((and
       (equal type "www/gopher-search")	; Ack!  Mosaic-style search href
       (string-match "\\?" file))	; and its got a search term in it!
      (setq file (concat (substring file 0 (match-beginning 0)) "\t"
			 (substring file (match-end 0) nil)))
      (url-gopher-retrieve host port file)
      (setq type "www/gopher"
	    parse-gopher t))
     ((equal type "www/gopher-search")	; Ack!  Mosaic-style search href
      (setq type "text/html"
	    parse-gopher t)
      (url-clear-tmp-buffer)
      (insert "<html>\n"
	      " <head>\n"
	      "  <title>Gopher Server</title>\n"
	      " </head>\n"
	      " <body>\n"
	      "  <div>\n"
	      "   <h1>Searchable Gopher Index</h1>\n"
	      "   <hr>\n"
	      "   <p>\n"
	      "    Enter the search keywords below\n"
	      "   </p>"
	      "   <form enctype=\"application/x-gopher-query\">\n"
	      "    <input name=\"internal-gopher\">\n"
	      "   </form>\n"
	      "   <hr>\n"
	      "  </div>\n"
	      " </body>\n"
	      "</html>\n"
	      "<!-- Automatically generated by URL v" url-version " -->\n"))
     ((null extr)			; Normal Gopher link
      (url-gopher-retrieve host port file)
      (setq parse-gopher t))
     ((eq extr 'gopher+)		; A gopher+ link
      (url-gopher-retrieve host port (concat file "\t+"))
      (setq parse-gopher t))
     ((eq extr 'ask-block)		; A gopher+ interactive query
      (url-gopher-retrieve host port (concat file "\t!")) ; Fetch the info
      (goto-char (point-min))
      (cond
       ((re-search-forward "^\\+ASK:[ \t\r]*" nil t) ; There is an ASK
	(let ((x (buffer-substring (1+ (point))
				   (or (re-search-forward "^\\+[^:]+:" nil t)
				       (point-max)))))
	  (erase-buffer)
	  (insert (url-convert-ask-to-form x))
	  (setq type "text/html" parse-gopher t)))
       (t (setq parse-gopher t)))))
    (if (or (equal type "www/gopher")
	    (equal type "text/plain")
	    (equal file "")
	    (equal type "text/html"))
	(url-gopher-clean-text))
    (if (and parse-gopher (or (equal type "www/gopher")
			      (equal file "")))
	(progn
	  (url-parse-gopher)
	  (setq type "text/html"
		url-current-mime-viewer (mm-mime-info type nil 5))))
    (setq url-current-mime-type (or type "text/plain")
	  url-current-mime-viewer (mm-mime-info type nil 5))))

(defun url-gopher (url)
  ;; Handle gopher URLs
  (let ((descr (url-grok-gopher-href url)))
    (cond
     ((or (not (member (nth 1 descr) url-bad-port-list))
	  (funcall
	   url-confirmation-func
	   (format "Warning!  Trying to connect to port %s - continue? "
		   (nth 1 descr))))
      (if url-use-hypertext-gopher
	  (url-do-gopher descr)
	(gopher-dispatch-object (vector (if (= 0 (length (nth 2 descr)))
					    ?1
					  (string-to-char (nth 2 descr)))
					(nth 2 descr) (nth 2 descr)
					(nth 0 descr)
					(string-to-int (nth 1 descr)))
				(current-buffer))))
     (t
      (ding)
      (url-warn 'security "Aborting connection to bad port...")))))

(provide 'url-gopher)
