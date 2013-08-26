;;; url-http.el --- HTTP Uniform Resource Locator retrieval code
;; Author: wmperry
;; Created: 1999/11/09 19:56:32
;; Version: 1.5
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
(require 'url-cookie)
(require 'timezone)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Support for HTTP/1.0 MIME messages
;;; ----------------------------------
;;; These functions are the guts of the HTTP/0.9 and HTTP/1.0 transfer
;;; protocol, handling access authorization, format negotiation, the
;;; whole nine yards.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun url-parse-viewer-types ()
  "Create a string usable for an Accept: header from mm-mime-data"
  (let ((tmp (append mm-mime-data mm-mime-data-default))
	label mjr mnr cur-mnr (str ""))
    (while tmp
      (setq mnr (cdr (car tmp))
	    mjr (car (car tmp))
	    tmp (cdr tmp))
      (while mnr
	(setq cur-mnr (car mnr)
	      label (concat mjr "/" (if (string= ".*" (car cur-mnr))
					"*"
				      (car cur-mnr))))
	(cond
	 ((string-match (regexp-quote label) str) nil)
	 ((> (+ (% (length str) 60)
		(length (concat ", " mjr "/" (car cur-mnr)))) 60)
	  (setq str (format "%s\r\nAccept: %s" str label)))
	 (t
	  (setq str (format "%s, %s" str label))))
	(setq mnr (cdr mnr))))
    (substring str 2 nil)))

(defun url-create-multipart-request (file-list)
  "Create a multi-part MIME request for all files in FILE-LIST"
  (let ((separator (current-time-string))
	(content "message/http-request")		   
	(ref-url nil))
    (setq separator
	  (concat "separator-"
		  (mapconcat
		   (function
		    (lambda (char)
		      (if (memq char url-mime-separator-chars)
			  (char-to-string char) ""))) separator "")))
    (cons separator
	  (concat
	   (mapconcat
	    (function
	     (lambda (file)
	       (concat "--" separator "\nContent-type: " content "\n\n"
		       (url-create-mime-request file ref-url)))) file-list
		       "\n")
	   "--" separator))))

(defun url-create-message-id ()
  "Generate a string suitable for the Message-ID field of a request"
  (concat "<" (url-create-unique-id) "@" (system-name) ">"))

(defun url-create-unique-id ()
  ;; Generate unique ID from user name and current time.
  (let* ((date (current-time-string))
	 (name (user-login-name))
	 (dateinfo (and date (timezone-parse-date date)))
	 (timeinfo (and date (timezone-parse-time (aref dateinfo 3)))))
    (if (and dateinfo timeinfo)
	(concat (upcase name) "."
		(aref dateinfo 0)	; Year
		(aref dateinfo 1)	; Month
		(aref dateinfo 2)	; Day
		(aref timeinfo 0)	; Hour
		(aref timeinfo 1)	; Minute 
		(aref timeinfo 2)	; Second
		)
      (error "Cannot understand current-time-string: %s." date))
    ))

(defun url-http-user-agent-string ()
  (if (or (eq url-privacy-level 'paranoid)
	  (and (listp url-privacy-level)
	       (memq 'agent url-privacy-level)))
      ""
    (format "User-Agent: %s/%s URL/%s%s\r\n"
	    url-package-name url-package-version
	    url-version
	    (cond
	     ((and url-os-type url-system-type)
	      (concat " (" url-os-type "; " url-system-type ")"))
	     ((or url-os-type url-system-type)
	      (concat " (" (or url-system-type url-os-type) ")"))
	     (t "")))))

(defun url-create-mime-request (fname ref-url)
  "Create a MIME request for fname, referred to by REF-URL."
  (let* ((extra-headers)
	 (request nil)
	 (url (url-view-url t))
	 (no-cache (cdr-safe (assoc "Pragma" url-request-extra-headers)))
	 (proxy-auth (if (or (cdr-safe (assoc "Proxy-Authorization"
					      url-request-extra-headers))
			     (not (boundp 'proxy-info))
			     (not url-using-proxy))
			 nil
		       (let ((url-basic-auth-storage
			      url-proxy-basic-authentication))
			 (url-get-authentication url-using-proxy nil 'any nil))))
	 (proxy-obj (if (and (boundp 'proxy-info) proxy-info)
			(url-generic-parse-url proxy-info)))
	 (real-fname (if proxy-obj (url-filename proxy-obj) fname))
	 (host (or (and proxy-obj (url-host proxy-obj))
		   (url-host url-current-object)))
	 (auth (if (cdr-safe (assoc "Authorization" url-request-extra-headers))
		   nil
		 (url-get-authentication (or
					  (and (boundp 'proxy-info)
					       proxy-info)
					  url) nil 'any nil))))
    (setq no-cache (and no-cache (string-match "no-cache" no-cache)))
    (if auth
	(setq auth (concat "Authorization: " auth "\r\n")))
    (if proxy-auth
	(setq proxy-auth (concat "Proxy-Authorization: " proxy-auth "\r\n")))

    (if (and ref-url (stringp ref-url) (or (string= ref-url "file:nil")
					   (string= ref-url "")))
	(setq ref-url nil))

    (if (or (memq url-privacy-level '(low high paranoid))
	    (and (listp url-privacy-level)
		 (memq 'lastloc url-privacy-level)))
	(setq ref-url nil))

    (setq extra-headers (mapconcat
			 (function (lambda (x)
				     (concat (car x) ": " (cdr x))))
			 url-request-extra-headers "\r\n"))
    (if (not (equal extra-headers ""))
	(setq extra-headers (concat extra-headers "\r\n")))
    (setq request
	  (format
	   (concat
	    "%s %s HTTP/1.0\r\n"	; The request
	    "MIME-Version: 1.0\r\n"	; Version of MIME we speaketh
	    "Extension: %s\r\n"		; HTTP extensions we support
	    "Host: %s:%s\r\n"		; Who we want to talk to
	    "%s"			; Who its from
	    "Accept-encoding: %s\r\n"	; Encodings we understand
	    "%s"			; Languages we understand
	    "Accept: %s\r\n"		; Types we understand
	    "%s"			; User agent
	    "%s"			; Proxy Authorization
	    "%s"			; Authorization
	    "%s"			; Cookies
	    "%s"			; If-modified-since
	    "%s"			; Where we came from
	    "%s"			; Any extra headers
	    "%s"			; Any data
	    "\r\n")			; End request
	   (or url-request-method "GET")
	   fname
	   (or url-extensions-header "none")
	   (or host "UNKNOWN.HOST.NAME")
	   (url-port (or proxy-obj url-current-object))
	   (if url-personal-mail-address
	       (concat "From: " url-personal-mail-address "\r\n")
	     "")
	   url-mime-encoding-string
	   (if url-mime-language-string
	       (concat "Accept-language: " url-mime-language-string "\r\n")
	     "")
	   url-mime-accept-string
	   (url-http-user-agent-string)
	   (or proxy-auth "")
	   (or auth "")
	   (url-cookie-generate-header-lines
	    host real-fname (equal "https" (url-type url-current-object)))
	   (if (and (not no-cache)
		    (member url-request-method '("GET" nil)))
	       (let ((tm (url-is-cached url)))
		 (if tm
		     (concat "If-modified-since: "
			     (url-get-normalized-date tm) "\r\n")
		   ""))
	     "")
	   (if ref-url (concat "Referer: " ref-url "\r\n") "")
	   extra-headers
	   (if url-request-data
	       (format "Content-length: %d\r\n\r\n%s"
		       (length url-request-data) url-request-data)
	     "")))
    request))

(defun url-setup-reload-timer (url must-be-viewing &optional time)
  ;; Set up a timer to load URL at optional TIME.  If TIME is unspecified,
  ;; default to 5 seconds.  Only loads document if MUST-BE-VIEWING is the
  ;; current URL when the timer expires."
  (if (or (not time)
	  (<= time 0))
      (setq time 5))
  (let ((func
	 (` (lambda ()
	      (if (equal (url-view-url t) (, must-be-viewing))
		  (let ((w3-reuse-buffers 'no))
		    (if (equal (, url) (url-view-url t))
			(kill-buffer (current-buffer)))
		    (w3-fetch (, url))))))))
    (cond
     ((featurep 'itimer)
      (start-itimer "reloader" func time))
     ((fboundp 'run-at-time)
      (run-at-time time nil func))
     (t
      (url-warn 'url "Cannot set up timer for automatic reload, sorry!")))))

(defun url-handle-refresh-header (reload)
  (if (and reload
	   url-honor-refresh-requests
	   (or (eq url-honor-refresh-requests t)
	       (funcall url-confirmation-func "Honor refresh request? ")))
      (let ((uri (url-view-url t)))
	(if (string-match ";" reload)
	    (progn
	      (setq uri (substring reload (match-end 0) nil)
		    reload (substring reload 0 (match-beginning 0)))
	      (if (string-match
		   "ur[li][ \t]*=[ \t]*\"*\\([^ \t\"]+\\)\"*"
		   uri)
		  (setq uri (url-match uri 1)))
	      (setq uri (url-expand-file-name uri (url-view-url t)))))
	(url-setup-reload-timer uri (url-view-url t)
				(string-to-int (or reload "5"))))))

(defun url-parse-mime-headers (&optional no-delete switch-buff)
  ;; Parse mime headers and remove them from the html
  (and switch-buff (set-buffer url-working-buffer))
  (let* ((st (point-min))
	 (nd (progn
	       (goto-char (point-min))
	       (skip-chars-forward " \t\n")
	       (if (re-search-forward "^\r*$" nil t)
		   (1+ (point))
		 (point-max))))
	 save-pos
	 status
	 class
	 hname
	 hvalu
	 result
	 )
    (narrow-to-region st (min nd (point-max)))
    (goto-char (point-min))
    (skip-chars-forward " \t\n")	; Get past any blank crap
    (skip-chars-forward "^ \t")	; Skip over the HTTP/xxx
    (setq status (read (current-buffer)); Quicker than buffer-substring, etc.
	  result (cons (cons "status" status) result))
    (end-of-line)
    (while (not (eobp))
      (skip-chars-forward " \t\n\r")
      (setq save-pos (point))
      (skip-chars-forward "^:\n\r")
      (downcase-region save-pos (point))
      (setq hname (buffer-substring save-pos (point)))
      (skip-chars-forward ": \t ")
      (setq save-pos (point))
      (skip-chars-forward "^\n\r")
      (setq hvalu (buffer-substring save-pos (point))
	    result (cons (cons hname hvalu) result))
      (if (string= hname "set-cookie")
	  (url-cookie-handle-set-cookie hvalu)))
    (or no-delete (delete-region st (min nd (point))))
    (setq url-current-mime-type (cdr (assoc "content-type" result))
	  url-current-mime-charset nil
	  url-current-mime-encoding (cdr (assoc "content-encoding" result))
	  url-current-mime-viewer (mm-mime-info url-current-mime-type nil t)
	  url-current-mime-headers result
	  url-current-can-be-cached
	  (not (string-match "no-cache"
			     (or (cdr-safe (assoc "pragma" result)) ""))))
    (cond ((and url-current-mime-type
		(string-match url-mime-content-type-charset-regexp url-current-mime-type))
	   (setq url-current-mime-charset
		 (substring url-current-mime-type (match-beginning 1) (match-end 1)))
	   (setq url-current-mime-type
		 (substring url-current-mime-type 0 (match-beginning 0)))))
    (url-handle-refresh-header (cdr-safe (assoc "refresh" result)))
    (if (and url-request-method
	     (not (string= url-request-method "GET")))
	(setq url-current-can-be-cached nil))
    (let ((expires (cdr-safe (assoc "expires" result))))
      (if (and expires url-current-can-be-cached (featurep 'timezone))
	  (progn
	    (if (string-match
		 (concat "^[^,]+, +\\(..\\)-\\(...\\)-\\(..\\) +"
			 "\\(..:..:..\\) +\\[*\\([^\]]+\\)\\]*$")
			      expires)
		(setq expires (concat (url-match expires 1) " "
				      (url-match expires 2) " "
				      (url-match expires 3) " "
				      (url-match expires 4) " ["
				      (url-match expires 5) "]")))
	    (setq expires
		  (let ((d1 (mapcar
			     (function
			      (lambda (s) (and s (string-to-int s))))
			     (timezone-parse-date
			      (current-time-string))))
			(d2 (mapcar
			     (function (lambda (s) (and s (string-to-int s))))
			     (timezone-parse-date expires))))
		    (- (timezone-absolute-from-gregorian 
			(nth 1 d1) (nth 2 d1) (car d1))
		       (timezone-absolute-from-gregorian 
			(nth 1 d2) (nth 2 d2) (car d2))))
		  url-current-can-be-cached (/= 0 expires)))))
    (setq class (/ status 100))
    (cond
     ;; Classes of response codes
     ;;
     ;; 5xx = Server Error
     ;; 4xx = Client Error
     ;; 3xx = Redirection
     ;; 2xx = Successful
     ;; 1xx = Informational
     ;;
     ((= class 2)			; Successful in some form or another
      (cond
       ((or (= status 206)		; Partial content
	    (= status 205))		; Reset content
	(setq url-current-can-be-cached nil))
       ((= status 204)			; No response - leave old document
	(kill-buffer url-working-buffer))
       (t nil))				; All others indicate success
      )
     ((= class 3)			; Redirection of some type
      (cond
       ((or (= status 301)		; Moved - retry with Location: header
	    (= status 302)		; Found - retry with Location: header
	    (= status 303))		; Method - retry with location/method
	(let ((x (url-view-url t))
	      (redir (or (cdr (assoc "uri" result))
			 (cdr (assoc "location" result))))
	      (redirmeth (upcase (or (cdr (assoc "method" result))
				     url-request-method
				     "get"))))
	  (if (and redir (string-match "\\([^ \t]+\\)[ \t]" redir))
	      (setq redir (url-match redir 1)))
	  (if (and redir (string-match "^<\\(.*\\)>$" redir))
	      (setq redir (url-match redir 1)))

	  ;; As per Roy Fielding, 303 maps _any_ method to a 'GET'
	  (if (= 303 status)
	      (setq redirmeth "GET"))

	  ;; As per Roy Fielding, 301, 302 use the same method as the
	  ;; original request, but if != GET, user interaction is
	  ;; required.
	  (if (and (not (string= "GET" redirmeth))
		   (not (funcall
			 url-confirmation-func
			 (concat
			  "Honor redirection with non-GET method "
			  "(possible security risks)? "))))
	      (progn
		(url-warn 'url
			  (format
			   "The URL %s tried to issue a redirect to %s using a method other than
GET, which can open up various security holes.  Please see the
HTTP/1.0 specification for more details." x redir) 'error)
		(if (funcall url-confirmation-func
			     "Continue (with method of GET)? ")
		    (setq redirmeth "GET")
		  (error "Transaction aborted."))))

	  (if (not (equal x redir))
	      (let ((url-request-method redirmeth))
		(url-maybe-relative redir))
	    (progn
	      (goto-char (point-max))
	      (insert "<hr>Error!  This URL tried to redirect me to itself!<P>"
		      "Please notify the server maintainer.")))))
       ((= status 304)			; Cached document is newer
	(message "Extracting from cache...")
	(url-cache-extract (url-cache-create-filename (url-view-url t))))
       ((= status 305)			; Use proxy in Location: header
	nil)))
     ((= class 4)			; Client error
      (cond
       ((and (= status 401)		; Unauthorized access, retry w/auth.
	     (< url-current-passwd-count url-max-password-attempts))
	(setq url-current-passwd-count (1+ url-current-passwd-count))
	(let* ((y (or (cdr (assoc "www-authenticate" result)) "basic"))
	       (url (url-view-url t))
	       (type (downcase (if (string-match "[ \t]" y)
				   (substring y 0 (match-beginning 0))
				 y))))
	  (cond
	   ((url-auth-registered type)
	    (let ((args y)
		  (ctr (1- (length y)))
		  auth
		  (url-request-extra-headers url-request-extra-headers))
	      (while (/= 0 ctr)
		(if (= ?, (aref args ctr))
		    (aset args ctr ?\;))
		(setq ctr (1- ctr)))
	      (setq args (mm-parse-args y)
		    auth (url-get-authentication url
						 (cdr-safe
						  (assoc "realm" args))
						 type t args))
	      (if auth
		  (setq url-request-extra-headers
			(cons (cons "Authorization" auth)
			      url-request-extra-headers)))
	      (url-retrieve url t)))
	   (t
	    (widen)
	    (goto-char (point-max))
	    (setq url-current-can-be-cached nil)
	    (insert "<hr>Sorry, but I do not know how to handle " y
		    " authentication.  If you'd like to write it,"
		    " send it to " url-bug-address ".<hr>")))))
       ((= status 407)			; Proxy authentication required
	(let* ((y (or (cdr (assoc "proxy-authenticate" result)) "basic"))
	       (url (url-view-url t))
	       (urlobj (url-generic-parse-url url))
	       (url-basic-auth-storage url-proxy-basic-authentication)
	       (url-using-proxy (url-find-proxy-for-url urlobj
							(url-host urlobj)))
	       (type (downcase (if (string-match "[ \t]" y)
				   (substring y 0 (match-beginning 0))
				 y))))
	  (cond
	   ((url-auth-registered type)
	    (let ((args y)
		  (ctr (1- (length y)))
		  auth
		  (url-request-extra-headers url-request-extra-headers))
	      (while (/= 0 ctr)
		(if (= ?, (aref args ctr))
		    (aset args ctr ?\;))
		(setq ctr (1- ctr)))
	      (setq args (mm-parse-args y)
		    auth (url-get-authentication (or url-using-proxy url)
						 (cdr-safe
						  (assoc "realm" args))
						 type t args))
	      (if auth
		  (setq url-request-extra-headers
			(cons (cons "Proxy-Authorization" auth)
			      url-request-extra-headers)))
	      (setq url-proxy-basic-authentication url-basic-auth-storage)
	      (url-retrieve url t)))
	   (t
	    (widen)
	    (goto-char (point-max))
	    (setq url-current-can-be-cached nil)
	    (insert "<hr>Sorry, but I do not know how to handle " y
		    " authentication.  If you'd like to write it,"
		    " send it to " url-bug-address ".<hr>")))))
       ;;((= status 400) nil)		; Bad request - syntax
       ;;((= status 401) nil)		; Tried too many times
       ;;((= status 402) nil)		; Payment required, retry w/Chargeto:
       ;;((= status 403) nil)		; Access is forbidden
       ;;((= status 404) nil)		; Not found...
       ;;((= status 405) nil)		; Method not allowed
       ;;((= status 406) nil)		; None acceptable
       ;;((= status 408) nil)		; Request timeout
       ;;((= status 409) nil)		; Conflict
       ;;((= status 410) nil)		; Document is gone
       ;;((= status 411) nil)		; Length required
       ;;((= status 412) nil)		; Unless true
       (t				; All others mena something hosed
	(setq url-current-can-be-cached nil))))
     ((= class 5)
;;;      (= status 504)			; Gateway timeout
;;;      (= status 503)			; Service unavailable
;;;      (= status 502)			; Bad gateway
;;;      (= status 501)			; Facility not supported
;;;      (= status 500)			; Internal server error
      (setq url-current-can-be-cached nil))
     ((= class 1)
      (cond
       ((or (= status 100)		; Continue
	    (= status 101))		; Switching protocols
	nil)))
     (t
      (setq url-current-can-be-cached nil)))
    (widen)
    status))

(defun url-mime-response-p (&optional switch-buff)
  ;; Determine if the current buffer is a MIME response
  (and switch-buff (set-buffer url-working-buffer))
  (goto-char (point-min))
  (skip-chars-forward " \t\n")
  (and (looking-at "^HTTP/.+")))

(defsubst url-recreate-with-attributes (obj)
  (if (url-attributes obj)
      (concat (url-filename obj) ";"
	      (mapconcat
	       (function
		(lambda (x)
		  (if (cdr x)
		      (concat (car x) "=" (cdr x))
		    (car x)))) (url-attributes obj) ";"))
    (url-filename obj)))

(defun url-http (url &optional proxy-info)
  ;; Retrieve URL via http.
  (let* ((urlobj (url-generic-parse-url url))
	 (ref-url (or url-current-referer (url-view-url t))))
    (url-clear-tmp-buffer)
    (let* ((server (url-host urlobj))
	   (port   (url-port urlobj))
	   (file   (or proxy-info (url-recreate-with-attributes urlobj)))
	   (dest   (url-target urlobj))
	   request)
      (if (equal port "") (setq port "80"))
      (if (equal file "") (setq file "/"))
      (if (not server)
	  (message "Malformed URL: `%s'" url)
	(if (or (not (member port url-bad-port-list))
		(funcall url-confirmation-func
			 (concat
			  "Warning!  Trying to connect to port "
			  port
			  " - continue? ")))
	    (progn
	      (setq request (url-create-mime-request file ref-url))
	      (url-lazy-message "Contacting %s:%s" server port)
	      (let ((process
		     (url-open-stream "WWW" url-working-buffer server
				      (string-to-int port))))
		(if (not (processp process))
		    (url-sentinel url-working-buffer nil)
		  (progn
		    (url-process-put process 'url (or proxy-info url))
		    (set-process-sentinel process 'ignore)
		    (process-kill-without-query process)
		    (process-send-string process request)
		    (url-lazy-message "Request sent, waiting for response...")
		    (setq url-current-content-length nil)
		    (make-local-variable 'after-change-functions)
		    (add-hook 'after-change-functions 'url-after-change-function)
		    (if url-be-asynchronous
			(set-process-sentinel process 'url-sentinel)
		      (unwind-protect
			  (save-excursion
			    (set-buffer url-working-buffer)
			    (while (memq (url-process-status process)
					 '(run open))
			      (url-accept-process-output process)))
			(condition-case ()
			    (url-kill-process process)
			  (error nil))))
		    (if url-be-asynchronous
			nil
		      (message "Retrieval complete.")
		      (if (fboundp 'clear-progress) (clear-progress))
		      (remove-hook 'after-change-functions
				   'url-after-change-function))))))
	  (progn
	    (ding)
	    (url-warn 'security "Aborting connection to bad port...")))))))

(defun url-https (url)
  ;; Retrieve a URL via SSL
  (condition-case ()
      (require 'ssl)
    (error (error "Not configured for SSL, please read the info pages.")))
  (let ((url-this-is-ssl t)
	(url-gateway-method 'ssl))
    (url-http url)))

(provide 'url-http)
