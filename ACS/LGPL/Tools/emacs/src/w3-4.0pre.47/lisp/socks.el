;;; socks.el --- A Socks v5 Client for Emacs
;; Author: wmperry
;; Created: 1999/11/09 14:52:16
;; Version: 1.4
;; Keywords: comm, firewalls

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1996 - 1998 by William M. Perry <wmperry@cs.indiana.edu>
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
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This is an implementation of the SOCKS v5 protocol as defined in
;;; RFC 1928.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO
;; - Finish the redirection rules stuff
;; - Implement composition of servers.  Recursively evaluate the
;;   redirection rules and do SOCKS-over-HTTP and SOCKS-in-SOCKS

(require 'cl)
(require 'custom)

;; For non-MULE
(if (not (fboundp 'char-int))
    (fset 'char-int 'identity))

(if (not (fboundp 'split-string))
    (defun split-string (string &optional pattern)
      "Return a list of substrings of STRING which are separated by PATTERN.
If PATTERN is omitted, it defaults to \"[ \\f\\t\\n\\r\\v]+\"."
      (or pattern
	  (setq pattern "[ \f\t\n\r\v]+"))
      (let (parts (start 0))
	(while (string-match pattern string start)
	  (setq parts (cons (substring string start (match-beginning 0)) parts)
		start (match-end 0)))
	(nreverse (cons (substring string start) parts)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Custom widgets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-widget 'dynamic-choice 'menu-choice
  "A pretty simple dynamic dropdown list"
  :format "%[%t%]: %v"
  :tag "Network"
  :case-fold t
  :void '(item :format "invalid (%t)\n")
  :value-create 's5-widget-value-create
  :value-delete 'widget-children-value-delete
  :value-get 'widget-choice-value-get
  :value-inline 'widget-choice-value-inline
  :mouse-down-action 'widget-choice-mouse-down-action
  :action 'widget-choice-action
  :error "Make a choice"
  :validate 'widget-choice-validate
  :match 's5-dynamic-choice-match
  :match-inline 's5-dynamic-choice-match-inline)

(defun s5-dynamic-choice-match (widget value)
  (let ((choices (funcall (widget-get widget :choice-function)))
	current found)
    (while (and choices (not found))
      (setq current (car choices)
	    choices (cdr choices)
	    found (widget-apply current :match value)))
    found))

(defun s5-dynamic-choice-match-inline (widget value)
  (let ((choices (funcall (widget-get widget :choice-function)))
	current found)
    (while (and choices (not found))
      (setq current (car choices)
	    choices (cdr choices)
	    found (widget-match-inline current value)))
    found))

(defun s5-widget-value-create (widget)
  (let ((choices (funcall (widget-get widget :choice-function)))
	(value (widget-get widget :value)))
    (if (not value)
	(widget-put widget :value (widget-value (car choices))))
    (widget-put widget :args choices)
    (widget-choice-value-create widget)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Customization support
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup socks nil
  "SOCKS Support"
  :prefix "socks-"
  :group 'processes)

'(defcustom socks-server-aliases nil
  "A list of server aliases for use in access control and filtering rules."
  :group 'socks
  :type '(repeat (list :format "%v"
		       :value ("" "" 1080 5)
		       (string :tag "Alias")
		       (string :tag "Hostname/IP Address")
		       (integer :tag "Port #")
		       (choice :tag "SOCKS Version"
			       (integer :tag "SOCKS v4" :value 4)
			       (integer :tag "SOCKS v5" :value 5)))))

'(defcustom socks-network-aliases
  '(("Anywhere" (netmask "0.0.0.0" "0.0.0.0")))
  "A list of network aliases for use in subsequent rules."
  :group 'socks
  :type '(repeat (list :format "%v"
		       :value (netmask "" "255.255.255.0")
		       (string :tag "Alias")
		       (radio-button-choice
			:format "%v"
			(list :tag  "IP address range"
			      (const :format "" :value range)
			      (string :tag "From")
			      (string :tag "To"))
			(list :tag  "IP address/netmask"
			      (const :format "" :value netmask)
			      (string :tag "IP Address")
			      (string :tag "Netmask"))
			(list :tag  "Domain Name"
			      (const :format "" :value domain)
			      (string :tag "Domain name"))
			(list :tag  "Unique hostname/IP address"
			      (const :format "" :value exact)
			      (string :tag "Hostname/IP Address"))))))

'(defun s5-servers-filter ()
  (if socks-server-aliases
      (mapcar (lambda (x) (list 'const :tag (car x) :value (car x))) s5-server-aliases)
    '((const :tag "No aliases defined" :value nil))))

'(defun s5-network-aliases-filter ()
  (mapcar (lambda (x) (list 'const :tag (car x) :value (car x)))
	  socks-network-aliases))

'(defcustom socks-redirection-rules
   nil
   "A list of redirection rules."
   :group 'socks
   :type '(repeat (list :format "%v"
			:value ("Anywhere" nil)
			(dynamic-choice :choice-function s5-network-aliases-filter
					:tag "Destination network")
			(radio-button-choice
			 :tag "Connection type"
			 (const :tag "Direct connection" :value nil)
			 (dynamic-choice :format "%t: %[%v%]"
					 :choice-function s5-servers-filter
					 :tag "Proxy chain via")))))

(defcustom socks-server
  (list "Default server" "socks" 1080 5)
  ""
  :group 'socks
  :type '(list
	  (string :format "" :value "Default server")
	  (string :tag "Server")
	  (integer :tag "Port")
	  (radio-button-choice :tag "SOCKS Version"
			       :format "%t: %v"
			       (const :tag "SOCKS v4  " :format "%t" :value 4)
			       (const :tag "SOCKS v5"   :format "%t" :value 5))))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Get down to the nitty gritty
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst socks-version 5)
(defvar socks-debug nil)

;; Common socks v5 commands
(defconst socks-connect-command 1)
(defconst socks-bind-command 2)
(defconst socks-udp-associate-command 3)

;; Miscellaneous other socks constants
(defconst socks-authentication-null 0)
(defconst socks-authentication-failure 255)

;; Response codes
(defconst socks-response-success               0)
(defconst socks-response-general-failure       1)
(defconst socks-response-access-denied         2)
(defconst socks-response-network-unreachable   3)
(defconst socks-response-host-unreachable      4)
(defconst socks-response-connection-refused    5)
(defconst socks-response-ttl-expired           6)
(defconst socks-response-cmd-not-supported     7)
(defconst socks-response-address-not-supported 8)

(defvar socks-errors
  '("Succeeded"
    "General SOCKS server failure"
    "Connection not allowed by ruleset"
    "Network unreachable"
    "Host unreachable"
    "Connection refused"
    "Time-to-live expired"
    "Command not supported"
    "Address type not supported"))

;; The socks v5 address types
(defconst socks-address-type-v4   1)
(defconst socks-address-type-name 3)
(defconst socks-address-type-v6   4)

;; Base variables
(defvar socks-timeout 5)
(defvar socks-connections (make-hash-table :size 13))

;; Miscellaneous stuff for authentication
(defvar socks-authentication-methods nil)
(defvar socks-username (user-login-name))
(defvar socks-password nil)

(defun socks-register-authentication-method (id desc callback)
  (let ((old (assq id socks-authentication-methods)))
    (if old
	(setcdr old (cons desc callback))
      (setq socks-authentication-methods
	    (cons (cons id (cons desc callback))
		  socks-authentication-methods)))))

(defun socks-unregister-authentication-method (id)
  (let ((old (assq id socks-authentication-methods)))
    (if old
	(setq socks-authentication-methods
	      (delq old socks-authentication-methods)))))

(socks-register-authentication-method 0 "No authentication" 'identity)

(defun socks-build-auth-list ()
  (let ((num 0)
	(retval ""))
    (mapcar
     (function
      (lambda (x)
	(if (fboundp (cdr (cdr x)))
	    (setq retval (format "%s%c" retval (car x))
		  num (1+ num)))))
     (reverse socks-authentication-methods))
    (format "%c%s" num retval)))

(defconst socks-state-waiting-for-auth 0)
(defconst socks-state-submethod-negotiation 1)
(defconst socks-state-authenticated 2)
(defconst socks-state-waiting 3)
(defconst socks-state-connected 4)

(defmacro socks-wait-for-state-change (proc htable cur-state)
  (`
   (while (and (= (cl-gethash 'state (, htable)) (, cur-state))
	       (memq (process-status (, proc)) '(run open)))
     (accept-process-output (, proc) socks-timeout))))

(defun socks-filter (proc string)
  (let ((info (cl-gethash proc socks-connections))
	state desired-len)
    (or info (error "socks-filter called on non-SOCKS connection %S" proc))
    (setq state (cl-gethash 'state info))
    (cond
     ((= state socks-state-waiting-for-auth)
      (cl-puthash 'scratch (concat string (cl-gethash 'scratch info)) info)
      (setq string (cl-gethash 'scratch info))
      (if (< (length string) 2)
	  nil				; We need to spin some more
	(cl-puthash 'authtype (char-int (aref string 1)) info)
	(cl-puthash 'scratch (substring string 2 nil) info)
	(cl-puthash 'state socks-state-submethod-negotiation info)))
     ((= state socks-state-submethod-negotiation)
      )
     ((= state socks-state-authenticated)
      )
     ((= state socks-state-waiting)
      (cl-puthash 'scratch (concat string (cl-gethash 'scratch info)) info)
      (setq string (cl-gethash 'scratch info))
      (case (cl-gethash 'server-protocol info)
	(http
	 (if (not (string-match "\r\n\r\n" string))
	     nil			; Need to spin some more
	   (debug)
	   (cl-puthash 'state socks-state-connected info)
	   (cl-puthash 'reply 0 info)
	   (cl-puthash 'response string info)))
	(4
	 (if (< (length string) 2)
	     nil			; Can't know how much to read yet
	   (setq desired-len
		 (+ 4 ; address length
		    2 ; port
		    2 ; initial data
		    ))
	   (if (< (length string) desired-len)
	       nil			; need to spin some more
	     (let ((response (char-int (aref string 1))))
	       (if (= response 90)
		   (setq response 0))
	       (cl-puthash 'state socks-state-connected info)
	       (cl-puthash 'reply response info)
	       (cl-puthash 'response string info)))))
	(5
	 (if (< (length string) 4)
	     nil
	   (setq desired-len
		 (+ 6			; Standard socks header
		    (cond
		     ((= (char-int (aref string 3)) socks-address-type-v4) 4)
		     ((= (char-int (aref string 3)) socks-address-type-v6) 16)
		     ((= (char-int (aref string 3)) socks-address-type-name)
		      (if (< (length string) 5)
			  255
			(+ 1 (char-int (aref string 4))))))))
	   (if (< (length string) desired-len)
	       nil			; Need to spin some more
	     (cl-puthash 'state socks-state-connected info)
	     (cl-puthash 'reply (char-int (aref string 1)) info)
	     (cl-puthash 'response string info))))))
     ((= state socks-state-connected)
      )
     )
    )
  )

(defun socks-open-connection (server-info)
  (interactive)
  (save-excursion
    (let ((proc (socks-original-open-network-stream "socks"
						    nil
						    (nth 1 server-info)
						    (nth 2 server-info)))
	  (info (make-hash-table :size 13))
	  (authtype nil))
      
      ;; Initialize process and info about the process
      (set-process-filter proc 'socks-filter)
      (process-kill-without-query proc)
      (cl-puthash proc info socks-connections)
      (cl-puthash 'state socks-state-waiting-for-auth info)
      (cl-puthash 'authtype socks-authentication-failure info)
      (cl-puthash 'server-protocol (nth 3 server-info) info)
      (cl-puthash 'server-name (nth 1 server-info) info)
      (case (nth 3 server-info)
	(http
	 ;; Don't really have to do any connection setup under http
	 nil)
	(4
	 ;; Don't really have to do any connection setup under v4
	 nil)
	(5
	 ;; Need to handle all the authentication crap under v5
	 ;; Send what we think we can handle for authentication types
	 (process-send-string proc (format "%c%s" socks-version
					   (socks-build-auth-list)))

	 ;; Basically just do a select() until we change states.
	 (socks-wait-for-state-change proc info socks-state-waiting-for-auth)
	 (setq authtype (cl-gethash 'authtype info))
	 (cond
	  ((= authtype socks-authentication-null)
	   (and socks-debug (message "No authentication necessary")))
	  ((= authtype socks-authentication-failure)
	   (error "No acceptable authentication methods found."))
	  (t
	   (let* ((auth-type (cl-gethash 'authtype info))
		  (auth-handler (assoc auth-type socks-authentication-methods))
		  (auth-func (and auth-handler (cdr (cdr auth-handler))))
		  (auth-desc (and auth-handler (car (cdr auth-handler)))))
	     (set-process-filter proc nil)
	     (if (and auth-func (fboundp auth-func)
		      (funcall auth-func proc))
		 nil			; We succeeded!
	       (delete-process proc)
	       (error "Failed to use auth method: %s (%d)"
		      (or auth-desc "Unknown") auth-type))
	     )
	   )
	  )
	 (cl-puthash 'state socks-state-authenticated info)
	 (set-process-filter proc 'socks-filter)))
      proc)))

(defun socks-send-command (proc command atype address port)
  (let ((addr (cond
	       ((or (= atype socks-address-type-v4)
		    (= atype socks-address-type-v6))
		address)
	       ((= atype socks-address-type-name)
		(format "%c%s" (length address) address))
	       (t
		(error "Unkown address type: %d" atype))))
	(info (cl-gethash proc socks-connections))
	request version)
    (or info (error "socks-send-command called on non-SOCKS connection %S"
		    proc))
    (cl-puthash 'state socks-state-waiting info)
    (setq version (cl-gethash 'server-protocol info))
    (case version
      (http
       (setq request (format (eval-when-compile
			       (concat
				"CONNECT %s:%d HTTP/1.0\r\n"
				"User-Agent: Emacs/SOCKS v1.0\r\n"
				"\r\n"))
			     (case atype
			       (socks-address-type-name address)
			       (otherwise
				(error "Unsupported address type for HTTP: %d" atype)))
			     port)))
      (4
       (setq request (format
		      "%c%c%c%c%s%s%c"
		      version		; version
		      command		; command
		      (lsh port -8)	; port, high byte
		      (- port (lsh (lsh port -8) 8)) ; port, low byte
		      addr		; address
		      (user-full-name)	; username
		      0			; terminate username
		      )))
      (5
       (setq request (format 
		      "%c%c%c%c%s%c%c"
		      version		; version 
		      command		; command
		      0			; reserved
		      atype		; address type
		      addr		; address
		      (lsh port -8)	; port, high byte
		      (- port (lsh (lsh port -8) 8)) ; port, low byte
		      )))
      (otherwise
       (error "Unknown protocol version: %d" version)))
    (process-send-string proc request)
    (socks-wait-for-state-change proc info socks-state-waiting)
    (process-status proc)
    (if (= (or (cl-gethash 'reply info) 1) socks-response-success)
	nil				; Sweet sweet success!
      (delete-process proc)
      (error "SOCKS: %s" (nth (or (cl-gethash 'reply info) 1) socks-errors)))
    proc))


;; Replacement functions for open-network-stream, etc.
(defvar socks-noproxy nil
  "*List of regexps matching hosts that we should not socksify connections to")

(defun socks-find-route (host service)
  (let ((route socks-server)
	(noproxy socks-noproxy))
    (while noproxy
      (if (eq ?! (aref (car noproxy) 0))
	  (if (string-match (substring (car noproxy) 1) host)
	      (setq noproxy nil))
	(if (string-match (car noproxy) host)
	    (setq route nil
		  noproxy nil)))
      (setq noproxy (cdr noproxy)))
    route))

(defvar socks-override-functions nil
  "*Whether to overwrite the open-network-stream function with the SOCKSified
version.")

(if (fboundp 'socks-original-open-network-stream)
    nil					; Do nothing, we've been here already
  (fset 'socks-original-open-network-stream
	(symbol-function 'open-network-stream))
  (if socks-override-functions
      (fset 'open-network-stream 'socks-open-network-stream)))

(defvar socks-services-file "/etc/services")
(defvar socks-tcp-services (make-hash-table :size 13 :test 'equal))
(defvar socks-udp-services (make-hash-table :size 13 :test 'equal))

(defun socks-parse-services ()
  (if (not (and (file-exists-p socks-services-file)
		(file-readable-p socks-services-file)))
      (error "Could not find services file: %s" socks-services-file))
  (save-excursion
    (clrhash socks-tcp-services)
    (clrhash socks-udp-services)
    (set-buffer (get-buffer-create " *socks-tmp*"))
    (erase-buffer)
    (insert-file-contents socks-services-file)
    ;; Nuke comments
    (goto-char (point-min))
    (while (re-search-forward "#.*" nil t)
      (replace-match ""))
    ;; Nuke empty lines
    (goto-char (point-min))
    (while (re-search-forward "^[ \t\n]+" nil t)
      (replace-match ""))
    ;; Now find all the lines
    (goto-char (point-min))
    (let (name port type)
      (while (re-search-forward "^\\([^ \t]+\\)[ \t]+\\([0-9]+\\)/\\([a-z]+\\)"
				nil t)
	(setq name (downcase (match-string 1))
	      port (string-to-int (match-string 2))
	      type (downcase (match-string 3)))
	(cl-puthash name port (if (equal type "udp")
			       socks-udp-services
			     socks-tcp-services))))))

(defun socks-find-services-entry (service &optional udp)
  "Return the port # associated with SERVICE"
  (if (= (hash-table-count socks-tcp-services) 0)
      (socks-parse-services))
  (cl-gethash (downcase service)
	      (if udp socks-udp-services socks-tcp-services)))

(defun socks-open-network-stream (name buffer host service)
  (let* ((route (socks-find-route host service))
	 proc info version atype)
    (if (not route)
	(socks-original-open-network-stream name buffer host service)
      (setq proc (socks-open-connection route)
	    info (cl-gethash proc socks-connections)
	    version (cl-gethash 'server-protocol info))
      (case version
	(4
	 (setq host (socks-nslookup-host host))
	 (if (not (listp host))
	     (error "Could not get IP address for: %s" host))
	 (setq host (apply 'format "%c%c%c%c" host))
	 (setq atype socks-address-type-v4))
	(otherwise
	 (setq atype socks-address-type-name)))
      (socks-send-command proc
			  socks-connect-command
			  atype
			  host
			  (if (stringp service)
			      (socks-find-services-entry service)
			    service))
      (cl-puthash 'buffer buffer info)
      (cl-puthash 'host host info)
      (cl-puthash 'service host info)
      (set-process-filter proc nil)
      (set-process-buffer proc (if buffer (get-buffer-create buffer)))
      proc)))

;; Authentication modules go here

;; Basic username/password authentication, ala RFC 1929
(socks-register-authentication-method 2 "Username/Password"
				      'socks-username/password-auth)

(defconst socks-username/password-auth-version 1)

(if (not (fboundp 'char-int))
    (fset 'char-int 'identity))

(defun socks-username/password-auth-filter (proc str)
  (let ((info (cl-gethash proc socks-connections))
	state desired-len)
    (or info (error "socks-filter called on non-SOCKS connection %S" proc))
    (setq state (cl-gethash 'state info))
    (cl-puthash 'scratch (concat (cl-gethash 'scratch info) str) info)
    (if (< (length (cl-gethash 'scratch info)) 2)
	nil
      (cl-puthash 'password-auth-status (char-int
					 (aref (cl-gethash 'scratch info) 1))
		  info)
      (cl-puthash 'state socks-state-authenticated info))))

(defun socks-username/password-auth (proc)
  (let* ((info (cl-gethash proc socks-connections))
	 (state (cl-gethash 'state info)))
    (if (not socks-password)
	(setq socks-password (read-passwd
			      (format "Password for %s@%s: "
				      socks-username
				      (cl-gethash 'server-name info)))))
    (cl-puthash 'scratch "" info)
    (set-process-filter proc 'socks-username/password-auth-filter)
    (process-send-string proc
			 (format "%c%c%s%c%s"
				 socks-username/password-auth-version
				 (length socks-username)
				 socks-username
				 (length socks-password)
				 socks-password))
    (socks-wait-for-state-change proc info state)
    (= (cl-gethash 'password-auth-status info) 0)))


;; More advanced GSS/API stuff, not yet implemented - volunteers?
;; (socks-register-authentication-method 1 "GSS/API" 'socks-gssapi-auth)

(defun socks-gssapi-auth (proc)
  nil)


;; CHAP stuff
;; (socks-register-authentication-method 3 "CHAP" 'socks-chap-auth)
(defun socks-chap-auth (proc)
  nil)


;; CRAM stuff
;; (socks-register-authentication-method 5 "CRAM" 'socks-cram-auth)
(defun socks-cram-auth (proc)
  nil)


(defcustom socks-nslookup-program "nslookup"
  "*If non-NIL then a string naming the nslookup program."
  :type '(choice (const :tag "None" :value nil) string)
  :group 'socks)

(defun socks-nslookup-host (host)
  "Attempt to resolve the given HOSTNAME using nslookup if possible."
  (interactive "sHost:  ")
  (if socks-nslookup-program
      (let ((proc (start-process " *nslookup*" " *nslookup*"
				 socks-nslookup-program host))
	    (res host))
	(process-kill-without-query proc)
	(save-excursion
	  (set-buffer (process-buffer proc))
	  (while (progn
		   (accept-process-output proc)
		   (memq (process-status proc) '(run open))))
	  (goto-char (point-min))
	  (if (re-search-forward "Name:.*\nAddress\\(es\\)?: *\\([0-9.]+\\)$" nil t)
	      (progn
		(setq res (buffer-substring (match-beginning 2)
					    (match-end 2))
		      res (mapcar 'string-to-int (split-string res "\\.")))))
	  (kill-buffer (current-buffer)))
	res)
    host))

(provide 'socks)
