;;; mule-sysdp.el --- consolidate MULE-version dependencies in one file.

;; Copyright (c) 1996 - 1999 William Perry

;; Author: William Perry <wmperry@cs.indiana.edu>
;; Keywords: lisp, tools

;; The purpose of this file is to eliminate the cruftiness that
;; would otherwise be required of packages that want to run on multiple
;; versions of Emacs with and without Mule support.

(require 'cl)

(defconst mule-sysdep-version (if (featurep 'mule)
				  (cond
				   ((string-match "XEmacs" emacs-version)
				    (if (boundp 'enable-multibyte-characters)
					(make-variable-buffer-local 'enable-multibyte-characters))
				    'xemacs)
				   ((and
				     (boundp 'mule-version)
				     (string-match "[0-9]+\\.[0-9]+"
						   mule-version))
				    (if (or (fboundp 'kinsoku)
					    (boundp 'kinsoku-ascii))
					(string-to-number (substring
							   mule-version
							   (match-beginning 0)
							   (match-end 0)))
				      0))
				   (t 2.3))
				0)
  "What version of mule we are running under.")

;; MULE variants
;; XEmacs 20.x  - 'xemacs
;; MULE 2.3/2.4 - 2.3 -or- 2.4 (2.4 exists only in beta versions)
;; Emacs 20.x   - 3.0
;; Emacs 20.3+  - 4.0 (with character encapsulation)

;; Return non-nil if CODING-SYSTEM is a valid coding system system.
(defun mule-coding-system-p (coding-system)
  (case mule-sysdep-version
    (xemacs
     (find-coding-system coding-system))
    (0
     nil)
    (otherwise
     (coding-system-p coding-system))))

;; Return the first valid coding system in the list ARGS.
(defun mule-coding-system-version (&rest args)
  (if (featurep 'mule)
      (progn
	(while (and args (not (mule-coding-system-p (car args))))
	  (setq args (cdr args)))
	(car args))))

(defconst w3-mime-charset-coding-alist
  `(("big5" . ,(mule-coding-system-version 'cn-big5 '*big5*))
    ("euc-jp" . ,(mule-coding-system-version 'euc-jp '*euc-japan*))
    ("euc-kr" . ,(mule-coding-system-version 'euc-kr '*euc-korea*))
    ("gb" . ,(mule-coding-system-version 'cn-gb-2312 '*euc-china*))
    ("iso-2022-jp" . ,(mule-coding-system-version 'iso-2022-jp '*iso-2022-jp*))
    ("iso-2022-kr" . ,(mule-coding-system-version 'iso-2022-kr '*iso-2022-kr*))
    ("iso-8859-1" . ,(mule-coding-system-version 'iso-8859-1 '*ctext*))
    ("koi-8" . ,(mule-coding-system-version 'koi8))
    ("koi8-r" . ,(mule-coding-system-version 'koi8))
    ("x-ctext" . ,(mule-coding-system-version 'ctext '*ctext*))
    ("x-sjis" . ,(mule-coding-system-version 'shift_jis '*sjis*))
    ("x-shiftjis" . ,(mule-coding-system-version 'shift_jis '*sjis*))
    )
  "Alist of MIME-charset parameter regexps vs the corresponding coding systems.")

(defconst w3-url-domain-language-environment-alist
  '(("\\.jp$" . "Japanese")
    ("\\.cn$" . "Chinese-GB")
    ("\\.tw$" . "Chinese-BIG5")
    ("\\.hk$" . "Chinese-BIG5")
    ("\\.sg$" . "Chinese-GB")
    ("\\.kr$" . "Korean")
    ("\\.ru$" . "Cyrillic-KOI8")
    ("\\.su$" . "Cyrillic-KOI8")
    )
  "Alist of regexps of URL vs the corresponding language environemnt.
While detecting a coding system of a document whose URL matches some of
the regular expressions, the coding system priorities defined in
the corresponding language environment is used.

This facility can be used only in Emacs 20.3 or later.")

(defconst w3-url-domain-coding-alist
  `(("\\.th$" . ,(mule-coding-system-version 'th-tis620 '*tis620*))
    ("\\.kr$" . ,(mule-coding-system-version 'euc-kr '*euc-korea*))
    ("\\.cn$" . ,(mule-coding-system-version 'cn-gb-2312 '*euc-china*))
    ("\\.sg$" . ,(mule-coding-system-version 'cn-gb-2312 '*euc-china*))
    ("\\.tw$" . ,(mule-coding-system-version 'cn-big5 '*big5*))
    ("\\.hk$" . ,(mule-coding-system-version 'cn-big5 '*big5*))
    ("\\.su$" . ,(mule-coding-system-version 'koi8))
    ("\\.il$" . ,(mule-coding-system-version 'iso-8859-8 '*iso-8859-8*))
    ("\\.fr$" . ,(mule-coding-system-version 'iso-8859-1 '*ctext*)))
  "Alist of regexps of URL vs the corresponding coding systems.
While decoding a document whose URL matches some of
the regeular expressions, the corresponding coding system is used.")

(defconst mule-retrieval-coding-system
  (mule-coding-system-version 'euc-japan '*euc-japan* 'coding-system-euc-japan)
  "Default retrieval coding system for packages that use this package.")

(defconst mule-no-coding-system
  (mule-coding-system-version 'no-conversion '*noconv*)
  "Coding system that means no coding system should be used.")

;; With Mule of version 2.3, insert-file-contents-literally is defined
;; but performs code conversion.  Thus, we re-define it here to avoid
;; the code conversion.
(if (and (numberp mule-sysdep-version) (= mule-sysdep-version 2.3))
    (defun insert-file-contents-literally
      (file &optional visit beg end replace)
      "Like `insert-file-contents', q.v., but only reads in the file.
A buffer may be modified in several ways after reading into the buffer due
to advanced Emacs features, such as file-name-handlers, format decoding,
find-file-hooks, etc.
  This function ensures that none of these modifications will take place."
      (let ((file-name-handler-alist nil)
	    (find-file-hooks nil)
	    (input-coding-system mule-no-coding-system))
	(insert-file-contents file visit beg end replace))))

;; List of coding systems that require calling
;; w3-replace-invalid-chars after decoding a text by them.
(defconst mule-invalid-char-coding-systems
  (list (mule-coding-system-version 'raw-text '*noconv*)
	(mule-coding-system-version 'iso-8859-1 '*ctext*)))

;; Return non-nil if CODING-SYSTEM requires calling
;; w3-replace-invalid-chars after decoding a text.
(defun mule-coding-system-with-invalid-chars (coding-system)
  (or (null coding-system)
      (progn
	(if (listp coding-system)
	    (setq coding-system (car coding-system)))
	(case mule-sysdep-version
	  ((2.3 2.4)
	   (while (not (vectorp (get coding-system 'coding-system)))
	     (setq coding-system (get coding-system 'coding-system))))
	  (3.0
	   (setq coding-system (coding-system-base coding-system))))
	(memq coding-system mule-invalid-char-coding-systems))))

(defun mule-detect-coding-version (host st nd)
  "Return a coding system of the current data."
  (let ((coding-system nil))
    ;; The facility of detecting a coding system by language
    ;; environment is provided only in Emacs 20.3 (Mule 4.0) or later.
    (if (and host
	     (numberp mule-sysdep-version)
	     (>= mule-sysdep-version 4.0))
	(let ((l w3-url-domain-language-environment-alist))
	  (while l
	    (if (string-match (car (car l)) host)
		(setq coding-system
		      (funcall 'detect-coding-with-language-environment
			       st nd (cdr (car l)))
		      l nil)
	      (setq l (cdr l))))))
    ;; Next, check if the data is from some domain which prefer a
    ;; specific coding system.
    (if (and (not coding-system)
	     host)
	(let ((l w3-url-domain-coding-alist))
	  (while l
	    (if (string-match (car (car l)) host)
		(setq coding-system (if (mule-coding-system-p
					 (cdr (car l)))
					(cdr (car l)))
		      l nil)
	      (setq l (cdr l))))))
    ;; At last, try to detect a coding system from the data itself.
    (if (not coding-system)
	(setq coding-system
	      (case mule-sysdep-version
		(2.3 (code-detect-region st nd))
		((2.4 xemacs)
		 (detect-coding-region st nd))
		(3.0
		 ;; Emacs 20.2 returns bogus information from
		 ;; detect-coding-region sometimes.  This is fixed in
		 ;; 20.3 (aka Mule 4.x)
		 (let ((possible (detect-coding-region st nd)))
		   (if (not (symbolp (car-safe possible)))
		       '(undecided-unix)
		     possible)))
		((4.0 4.1)
		 ;; We can use HIGHEST arg t for faster detection.
		 (detect-coding-region st nd t))
		(otherwise nil))))
    (if (and (not (listp coding-system))
	     (not (mule-coding-system-p coding-system)))
	(setq coding-system mule-no-coding-system))
    coding-system))

(defun mule-code-convert-region (code)
  (if (and (listp code) (car code))
      (setq code (car code)))
  (case mule-sysdep-version
    (2.3
     (set 'mc-flag t)
     (code-convert-region (point-min) (point-max) code *internal*)
     (set-file-coding-system code))
    (2.4
     (set (make-local-variable 'enable-multibyte-characters) t)
     (if (memq code '(autodetect coding-system-automatic))
	 nil
       (decode-coding-region (point-min) (point-max) code)
       (set-buffer-file-coding-system code)))
    (3.0
     (when default-enable-multibyte-characters
       (set (make-local-variable 'enable-multibyte-characters) t)
       (if (memq code '(autodetect automatic-conversion))
	   nil
	 (or code (setq code 'automatic-conversion))
	 (decode-coding-region (point-min) (point-max) code)
	 (set-buffer-file-coding-system code))))
    (4.0
     (when default-enable-multibyte-characters
       (set-buffer-multibyte t)
       (if (memq code '(autodetect automatic-conversion))
	   nil
	 (or code (setq code 'automatic-conversion))
	 (decode-coding-region (point-min) (point-max) code)
	 (set-buffer-file-coding-system (symbol-value 'last-coding-system-used)))))
    (xemacs
     (if (and (listp code) (not (car code)))
	 (progn
	   (setq code 'autodetect)
	   (condition-case ()
	       (get-coding-system 'autodetect)
	     (error (setq code 'automatic-conversion)))))
     (decode-coding-region (point-min) (point-max) code)
     (set-file-coding-system code))
    (otherwise
     nil)))

(defun mule-inhibit-code-conversion (proc)
  (if (process-buffer proc)
      (save-excursion
	(set-buffer (process-buffer proc))
	(set 'mc-flag nil)
	(if (fboundp 'set-buffer-multibyte)
	    (set-buffer-multibyte nil)
	  (set 'enable-multibyte-characters nil))))
  (case mule-sysdep-version
    ((3.0 4.0 4.1 2.4 2.3)
     (set-process-coding-system proc mule-no-coding-system
				mule-no-coding-system))
    (xemacs
     (set-process-input-coding-system proc mule-no-coding-system)
     (set-process-input-coding-system proc mule-no-coding-system))))

(defun mule-write-region-no-coding-system (st nd file &optional append visit lockname)
  (let ((coding-system-for-write mule-no-coding-system)
	(file-coding-system mule-no-coding-system)
	(buffer-file-coding-system mule-no-coding-system)
	(mc-flag t)
	(require-final-newline nil)
	;;(file-name-handler-alist nil)
	(crypt-encoding-alist nil)
	(jka-compr-compression-info-list nil)
	(jam-zcat-filename-list nil)
	(write-file-hooks nil)
	(write-contents-hooks nil)
	(file-coding-system-alist nil))
    (case mule-sysdep-version
      (2.3
       (write-region st nd file append visit lockname mule-no-coding-system))
      (3.0
       (let ((enable-multibyte-characters t))
	 (write-region st nd file append visit lockname)))
      ((4.0 4.1)
       (write-region st nd file append visit lockname))
      (otherwise
       (write-region st nd file append visit lockname)))))

(defun mule-encode-string (str)
  (case mule-sysdep-version
    (2.3
     (code-convert-string str *internal* mule-retrieval-coding-system))
    ((2.4 3.0 xemacs)
     (encode-coding-string str mule-retrieval-coding-system))
    ((4.0 4.1)
     (if default-enable-multibyte-characters
	 (encode-coding-string str mule-retrieval-coding-system)
       str))
    (otherwise
     str)))

(defun mule-decode-string (str)
  (and str
       (case mule-sysdep-version
	 ((2.4 3.0 xemacs)
	  (decode-coding-string str mule-retrieval-coding-system))
	 (2.3
	  (code-convert-string str *internal* mule-retrieval-coding-system))
	 ((4.0 4.1)
	  (if default-enable-multibyte-characters
	      (decode-coding-string str mule-retrieval-coding-system)
	    str))
	 (otherwise
	  str))))

(defun mule-truncate-string (str len &optional pad)
  "Truncate string STR so that string-width of STR is not greater than LEN.
 If width of the truncated string is less than LEN, and if a character PAD is
 defined, add padding end of it."
  (case mule-sysdep-version
    ((3.0 4.0 4.1)
     (truncate-string-to-width str len 0 pad))
    (2.4
     (let ((cl (string-to-vector str)) (n 0) (sw 0))
       (if (<= (string-width str) len) str
	 (while (<= (setq sw (+ (char-width (aref cl n)) sw)) len)
	   (setq n (1+ n)))
	 (string-match (make-string n ?.) str)
	 (setq str (substring str 0 (match-end 0))))
       (if pad (concat str (make-string (- len (string-width str)) pad)) str)))
    (2.3
     (let ((cl (string-to-char-list str)) (n 0) (sw 0))
       (if (<= (string-width str) len) str
	 (while (<= (setq sw (+ (char-width (nth n cl)) sw)) len)
	   (setq n (1+ n)))
	 (string-match (make-string n ?.) str)
	 (setq str (substring str 0 (match-end 0))))
       (if pad (concat str (make-string (- len (string-width str)) pad)) str)))
    (otherwise
     (concat (if (> (length str) len) (substring str 0 len) str)
	     (if (or (null pad) (> (length str) len))
		 ""
	       (make-string (- len (length str)) pad))))))

(defun mule-find-charset-region (beg end &optional table)
  (case mule-sysdep-version
    (2.3 (code-detect-region beg end))
    ((2.4 3.0 4.0 4.1) (find-charset-region beg end table))
    (xemacs (charsets-in-region beg end))
    (otherwise '(no-conversion))))

(defun mule-coding-system-name (codesys)
  (case mule-sysdep-version
    ((3.0 4.0 4.1) nil)
    (xemacs (coding-system-name codesys))))

(defun mule-find-coding-system (sys)
  (case mule-sysdep-version
    ((2.3 2.4) nil)
    ((3.0 4.0 4.1) (if (get sys 'coding-system) sys nil))
    (xemacs (find-coding-system sys))
    (otherwise nil)))
     
(defun mule-make-iso-character (char)
  (if (<= char 127)
      char
    (case mule-sysdep-version
      (2.3 (make-character lc-ltn1 char))
      (2.4 (make-char charset-latin-iso8859-1 char))
      (3.0 (make-char 'latin-iso8859-1 char))
      ((4.0 4.1) (if default-enable-multibyte-characters
		     (make-char 'latin-iso8859-1 char)
		   char))
      (xemacs char)
      (otherwise char))))

(case mule-sysdep-version
  ((2.3 2.4 3.0 4.0 4.1 xemacs) nil)
  (otherwise (fset 'string-width 'length)))

(and
 (boundp 'MULE)
 (not (featurep 'mule))
 (provide 'mule))

(provide 'mule-sysdp)
