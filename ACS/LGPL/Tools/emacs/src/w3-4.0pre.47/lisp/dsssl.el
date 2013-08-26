;;; dsssl.el --- DSSSL parser
;; Author: wmperry
;; Created: 1998/12/18 02:19:24
;; Version: 1.1.1.2
;; Keywords: 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1996, 1997 by William M. Perry <wmperry@cs.indiana.edu>
;;; Copyright (c) 1997 - 1999 by Free Software Foundation, Inc.
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

(require 'cl)
(require 'dsssl-flow)

(if (not (fboundp 'cl-copy-hashtable))
    (defun cl-copy-hashtable (h)
      (let ((new (make-hash-table)))
	(cl-maphash (function (lambda (k v) (cl-puthash k v new))) h)
	new)))

(defconst dsssl-builtin-functions
  '(not boolean\?  case equal\?  null\?  list\?  list length append
	reverse list-tail list-ref member symbol\?  keyword\?  quantity\?
	number\?  real\?  integer\?  = < > <= >= + * - / max min abs quotient
	modulo remainder floor ceiling truncate round number->string
	string->number char\?  char=\?  char-property string\?  string
	string-length string-ref string=\?  substring string-append
	procedure\?  apply external-procedure make time time->string quote
	char-downcase identity error let)
  "A list of all the builtin DSSSL functions that we support.")

(defsubst dsssl-check-args (args expected)
  ;; Signal an error if we don't have the expected # of arguments
  (or (= (length args) expected)
      (error "Wrong # arguments (expected %d): %d" expected (length args))))

(defsubst dsssl-min-args (args min)
  (or (>= (length args) min)
      (error "Wrong # arguments (expected at least %d): %d" min
	     (length args))))

(defun dsssl-call-function (func args)
  (declare (special defines units))
  (let ((old-defines nil)
	(old-units nil)
	(func-args (nth 1 func))
	(real-func (nth 2 func))
	(retval nil))
    ;; Make sure we got the right # of arguments
    (dsssl-check-args args (length func-args))

    ;; make sure we evaluate all the arguments in the old environment
    (setq args (mapcar 'dsssl-eval args))

    ;; Save the old environment
    (setq old-defines (cl-copy-hashtable defines)
	  old-units (cl-copy-hashtable units))
    
    ;; Create the function's environment
    (while func-args
      (cl-puthash (car func-args) (car args) defines)
      (setq func-args (cdr func-args)
	    args (cdr args)))

    ;; Now evaluate the function body, returning the value of the last one
    (while real-func
      (setq retval (dsssl-eval (car real-func))
	    real-func (cdr real-func)))

    ;; Restore the previous environment
    (setq defines old-defines
	  units old-units)

    ;; And we are out of here baby!
    retval))

(defun dsssl-eval (form)
  ;; We expect to have a 'defines' and 'units' hashtable floating around
  ;; from higher up the call stack.
  (declare (special defines units))
  (cond
   ((consp form)			; A function call
    (let ((func (car form))
	  (args (cdr form)))
      (case func
	(cons
	 (dsssl-check-args args 2)
	 (cons (dsssl-eval (pop args)) (dsssl-eval (pop args))))
	(cdr
	 (dsssl-check-args args 1)
	 (cdr (dsssl-eval (pop args))))
	(car
	 (dsssl-check-args args 1)
	 (car (dsssl-eval (pop args))))
	(not
	 (dsssl-check-args args 1)
	 (not (dsssl-eval (car args))))
	(boolean\?
	 (dsssl-check-args args 1)
	 (and (symbolp (car args))
	      (memq (car args) '(\#f \#t))))
	(if
	 (dsssl-min-args args 2)
	 (let ((val (dsssl-eval (pop args))))
	   (if val
	       (dsssl-eval (nth 0 args))
	     (if (nth 1 args)
		 (dsssl-eval (nth 1 args))))))
	(let				; FIXME
	 )
	(case
	 (dsssl-min-args args 2)
	 (let* ((val (dsssl-eval (pop args)))
		(conditions args)
		(done nil)
		(possibles nil)
		(cur nil))
	   (while (and conditions (not done))
	     (setq cur (pop conditions)
		   possibles (nth 0 cur))
	     (if (or (and (listp possibles)
			  (member val possibles))
		     (equal val possibles)
		     (memq possibles '(default otherwise)))
		 (setq done (dsssl-eval (nth 1 cur)))))
	   done))
	(equal\?
	 (dsssl-check-args args 2)
	 (equal (dsssl-eval (car args)) (dsssl-eval (cadr args))))
	(null\?
	 (dsssl-check-args args 1)
	 (null (dsssl-eval (car args))))
	(list\?
	 (dsssl-check-args args 1)
	 (listp (dsssl-eval (car args))))
	(list
	 (mapcar 'dsssl-eval args))
	(length
	 (dsssl-check-args args 1)
	 (length (dsssl-eval (car args))))
	(append
	 (apply 'append (mapcar 'dsssl-eval args)))
	(reverse
	 (dsssl-check-args args 1)
	 (reverse (dsssl-eval (car args))))
	(list-tail
	 (dsssl-check-args args 2)
	 (nthcdr (dsssl-eval (car args)) (dsssl-eval (cadr args))))
	(list-ref
	 (dsssl-check-args args 2)
	 (nth (dsssl-eval (car args)) (dsssl-eval (cadr args))))
	(member
	 (dsssl-check-args args 2)
	 (member (dsssl-eval (car args)) (dsssl-eval (cadr args))))
	(symbol\?
	 (dsssl-check-args args 1)
	 (symbolp (dsssl-eval (car args))))
	(keyword\?
	 (dsssl-check-args args 1)
	 (keywordp (dsssl-eval (car args))))
	(quantity\?
	 (dsssl-check-args args 1)
	 (error "%s not implemented yet." func))
	(number\?
	 (dsssl-check-args args 1)
	 (numberp (dsssl-eval (car args))))
	(real\?
	 (dsssl-check-args args 1)
	 (let ((rval (dsssl-eval (car args))))
	   (and (numberp rval)
		(/= (truncate rval) rval))))
	(integer\?
	 (dsssl-check-args args 1)
	 (let ((rval (dsssl-eval (car args))))
	   (and (numberp rval)
		(= (truncate rval) rval))))
	((= < > <= >=)
	 (dsssl-min-args args 2)
	 (let ((not-done t)
	       (initial (dsssl-eval (car args)))
	       (next nil))
	   (setq args (cdr args))
	   (while (and args not-done)
	     (setq next (dsssl-eval (car args))
		   args (cdr args)
		   not-done (funcall func initial next)
		   initial next))
	   not-done))
	((+ *)
	 (dsssl-min-args args 2)
	 (let ((acc (dsssl-eval (car args))))
	   (setq args (cdr args))
	   (while args
	     (setq acc (funcall func acc (dsssl-eval (car args)))
		   args (cdr args)))
	   acc))
	(-
	 (dsssl-min-args args 1)
	 (apply func (mapcar 'dsssl-eval args)))
	(/
	 (dsssl-min-args args 1)
	 (if (= (length args) 1)
	     (/ 1 (dsssl-eval (car args)))
	   (apply func (mapcar 'dsssl-eval args))))
	((max min)
	 (apply func (mapcar 'dsssl-eval args)))
	(abs
	 (dsssl-check-args args 1)
	 (abs (dsssl-eval (car args))))
	(quotient			; FIXME
	 (error "`%s' not implemented yet!" func))
	(modulo
	 (dsssl-check-args args 2)
	 (mod (dsssl-eval (car args)) (dsssl-eval (cadr args))))
	(remainder
	 (dsssl-check-args args 2)
	 (% (dsssl-eval (car args)) (dsssl-eval (cadr args))))
	((floor ceiling truncate round)
	 (dsssl-check-args args 1)
	 (funcall func (dsssl-eval (car args))))
	(number->string
	 (dsssl-min-args args 1)
	 (if (= (length args) 1)
	     (number-to-string (dsssl-eval (car args)))
	   (if (= (length args) 2)	; They gave us a radix
	       (error "Radix arg not supported yet.")
	     (dsssl-check-args args 1))))
	(string->number
	 (dsssl-min-args args 1)
	 (if (= (length args) 1)
	     (string-to-number (dsssl-eval (car args)))
	   (if (= (length args) 2)	; They gave us a radix
	       (error "Radix arg not supported yet.")
	     (dsssl-check-args args 1))))
	(char\?
	 (dsssl-check-args args 1)
	 (characterp (dsssl-eval (car args))))
	(char=\?
	 (dsssl-check-args args 2)
	 (char-equal (dsssl-eval (car args)) (dsssl-eval (cadr args))))
	(char-downcase
	 (dsssl-check-args args 1)
	 (downcase (dsssl-eval (car args))))
	(char-property			; FIXME
	 (error "`%s' not implemented yet!" func))
	(string\?
	 (dsssl-check-args args 1)
	 (stringp (dsssl-eval (car args))))
	(string
	 (dsssl-min-args args 1)
	 (mapconcat 'char-to-string (mapcar 'dsssl-eval args) ""))
	(string-length
	 (dsssl-check-args args 1)
	 (length (dsssl-eval (car args))))
	(string-ref
	 (dsssl-check-args args 2)
	 (aref (dsssl-eval (car args)) (dsssl-eval (cadr args))))
	(string=\?
	 (dsssl-check-args args 2)
	 (string= (dsssl-eval (car args)) (dsssl-eval (cadr args))))
	(substring
	 (substring (dsssl-eval (pop args))
		    (dsssl-eval (pop args))
		    (dsssl-eval (pop args))))
	(string-append
	 (let ((rval ""))
	   (while args
	     (setq rval (concat rval (dsssl-eval (pop args)))))
	   rval))
	(procedure\?
	 (dsssl-check-args args 1)
	 (let* ((sym (dsssl-eval (car args)))
		(def (cl-gethash sym defines)))
	   (or (memq sym dsssl-builtin-functions)
	       (and def (listp def) (eq (car def) 'lambda)))))
	(apply				; FIXME
	 )
	(external-procedure		; FIXME
	 )
	(make
	 (let* ((type (dsssl-eval (pop args)))
		(symname nil)
		(props nil)
		(tail nil)
		(children nil)
		(temp nil)
		)
	   ;; Massage :children into the last slot
	   (setq props (mapcar 'dsssl-eval args)
		 tail (last props)
		 children (car tail))
	   (if (consp tail) 
	       (setcar tail nil))
	   (if (not (car props))
	       (setq props nil))
	   (setq temp (- (length props) 1))
	   ;; Not sure if we should really bother with this or not, but
	   ;; it does at least make it look more common-lispy keywordish
	   ;; and such.  DSSSL keywords look like font-weight:, this makes
	   ;; it :font-weight
	   (while (>= temp 0)
	     (setq symname (symbol-name (nth temp props)))
	     (if (string-match "^\\(.*\\):$" symname)
		 (setf (nth temp props) 
		       (intern (concat ":" (match-string 1 symname)))))
	     (setq temp (- temp 2)))

	   ;; Create the actual flow object
	   (make-flow-object :type type
			     :children children
			     :properties props)
	   )
	 )
	(time
	 (mapconcat 'int-to-string (current-time) ":"))
	(time->string
	 (dsssl-check-args args 1)
	 (current-time-string
	  (mapcar 'string-to-int
		  (split-string (dsssl-eval (car args)) ":"))))
	(quote
	 (dsssl-check-args args 1)
	 (car args))
	(identity
	 (dsssl-check-args args 1)
	 (dsssl-eval (car args)))
	(error
	 (apply 'error (mapcar 'dsssl-eval args)))
	(otherwise
	 ;; A non-built-in function - look it up
	 (let ((def (cl-gethash func defines)))
	   (if (and def (listp def) (eq (car def) 'lambda))
	       (dsssl-call-function def args)
	     (error "Symbol's function definition is void: %s" func))))
	)
      )
    )
   ((symbolp form)			; A variable
    ;; A DSSSL keyword!
    (if (string-match ":$" (symbol-name form))
	form
      (let ((val (cl-gethash form defines 'ThIS-Is_A_BOgUs-VariuhhBBLE)))
	(if (not (eq val 'ThIS-Is_A_BOgUs-VariuhhBBLE))
	    val
	  ;; Ok, we got a bogus variable, but maybe it is really a UNIT
	  ;; dereference.  Check.
	  (let ((name (symbol-name form))
		(the-units nil)
		(number nil)
		(conversion nil))
	    (if (not (string-match "^\\([0-9.]+\\)\\([a-zA-Z]+\\)$" name))
		(error "Symbol's value as variable is void: %s" form)
	      (setq number (string-to-int (match-string 1 name))
		    the-units  (intern (downcase (match-string 2 name)))
		    conversion (cl-gethash the-units units))
	      (if (or (not conversion) (not (numberp conversion)))
		  (error "Symbol's value as variable is void: %s" form)
		(* number conversion))))))))
   (t
    form)
   )
  )

(defsubst dsssl-predeclared ()
  (declare (special defines units))
  (cl-puthash '\#f nil defines)
  (cl-puthash 'nil nil defines)
  (cl-puthash '\#t t defines)
  ;; NOTE: All units are stored internally as points.
  (cl-puthash 'in (float 72) units)
  (cl-puthash 'mm (float (* 72 25.4)) units)
  (cl-puthash 'cm (float (* 72 2.54)) units)
  )

(defun dsssl-parse (buf)
  ;; Return the full representation of the DSSSL stylesheet as a series
  ;; of LISP objects.
  (let ((defines (make-hash-table :size 13))
	(units   (make-hash-table :size 13))
	(buf-contents nil))
    (dsssl-predeclared)
    (save-excursion
      (setq buf-contents (if (or (bufferp buf) (get-buffer buf))
			     (progn
			       (set-buffer buf)
			       (buffer-string))
			   buf))
      (set-buffer (generate-new-buffer " *dsssl-style*"))
      (insert buf-contents)
      (goto-char (point-min))
      (skip-chars-forward " \t\n\r")
      (if (looking-at "<!")		; DOCTYPE present
	  (progn
	    ;; This should _DEFINITELY_ be smarter
	    (search-forward ">" nil t)
	    ))
      (let ((result nil)
	    (temp nil)
	    (save-pos nil))
	(while (not (eobp))
	  (condition-case ()
	      (setq save-pos (point)
		    temp (read (current-buffer)))
	    (invalid-read-syntax
	     ;; This disgusting hack is in here so that we can basically
	     ;; extend the lisp reader to gracefully deal with converting
	     ;; DSSSL #\A to Emacs-Lisp ?A notation.  If you know of a
	     ;; better way, please feel free to send me some email.
	     (setq temp nil)
	     (backward-char 1)
	     (if (looking-at "#\\\\")
		 (replace-match "?")
	       (insert "\\"))
	     (goto-char save-pos))
	    (error nil))
	  (cond
	   ((null temp)
	    nil)
	   ((listp temp)
	    (case (car temp)
		  (define-unit
		    (cl-puthash (cadr temp) (dsssl-eval (caddr temp))
				units))
		  (define
		    (if (listp (cadr temp))
			;; A function
			(cl-puthash (caadr temp)
				     (list 'lambda
					   (cdadr temp)
					   (cddr temp)) defines)
		      ;; A normal define
		      (cl-puthash (cadr temp)
				   (dsssl-eval (caddr temp)) defines)))
		  (otherwise
		   (setq result (cons temp result)))))
	   (t
	    (setq result (cons temp result))))
	  (skip-chars-forward " \t\n\r"))
	(kill-buffer (current-buffer))
	(list defines units (nreverse result))))))

(defun dsssl-test (x)
  (let* ((result (dsssl-parse x))
	 (defines (nth 0 result))
	 (units   (nth 1 result))
	 (forms   (nth 2 result)))
    (mapcar 'dsssl-eval forms)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The flow object classes.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro flow-object-property (obj prop &optional default)
  "Return property PROP of the DSSSL flow object OBJ.
OBJ can be any flow object class, as long as it was properly derived
from the base `flow-object' class."
  (` (plist-get (flow-object-properties (, obj)) (, prop) (, default))))

;; Now for specific types of flow objects
;; Still to do:
;;; display-group
;;; paragraph
;;; sequence
;;; line-field
;;; paragraph-break
;;; simple-page-sequence
;;; score
;;; table
;;; table-row
;;; table-cell
;;; rule
;;; external-graphic


(provide 'dsssl)
