;; OCCAM mode for GNU emacs at DIKU
;; by Jesper Larsson Traff, DIKU autumn 1989.

;; ***changes added by jcs, autumn 1993
;; automatically add colons to declarations if necessary
;; added more key words
;; cleaned up the indentation scheme.
;; 1) added the function occam-move-region which indents/unindents a block of
;;   text the number of spaces specified by a prefix argument
;; 2) added feature to show the keyword in the current scope(similar to
;;    blink matching paren) when backspacing
;; 3) added hillit functionality to occam mode (auto highlight of keywords, etc.)
;; requires emacs 19- to use this feature

;; Copyright (C) Jesper Larsson Traff and DIKU

;; LCD Archive Entry:
;; occam-mode|Jesper Larsson Traff||
;; OCCAM programing mode.|
;; 89-09||~/modes/occam-mode.el.Z|

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;;any expression which evaluates to a truth value
(defconst if-reg-exp "[^:\n]*\\(>\\|<\\|TRUE\\|FALSE\\|[^:]=\\|AND\\|OR\\|NOT\\).*")

;; regular expression to determine whether to unindent after a case
;; keyword
(defconst case-reg-exp "\\([^\n]*;[^\n]*\\|[ \t]*[a-zA-Z1-9\\.]+\\([ \t]*--[^\n]*\\|[ \t]*$\\)\\)")

(defconst alt-reg-exp "[^\n]* \\? [^\n]*")

(defconst occam-indent 2 
  "*OCCAM standard indentation (do not change!)")

(defvar choice-level 0)

;; words which signal that a colon is needed at the end of this line.
(defconst col-words
  '("CHAN"
    "PLACE"
    "["
    "VAL"
    "BOOL"
    "TIMER"
    "INT" "INT16""INT32" "INT64"  ; integer declarations
    "REAL" "REAL32" "REAL64"      ; real declarations
    "BYTE"))

(defconst occam-process-keywords
  '("SEQ"                         ; sequential process
    "PAR"                         ; parallel process
    "IF"                          ; conditional process
    "ALT"                         ; alternative (special) process
    "WHILE"                       ; iterative process
    "CASE"                        ; selection process
    "VALOF"
    "PROC"
    "PROCESSOR"
    "PLACED"
    "PRI"
    )
  "*OCCAM proccess keywords")

(defconst occam-reserved-words
  '("INT" "INT16""INT32" "INT64"  ; integer declarations
    "REAL" "REAL32" "REAL64"      ; real declarations
    "BYTE"                        ; byte (character) declaration
    "BOOL" "TRUE" "FALSE"         ; boolean declaration and constants
    "CHAN"                        ; channel declaration
    "OF"
    "PROTOCOL"                    ; protocol declaration
    "TIMER"                       ; timer declaration
    "VAL"
    "IS"
    "PLUS"
    "RESULT"
    "FOR"                         ; replicator keyword
    "RETYPES"
    "AT"
    "SIZE"                        ; size operator
    "FROM"                        ; array selector keyword
    "SKIP" "STOP"                 ; special processes
    "INCLUDE"
    "USE"
    "PLACE"
    "REM"

    )
  "*OCCAM reserved words (will be capitalized)")

(defvar occam-mode-syntax-table nil
  "Syntax table in use in OCCAM mode buffers")

(if occam-mode-syntax-table
    ()
  (setq occam-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\? "." occam-mode-syntax-table)
  (modify-syntax-entry ?\! "." occam-mode-syntax-table)
  (modify-syntax-entry ?\: "." occam-mode-syntax-table)
  ;;(modify-syntax-entry ?. "w" occam-mode-syntax-table)
  (modify-syntax-entry ?\\ "\\" occam-mode-syntax-table)
  (modify-syntax-entry ?/ "." occam-mode-syntax-table)
  (modify-syntax-entry ?* "." occam-mode-syntax-table)
  (modify-syntax-entry ?+ "." occam-mode-syntax-table)
  (modify-syntax-entry ?- "." occam-mode-syntax-table)
  (modify-syntax-entry ?= "." occam-mode-syntax-table)
  (modify-syntax-entry ?< "." occam-mode-syntax-table)
  (modify-syntax-entry ?> "." occam-mode-syntax-table)
  (modify-syntax-entry ?& "." occam-mode-syntax-table)
  (modify-syntax-entry ?| "." occam-mode-syntax-table)
  (modify-syntax-entry ?~ "." occam-mode-syntax-table)
  ;;comment start is the string "--"
  (modify-syntax-entry ?- "< 12" occam-mode-syntax-table)
  ;comment end is return
  (modify-syntax-entry ?\n ">" occam-mode-syntax-table)
  (modify-syntax-entry ?\t " " occam-mode-syntax-table)
  (modify-syntax-entry ?  " " occam-mode-syntax-table)
  (modify-syntax-entry ?\' "\"" occam-mode-syntax-table))

(defvar occam-mode-map ()
  "Keymap used in OCCAM mode")

(if occam-mode-map
    ()
  (setq occam-mode-map (make-sparse-keymap))
  (define-key occam-mode-map " " 'uppercase-occam-keyword)
  ;;inserts a colon and indents correctly if needed
  (define-key occam-mode-map ":" 'occam-insert-col)
  (define-key occam-mode-map "\r" 'occam-indent-newline)
  ;;(define-key occam-mode-map "\t" 'indent-according-to-mode)
  (define-key occam-mode-map "" (quote occam-comment-region))
  (define-key occam-mode-map "m" (quote occam-move-region))
  (define-key occam-mode-map "\177" 'backward-delete-unindent))


(defun occam-mode ()
  "Major mode for editing OCCAM programs.
TAB and CR automatically indents.
All OCCAM keywords (which are separated into process keywords which force 
indentation and reserved words) are recognized and uppercase'd.

Variables and constants controlling case change:
    occam-indentation :      indentation, default 2
    occam-process-keywords : list of process keywords
    occam-reserved-words :   list of reserved words

The value of the variable occam-mode-hook (must be a function name) is called 
with no arguments prior to entering  OCCAM mode if the value of that variable
is non-nil"
  (interactive)
  (kill-all-local-variables)
  (use-local-map occam-mode-map)
  (make-local-variable 'indent-line-function)
  ;;(setq indent-line-function 'occam-user-indent)
  (setq indent-line-function 'my-indent)
  (setq mode-name "OCCAM")
  (setq major-mode 'occam-mode)
  ;;take care of commenting regions
  (make-local-variable 'comment-start)
  (setq comment-start "--")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "--+ *")
  (make-local-variable 'comment-indent-function)
  (make-local-variable 'last-keyword-data)
  (setq last-keyword-data nil)
  (setq comment-indent-function 'occam-comment-indent)
  (run-hooks 'occam-mode-hook))

(defun occam-user-indent()
  (interactive)
  (insert "  "))

(defun occam-indent-line (indent-level)
  "Indents current OCCAM line"
  (save-excursion
    (beginning-of-line)
    (let ((p (point)))
      (insert-char 32 indent-level)
       ;;(indent-to indent-level)
      ;;delete any white space user may have entered
      (while (looking-at "[ \t]")
	(delete-char 1))
      (untabify p (point)))))

;;if there is a keyword on the current line returns that word
;;otherwise return null
(defun get-key-word-this-line()
  ;;first check to see if the word we are looking at is a keyword
  (save-excursion
    (let ((poss (current-word)))
      ;;make sure the word is not in a comment or this is a blank line
      (if (looking-at "\\([ \t]*$\\)\\|\\([ \t]*--\\)")
	  nil
	(if (word-in-list poss occam-process-keywords)
	    poss
	  (progn
	    ;;first check to see if a function
	    (forward-word 1)
	    (setq poss (current-word))
	    (if
		(string-equal poss "FUNCTION")
		poss
	      ;;now check the end of the line to see if case is there
	      (end-of-line)
	      (forward-word -1)
	      (setq poss (current-word))
	      (if
		  (string-equal poss "CASE")
		  poss
		nil))))))))

;; returns a dotted pair of the keyword and info as to whether or not it is
;; a new-word (immediately proceeding) or an old world (further back)
;; info concerning whether it may need to be unindented or whether it
;; there is no chance of unindenting

(defun get-last-keyword ()
  (interactive)
  (save-excursion
    (let ((keyword nil)(done nil)(current-indent-level)(last-indent-level))
      (beginning-of-line)
      (back-to-indentation)
      (setq current-indent-level (current-column))
      ;;first check to see if this line has a keyword in it
      (setq keyword (get-key-word-this-line))
      (if keyword (list 'new-word current-indent-level keyword)
	(while (and (null keyword) (not done))
	  (setq last-indent-level (current-column))
	  ;;skip everything which is indented more than the last command
	  (if (< last-indent-level current-indent-level)
	      (setq keyword (get-key-word-this-line)))
	  (if (bobp) (setq done 't)
	    (beginning-of-line 0)
	    (back-to-indentation)))
	(list 'old-word last-indent-level keyword)))))

(defun calculate-occam-indent ()
  "calculate indentation for current OCCAM line"
  (interactive)
    (save-excursion
      (setq last-keyword-data (get-last-keyword))
      (let ((last-keyword-type (car last-keyword-data))
	    (indentation (car (cdr last-keyword-data)))
	    (last-keyword (car (cdr (cdr last-keyword-data)))))
	(cond
	 ((eq last-keyword-type 'new-word)
	  ;;indent 2 spaces from last no matter what the keyword is
	  (+ 2 indentation))
	 ((string-equal last-keyword "IF")
	  ;;indent 4 spaces / possibly remove 2 spaces when the syntax
	  ;;of the current line is read
	  (+ 4 indentation))
	 ((string-equal last-keyword "CASE")
	  (+ 4 indentation))
	 ((string-equal last-keyword "ALT")
	  (+ 4 indentation))
	 ((null last-keyword) 0)
	 (t (+ 2 indentation))
	 ))))

(defun looking-at-word-in-list-p(list)
  (save-excursion
    (let (bow eow)
      (progn
	(beginning-of-line)
	(word-in-list (current-word)
		      list)))))

(defun current-word()
  (save-excursion
    (let ((b))
      (forward-word 1)
      (setq b (point))
      (forward-word -1)
      (buffer-substring b (point)))))

(defun uppercase-occam-keyword ()
  "check if last word was an OCCAM keyword"
  (interactive)
  (occam-keyword (append occam-process-keywords occam-reserved-words))
    (insert " "))

(defun occam-insert-col()
  (interactive)
  (let ((nulllineflag nil)(eolflag nil))
    (if (eolp)
	(progn 
	  (setq eolflag 't)
	  (save-excursion
	    (beginning-of-line)
	    (if (looking-at "[ \t]*$")
		(setq nulllineflag 't)))))
    (if
	(and eolflag nulllineflag)
	(progn
	  (kill-line 0)
	  (insert ":\n\n"))
      (if eolflag
	  (progn
	    (end-of-line)
	    (insert ":"))
	(insert ":")))
))
    

(defun occam-move-region (arg)
  (interactive "P")
  (if (< (point) (mark))
      (beginning-of-line)
    (exchange-point-and-mark)
    (beginning-of-line))
  (save-excursion
    (save-restriction
      (let ((a (prefix-numeric-value arg)))
	(call-interactively 'narrow-to-region)
	(goto-char (point-min))
	(while (not (eobp))
	  (beginning-of-line)
	  (if (> a 0)
	      (insert-char 32 a)
	    (delete-char (- a)))
	  (beginning-of-line 2))))))

;; indent to the same level as the line directly above this line
;; so this function just returns the amount the above line
;; is indented to.
(defun occam-comment-indent ()
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (if (looking-at-word-in-list-p occam-process-keywords)
	(+ 2 (current-column))
      (current-column))))
    
;;if given an arg uncomment the region
;;if no arg, comment the region

(defun occam-comment-region (&optional arg)
  (interactive "P")
  (if (< (point) (mark))
      (beginning-of-line)
    (exchange-point-and-mark)
    (beginning-of-line))
  (cond ((null arg)
	 (save-excursion
	   (save-restriction
	     (call-interactively 'narrow-to-region)
	     (goto-char (point-min))
	     (skip-chars-forward " \t")
	     (let ((col (current-column)))
	       (beginning-of-line)
	       (while (not (eobp))
		 (indent-to  col)
		 (insert "--")
		 ;;delete any white space usepr may have entered
		 (beginning-of-line 2))))))
	((equal (prefix-numeric-value arg) 1)
	 (save-excursion
	   (save-restriction
	     (call-interactively 'narrow-to-region)
	     (goto-char (point-min))
	     (while (re-search-forward "^[ \t]*--" nil t)
	       (replace-match "" nil nil)
	       (beginning-of-line 2)))))
	((equal (prefix-numeric-value arg) 4)
	 (save-excursion
	   (save-restriction
	     (call-interactively 'narrow-to-region)
	     (goto-char (point-min))
	     (while (re-search-forward "^[ \t]*--" nil t)
	       (replace-match "" nil nil)
	       ))))
	))

(defun my-indent ()
  (interactive)
  ;;delete any white space the user may have typed in
  (beginning-of-line)
  (while (looking-at "[ \t]")
    (delete-char 1))
  (end-of-line 0)
  (occam-indent-newline)
  (kill-line))

(defun my-indent ()
  (interactive)
  ;;delete any white space the user may have typed in
  (let ((ind-level 0))
    (setq ind-level (calculate-occam-indent))
    (beginning-of-line)
    (while (looking-at "[ \t]")
      (delete-char 1))
    (occam-indent-line ind-level)))

(defun my-indent ()
  (interactive)
  ;;delete any white space the user may have typed in
  (let ((ind-level 0))
    (end-of-line 0)
    (setq ind-level (calculate-occam-indent))
    (beginning-of-line 2)
    (while (looking-at "[ \t]")
      (delete-char 1))
    (occam-indent-line ind-level)
    (while (looking-at "[ \t]")
      (forward-char 1))))




(defun occam-indent-newline ()
  "In default case indent new line to current indentation but:
   1) if this line contained a process keyword -> indent 2 spaces for the next line
   3) if this line is after if/case/alt -> indent 2 spaces
   2) if this line is an expression and not right after if/case/alt 
      ->unindent this line 2 spaces.
"
  (interactive)
  (if (not (eolp))
      (let ((ind (calculate-occam-indent)))
	(newline)
	(occam-indent-line ind)
	(while (looking-at "[ \t]")
	  (forward-char 1))
	)
  ;;uppercase the word if it is a keyword
    (occam-keyword (append occam-process-keywords occam-reserved-words))
    (let ((needed-punc nil) (current-indent-level 0) 
	  (last-keyword-type (car last-keyword-data))
	  (next-line-indent-level (calculate-occam-indent)) 
	  (keyword-type (car last-keyword-data))
	  (keyword-indent-level (car (cdr last-keyword-data)))
	  (last-keyword (car (cdr (cdr last-keyword-data)))))
      (save-excursion
	(let ((eol (point))(eow nil)(bow nil))
	  (beginning-of-line)
	  (if (or (looking-at ".*--") (looking-at "[ \t]*$")) 
	      (setq needed-punc 'none))
	  (back-to-indentation)
	  (setq current-indent-level (current-column))
	  (forward-word 1)
	  (setq eow (point))
	  (forward-word -1)
	  (setq bow (point))
	  (cond 
	   ((eq needed-punc 'none))
	   ((string= (buffer-substring bow eow) "PROC")
	    (setq needed-punc 'parens))
	   ((word-in-list (upcase (buffer-substring bow eow))
			  col-words)
	    (setq needed-punc 'colon))
	   ;; check to see if this line should be unindented
	   ;; this is true iff the last keyword was an if case or
	   ;; alt *and* this line matches the appropriate regexp.
	   ((and
	     (= (- current-indent-level keyword-indent-level) 4)
	     ;;neither of the last two keywords(this line or the line
	     ;;before can be new
	     (eq keyword-type 'old-word)
	     (eq last-keyword-type 'old-word)
	     (or
	      (and (string-equal last-keyword "IF")
		   (looking-at if-reg-exp))
	      (and (string-equal last-keyword "CASE")
		   (looking-at case-reg-exp))
	      (and (string-equal last-keyword "ALT")
		   (looking-at alt-reg-exp))))
	    (setq needed-punc 'unindent)))
	  (if (looking-at "[ \t]*$")
	      (delete-region (point) eol)
	    ())))
      ;;add a ":" if necessary
      (if (and
	   (eq needed-punc 'colon)
	   (not (string-equal ":" (buffer-substring (1- (point)) (point)))))
	  (insert " :")
	;;add parens if necessary
      (if (and
	   (eq needed-punc 'parens)
	   (not (string-equal ")" (buffer-substring (1- (point)) (point)))))
	  (insert " ()")
	(if (eq needed-punc 'unindent)
	    (progn
	      (beginning-of-line)
	      (delete-char 2)
	      (end-of-line)))))
      (newline)
      (occam-indent-line next-line-indent-level)
      (end-of-line))))

;;this function moves the cursor momentarily to the keyword of the current
;;scope
(defun blink-new-keyword ()
  (save-excursion
    ;;now move the cursor to the last keyword
    (let ((keyword nil)(done nil)(current-indent-level)(last-indent-level))
      (setq current-indent-level (current-column))
      (while (and (null keyword) (not done))
	(beginning-of-line 0)
	(back-to-indentation)
	(setq last-indent-level (current-column))
	;;skip everything which is indented more than the last command
	(if (< last-indent-level current-indent-level)
	    (setq keyword (get-key-word-this-line)))
	(if (bobp) (setq done 't)))
      ;;after found keyword, flash it
      (back-to-indentation)
      (if (pos-visible-in-window-p)
	  (sit-for 1)
	;;if we can't see it then do this
		  (message
		   "Matches %s"
		   (if (save-excursion
			 (skip-chars-backward " \t")
			 (not (bolp)))
		       (buffer-substring (progn (beginning-of-line) (point))
					 (1+ (point)))
		     (buffer-substring (point)
				       (progn
					(forward-char 1)
					(skip-chars-forward "\n \t")
					(end-of-line)
					(point)))))))))

;;this function also blinks to the keyword which the current
;;line is in the scope of (following the delete.
(defun backward-delete-unindent ()
  "Delete and unindent"
  (interactive)
  (let ((p (point)))
    (skip-chars-backward " \t" (- p (current-column)))
    (if (bolp)
	(progn
	  (goto-char p)
	  (if (bolp)
	      (delete-char -1)
	    (delete-char (- occam-indent))
	    (blink-new-keyword)
	    ))
      (progn
	(goto-char p)
	(delete-char -1)))))

(defun occam-keyword (keywords)
  "upcase current word and return 't if it is an OCCAM keyword;
 return nil if it is not"
  (save-excursion
    (let ((eow (point)))
      (forward-word -1)
      (let ((bow (point)))
	(if (re-search-backward "\<" (- bow (current-column)) t)
	    nil
	  (if (word-in-list (upcase (buffer-substring bow eow))
			    keywords)
	      ;;if it is a keyword then
	      ;; if we are not in a comment uppercase 
	      (if (re-search-backward "--" (- (point) (current-column)) t)
		  't
		(not (upcase-region bow eow)))
	    nil))))))

(defun word-in-list (word words)
  "t if word occurs in words, nil otherwise"
  (if (null words)
      nil
    (if (string-equal word (car words))
	t
      (word-in-list word (cdr words)))))

(if (featurep 'hilit19)
  (hilit-set-mode-patterns
   'occam-mode
   '(;; comments
     ("--.*" nil comment)
     ;; main structure
     ("\\(PROC\\|FUNCTION\\)[^(]*" nil defun)
     ("[!?]" nil label)
     ("\\(CHAN\\|PLACE\\|VAL\\|BOOL\\|TIMER\\|INT\\|INT16\\|INT32\\|INT64\\|REAL\\|REAL32\\|REAL64\\|BYTE\\)[^:)(]+[:)]" nil define)
     ("\\(SEQ\\|PAR\\|IF\\|ALT\\|WHILE\\|CASE\\|VALOF\\|PROCESSOR\\|PLACED\\|PRI\\)" nil keyword)
     (":=" nil decl)
     ("\\(\\\[\\||\\|\\\]\\)" nil include))))

(setq auto-mode-alist (cons '("\\.occ$" . occam-mode) auto-mode-alist))
