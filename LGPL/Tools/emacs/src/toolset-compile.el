;; Run toolset compilers as inferior of Emacs, and parse its error messages.
;; Copyright (C) 1985, 1986 Free Software Foundation, Inc.
;; Modified by David Beckett 1992

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

(provide 'toolset-compile)

(defvar toolset-compilation-process nil
  "Process created by compile command, or nil if none exists now.
Note that the process may have been \"deleted\" and still
be the value of this variable.")

(defvar toolset-compilation-error-list nil
  "List of error message descriptors for visiting erring functions.
Each error descriptor is a list of length two.
Its car is a marker pointing to an error message.
Its cadr is a marker pointing to the text of the line the message is about,
  or nil if that is not interesting.
The value may be t instead of a list;
this means that the buffer of error messages should be reparsed
the next time the list of errors is wanted.")

(defvar toolset-compilation-parsing-end nil
  "Position of end of buffer when last error messages parsed.")

(defvar toolset-compilation-error-message nil
  "Message to print when no more matches for toolset-compilation-error-regexp are found")

;; The filename excludes colons to avoid confusion when error message
;; starts with digits.
;;  "\\([^ :\n]+\\(: *\\|, line \\|(\\)[0-9]+\\)\\|\\([0-9]+ *of *[^ \n]+\\)"
;;  \([^ :\n]+\(: *\|, line \|(\)[0-9]+\)\|\([0-9]+ *of *[^ \n]+\)
;;
;;                                       \|
;;  \([^ :\n]+                   [0-9]+\)  \([0-9]+ *of *[^ \n]+\)
;;            \(: *\|, line \|(\)
(defvar toolset-compilation-error-regexp
  "^[^-]+-[^-]+-[^(]+([0-9]+)"
  "Regular expression for filename/linenumber in error in compilation log.")

(defun toolset-compile (command)
  "Compile the program including the current buffer.  Default: run `make'.
Runs COMMAND, a shell command, in a separate process asynchronously
with output going to the buffer *compilation*.
You can then use the command \\[toolset-next-error] to find the next
error message and move to the source code that caused it."
  (interactive (list (read-string "Compile command: " toolset-compile-command)))
  (setq toolset-compile-command command)
  (toolset-compile1 toolset-compile-command "No more errors"))

(defun toolset-grep (command)
  "Run grep, with user-specified args, and collect output in a buffer.
While grep runs asynchronously, you can use the \\[toolset-next-error] command
to find the text that grep hits refer to."
  (interactive "sRun grep (with args): ")
  (toolset-compile1 (concat "grep -n " command " /dev/null")
	    "No more grep hits" "grep"))

(defun toolset-compile1 (command error-message &optional name-of-mode)
  (save-some-buffers)
  (if toolset-compilation-process
      (if (or (not (eq (process-status toolset-compilation-process) 'run))
	      (yes-or-no-p "A compilation process is running; kill it? "))
	  (condition-case ()
	      (let ((comp-proc toolset-compilation-process))
		(interrupt-process comp-proc)
		(sit-for 1)
		(delete-process comp-proc))
	    (error nil))
	(error "Cannot have two compilation processes")))
  (setq toolset-compilation-process nil)
  (toolset-compilation-forget-errors)
  (setq toolset-compilation-error-list t)
  (setq toolset-compilation-error-message error-message)
  (setq toolset-compilation-process
	(start-process "compilation" "*toolset-compilation*"
		       shell-file-name
		       "-c" (concat "exec " command)))
  (with-output-to-temp-buffer "*toolset-compilation*"
    (princ "cd ")
    (princ default-directory)
    (terpri)
    (princ command)
    (terpri))
  (set-process-sentinel toolset-compilation-process 'toolset-compilation-sentinel)
  (let* ((thisdir default-directory)
	 (outbuf (process-buffer toolset-compilation-process))
	 (outwin (get-buffer-window outbuf))
	 (regexp toolset-compilation-error-regexp))
    (if (eq outbuf (current-buffer))
	(goto-char (point-max)))
    (save-excursion
      (set-buffer outbuf)
      (buffer-flush-undo outbuf)
      (let ((start (save-excursion (set-buffer outbuf) (point-min))))
	(set-window-start outwin start)
	(or (eq outwin (selected-window))
	    (set-window-point outwin start)))
      (setq default-directory thisdir)
      (fundamental-mode)
      (make-local-variable 'toolset-compilation-error-regexp)
      (setq toolset-compilation-error-regexp regexp)
      (setq mode-name (or name-of-mode "Toolset Compilation"))
      ;; Make log buffer's mode line show process state
      (setq mode-line-process '(": %s")))))

;; Called when compilation process changes state.

(defun toolset-compilation-sentinel (proc msg)
  (cond ((null (buffer-name (process-buffer proc)))
	 ;; buffer killed
	 (set-process-buffer proc nil))
	((memq (process-status proc) '(signal exit))
	 (let* ((obuf (current-buffer))
		omax opoint)
	   ;; save-excursion isn't the right thing if
	   ;;  process-buffer is current-buffer
	   (unwind-protect
	       (progn
		 ;; Write something in *toolset-compilation* and hack its mode line,
		 (set-buffer (process-buffer proc))
		 (setq omax (point-max) opoint (point))
		 (goto-char (point-max))
		 (insert ?\n mode-name " " msg)
		 (forward-char -1)
		 (insert " at "
			 (substring (current-time-string) 0 -5))
		 (forward-char 1)
		 (setq mode-line-process
		       (concat ": "
			       (symbol-name (process-status proc))))
		 ;; If buffer and mode line will show that the process
		 ;; is dead, we can delete it now.  Otherwise it
		 ;; will stay around until M-x list-processes.
		 (delete-process proc))
	     (setq toolset-compilation-process nil)
	     ;; Force mode line redisplay soon
	     (set-buffer-modified-p (buffer-modified-p)))
	   (if (and opoint (< opoint omax))
	       (goto-char opoint))
	   (set-buffer obuf)))))

(defun toolset-kill-compilation ()
  "Kill the process made by the \\[toolset-compile] command."
  (interactive)
  (if toolset-compilation-process
      (interrupt-process toolset-compilation-process)))

(defun toolset-kill-grep ()
  "Kill the process made by the \\[toolset-grep] command."
  (interactive)
  (if toolset-compilation-process
      (interrupt-process toolset-compilation-process)))

(defun toolset-next-error (&optional argp)
  "Visit next compilation error message and corresponding source code.
This operates on the output from the \\[toolset-compile] command.
If all preparsed error messages have been processed,
the error message buffer is checked for new ones.
A non-nil argument (prefix arg, if interactive)
means reparse the error message buffer and start at the first error."
  (interactive "P")
  (if (or (eq toolset-compilation-error-list t)
	  argp)
      (progn (toolset-compilation-forget-errors)
	     (setq toolset-compilation-parsing-end 1)))
  (if toolset-compilation-error-list
      nil
    (save-excursion
      (set-buffer "*toolset-compilation*")
      (set-buffer-modified-p nil)
      (toolset-compilation-parse-errors)))
  (let ((next-error (car toolset-compilation-error-list)))
    (if (null next-error)
	(error (concat toolset-compilation-error-message
		       (if (and toolset-compilation-process
				(eq (process-status toolset-compilation-process)
				    'run))
			   " yet" ""))))
    (setq toolset-compilation-error-list (cdr toolset-compilation-error-list))
    (if (null (car (cdr next-error)))
	nil
      (switch-to-buffer (marker-buffer (car (cdr next-error))))
      (goto-char (car (cdr next-error)))
      (set-marker (car (cdr next-error)) nil))
    (let* ((pop-up-windows t)
	   (w (display-buffer (marker-buffer (car next-error)))))
      (set-window-point w (car next-error))
      (set-window-start w (car next-error)))
    (set-marker (car next-error) nil)))

;; Set toolset-compilation-error-list to nil, and
;; unchain the markers that point to the error messages and their text,
;; so that they no longer slow down gap motion.
;; This would happen anyway at the next garbage collection,
;; but it is better to do it right away.
(defun toolset-compilation-forget-errors ()
  (if (eq toolset-compilation-error-list t)
      (setq toolset-compilation-error-list nil))
  (while toolset-compilation-error-list
    (let ((next-error (car toolset-compilation-error-list)))
      (set-marker (car next-error) nil)
      (if (car (cdr next-error))
	  (set-marker (car (cdr next-error)) nil)))
    (setq toolset-compilation-error-list (cdr toolset-compilation-error-list))))

(defun toolset-compilation-parse-errors ()
  "Parse the current buffer as error messages.
This makes a list of error descriptors, toolset-compilation-error-list.
For each source-file, line-number pair in the buffer,
the source file is read in, and the text location is saved in
toolset-compilation-error-list. 
The function toolset-next-error, assigned to \\[toolset-next-error],
takes the next error off the list 
and visits its location."
  (setq toolset-compilation-error-list nil)
  (message "Parsing error messages...")
  (let (text-buffer
	last-filename last-linenum)
    ;; Don't reparse messages already seen at last parse.
    (goto-char toolset-compilation-parsing-end)
    ;; Don't parse the first two lines as error messages.
    ;; This matters for grep.
    (if (bobp)
	(forward-line 2))
    (while (re-search-forward toolset-compilation-error-regexp nil t)
      (let (linenum filename
	    error-marker text-marker)
	;; Extract file name and line number from error message.
	(save-restriction
	  (narrow-to-region (match-beginning 0) (match-end 0))
	  (goto-char (point-min))
	  (skip-chars-forward "^\\-")
	  (skip-chars-forward "-")
	  (skip-chars-forward "^\\-")
	  (skip-chars-forward "-")
	  (narrow-to-region (point) (match-end 0))
	  (setq filename (toolset-compilation-grab-filename))
	  (skip-chars-forward "^(")
	  (skip-chars-forward "(")
	  (setq linenum (read (current-buffer))))
	;; Locate the erring file and line.
	(if (and (equal filename last-filename)
		 (= linenum last-linenum))
	    nil
	  (beginning-of-line 1)
	  (setq error-marker (point-marker))
	  ;; text-buffer gets the buffer containing this error's file.
	  (if (not (equal filename last-filename))
	      (setq text-buffer
		    (and (file-exists-p (setq last-filename filename))
			 (find-file-noselect filename))
		    last-linenum 0))
	  (if text-buffer
	      ;; Go to that buffer and find the erring line.
	      (save-excursion
		(set-buffer text-buffer)
		(if (zerop last-linenum)
		    (progn
		      (goto-char 1)
		      (setq last-linenum 1)))
		;; Move the right number of lines from the old position.
		;; If we can't move that many, put 0 in last-linenum
		;; so the next error message will be handled starting from
		;; scratch.
		(if (eq selective-display t)
		    (or (re-search-forward "[\n\C-m]" nil 'end
					   (- linenum last-linenum))
			(setq last-linenum 0))
		  (or (= 0 (forward-line (- linenum last-linenum)))
		      (setq last-linenum 0)))
		(setq last-linenum linenum)
		(setq text-marker (point-marker))
		(setq toolset-compilation-error-list
		      (cons (list error-marker text-marker)
			    toolset-compilation-error-list)))))
	(forward-line 1)))
    (setq compilation-parsing-end (point-max)))
  (message "Parsing error messages...done")
  (setq toolset-compilation-error-list (nreverse toolset-compilation-error-list)))

(defun toolset-compilation-grab-filename ()
  "Return a string which is a filename, starting at point.
Ignore quotes and parentheses around it, as well as trailing colons."
  (if (eq (following-char) ?\")
      (save-restriction
	(narrow-to-region (point)
			  (progn (forward-sexp 1) (point)))
	(goto-char (point-min))
	(read (current-buffer)))
    (buffer-substring (point)
		      (progn
			(skip-chars-forward "^ :,\n\t(")
			(point)))))
