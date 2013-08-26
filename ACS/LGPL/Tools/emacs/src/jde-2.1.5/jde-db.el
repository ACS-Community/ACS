;;; jde-db.el -- Debugger mode for jdb.
;; $Revision: 1.26 $ $Date: 2002/02/27 10:32:24 $ 

;; Author: Paul Kinnucan <paulk@mathworks.com>
;; Maintainer: Paul Kinnucan
;; Keywords: java, tools

;; Copyright (C) 1997 Paul Kinnucan.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, US
;;; Commentary:

;; This package interfaces emacs to jdb, the debugger
;; distributed as part of JavaSoft's Java
;; Development Kit (JDK).

;; Please send bug reports and enhancement suggestions
;; to Paul Kinnucan at <paulk@mathworks.com>

;; See end of this file for change history.

;;; Code:

(require 'gud)

;; ======================================================================
;; jde-db variables

(defcustom jde-db-debugger (cons "jdb" "Executable")
"*Specify debugger.
Enter the path name of debugger, if debugger ia an executable; otherwise,
the fully qualified package name of the debugger class."
  :group 'jde-project
  :type '(cons
	  (string :tag "Name")
	  (radio-button-choice :format "%t \n%v"
			       :tag "Debugger type is "
		 (const "Executable")
		 (const "Class"))))

(defcustom jde-db-source-directories nil
  "*List of source directory paths.
The JDE uses this list to locate source files corresponding
to class files when debugging or building applications.
When entering paths in the custom buffer, enter each path as a separate
item in a separate edit field. Do NOT put more than one path in the
same edit field. You'll only confuse JDE."
  :group 'jde-project
  :type '(repeat (string :tag "Path"))
  :set '(lambda (sym val)
	  (set-default 
	   sym 
	   (mapcar
	    (lambda (path)
	      (if (not (string= (substring path (- (length path) 1)) "/"))
	       (concat path "/")
	       path))
	    val))))


(defcustom jde-db-mode-hook nil
  "*Customization hook for jde-db inferior mode."
  :group 'jde-project
  :type 'hook
)

(defcustom jde-db-set-initial-breakpoint t
  "*Set breakpoint in main and run application.
If this variable is non-nil, the JDE issues the following 
debugger commands at startup:

  stop in app-class.main
  run

where app-class is the qualified name of your application's
main class. This variable is non-nil by default. Set it to
nil, if you want to set an initial breakpoint yourself."
  :group 'jde-project
  :type 'boolean)

(defcustom jde-db-startup-commands nil
  "*Commands to run at debugger startup."
  :group 'jde-project
  :type '(repeat (string :tag "Command"))
)

(defcustom jde-db-read-vm-args nil
"*Read vm arguments from the minibuffer.
If this variable is non-nil, the jde-db command reads vm arguments
from the minibuffer and appends them to those specified by
the `jde-db-option' variable group."
  :group 'jde-project
  :type 'boolean)

(defvar jde-db-interactive-vm-args ""
"Vm arguments read from the minibuffer.")

(defvar jde-db-interactive-vm-arg-history nil
"History of vm arguments read from the minibuffer")

(defcustom jde-db-read-app-args nil
"*Read arguments to be passed to application from the minibuffer."
  :group 'jde-project
  :type 'boolean)

(defvar jde-db-interactive-app-args ""
"Application arguments read from the minibuffer.")

(defvar jde-db-interactive-app-arg-history nil
"History of application arguments read from the minibuffer")


(defgroup jde-db-options nil
  "JDE Debugger Options"
  :group 'jde
  :prefix "jde-run-option-")

(defcustom jde-db-option-classpath nil
"*Specify paths of classes required to run this application.
The JDE uses the specified paths to construct a -classpath
argument to pass to the Java interpreter. This option overrides the
`jde-global-classpath' option."
  :group 'jde-db-options
  :type '(repeat (file :tag "Path")))
 
(defcustom jde-db-option-verbose (list nil nil nil)
  "*Print messages about the running process.
The messages are printed in the run buffer."
  :group 'jde-db-options
  :type '(list :indent 2
	       (checkbox :format "\n  %[%v%] %h \n"
			 :doc "Print classes loaded.
Prints a message in the run buffer each time a class is loaded.")
	       (checkbox :format "%[%v%] %h \n"
			 :doc "Print memory freed.
Prints a message in the run buffer each time the garbage collector
frees memory.")
	       (checkbox :format "%[%v%] %h \n"
			 :doc "Print JNI info.
Prints JNI-related messages including information about which native
methods have been linked and warnings about excessive creation of
local references.")))

(defcustom jde-db-option-properties nil
  "*Specify property values.
Enter the name of the property, for example, awt.button.color, in the
Property Name field; enter its value, for example, green, in the
Property Value field. You can specify as many properties as you like."
  :group 'jde-db-options
  :type '(repeat (cons 
		  (string :tag "Property Name") 
		  (string :tag "Property Value"))))

(defcustom jde-db-option-heap-size (list
				    (cons 1 "megabytes")
				    (cons 16 "megabytes"))
"*Specify the initial and maximum size of the interpreter heap."
:group 'jde-db-options
:type '(list
	(cons (integer :tag "Start")
	     (radio-button-choice (const "bytes")
				  (const "kilobytes")
				  (const "megabytes")))
	(cons (integer :tag "Max")
	       (radio-button-choice (const "bytes")
				    (const "kilobytes")
				    (const "megabytes")))))


(defcustom jde-db-option-stack-size (list
				     (cons 128 "kilobytes")
				     (cons 400 "kilobytes"))
  "*Specify size of the C and Java stacks."
  :group 'jde-db-options
  :type '(list
	  (cons (integer :tag "C Stack")
	       (radio-button-choice (const "bytes")
				    (const "kilobytes")
				    (const "megabytes")))
	  (cons (integer :tag "Java Stack")
	       (radio-button-choice (const "bytes")
				    (const "kilobytes")
				    (const "megabytes")))))

(defcustom jde-db-option-garbage-collection (list t t)
  "*Specify garbage collection options."
  :group 'jde-db-options
  :type '(list :indent 2
	       (checkbox :format "%[%v%] %t \n"
			 :tag "Collect garbage asynchronously.")
	       (checkbox :format "%[%v%] %t \n"
			 :tag "Collect unused classes.")))

(defcustom jde-db-option-java-profile (cons nil "./java.prof")
  "*Enable Java profiling."
  :group 'jde-db-options
  :type '(cons boolean
	       (file :tag "File"
		     :help-echo 
"Specify where to put profile results here.")))

(defcustom jde-db-option-heap-profile (cons nil
					    (list "./java.hprof"
						  5
						  20
						  "Allocation objects"))
"*Output heap profiling data."
  :group 'jde-db-options
  :type '(cons boolean
	       (list
		(string :tag "Ouput File Path")
		(integer :tag "Stack Trace Depth")
		(integer :tag "Allocation Sites")
		(radio-button-choice :format "%t \n%v"
				     :tag "Sort output based on:"
		 (const "Allocation objects")
		 (const "Live objects")))))
		 
(defcustom jde-db-option-verify (list nil t)
  "*Verify classes."
  :group 'jde-db-options
  :type '(list :indent 2
	       (checkbox :format "%[%v%] %t \n"
			 :tag "Executed code in all classes.")
	       (checkbox :format "%[%v%] %t \n"
			 :tag "Classes loaded by a classloader.")))

(defcustom jde-db-option-vm-args nil
  "*Specify arguments to be passed to the Java vm.
This option allows you to specify one or more arguments to be passed
to the Java interpreter. It is an alternative to using JDE Run Option
variables, such as `jde-run-option-stack-size', to specify Java
interpreter options. Also, it makes it possible to use the JDE with
interpreters that accept command line arguments not supported by 
the JDE Run Option variable set."
  :group 'jde-db-options
  :type '(repeat (string :tag "Argument")))


(defcustom jde-db-option-application-args nil
  "*Specify command-line arguments to pass to the application.
The JDE passes the specified arguments to the application on
the command line."
  :group 'jde-db-options
  :type '(repeat (string :tag "Argument")))


;;;###autoload
(defun jde-db-set-debugger (debugger is-executable)
  "Specify the pathname of the debugger, if an executable, or the
debugger's fully qualified class name, if a class."
  (interactive
   "sEnter name of Java interpreter: \nsIs %s executable? (yes): ")
  (let ((db debugger)
	(type
	 (if (stringp is-executable)
	     (if (or
		  (string= is-executable "")
		  (eq (aref is-executable 0) ?y))
		 "Executable"
	       "Class")
	   "Executable")))
    (setq jde-db-debugger (cons db type))))

;;;###autoload
(defun jde-db-set-args (args)
  "Specify the arguments (except -classpath) to be passed to the debugger."
  (interactive 
   "sEnter arguments: ")
  (setq jde-db-option-vm-args (jde-run-parse-args args)))

;;;###autoload
(defun jde-db-set-app-args (args)
  "Specify the arguments to be passed to the Java application class."
  (interactive 
   "sEnter arguments: ")
  (setq jde-db-option-application-args (jde-run-parse-args args)))

(defun jde-db-get-vm-args ()
  "Builds a command-line argument string to pass to the Java vm.
This function builds the string from the values of the JDE
Run Option panel variables."
  (let (options
	(memory-unit-abbrevs
	 (list (cons "bytes" "")
	       (cons "kilobytes" "k")
	       (cons "megabytes" "m"))))

    ;; Set the classpath option. Use the local
    ;; classpath, if set; otherwise, the global
    ;; classpath.
    (if jde-db-option-classpath
	(setq options
	      (list "-classpath"
		    (jde-run-build-classpath-arg
		     jde-db-option-classpath)))
      (if jde-global-classpath
	  (setq options
		(list "-classpath"
		      (jde-run-build-classpath-arg
		       jde-global-classpath)))))

    ;; Set the verbose options.
    (let ((print-classes-loaded
	   (nth 0 jde-db-option-verbose))
	  (print-memory-freed
	   (nth 1 jde-db-option-verbose))
	  (print-jni-info
	   (nth 2 jde-db-option-verbose)))
      (if print-classes-loaded
	  (setq options (nconc options (list "-v"))))
      (if print-memory-freed
	  (setq options (nconc options '("-verbosegc"))))
      (if print-jni-info
	  (setq options (nconc options '("-verbosejni")))))

    ;; Set properties arguments.
    (if jde-db-option-properties
	(let ((count (length jde-db-option-properties))
	      (n 0))
	  (while (< n count)
	    (let ((prop (nth n jde-db-option-properties)))
	      (setq options 
		    (nconc options
			   (list (concat "-D" (car prop) "=" (cdr prop))))))    
	    (setq n (1+ n)))))

    ;; Set heap size options.
    (let* ((start-cons (nth 0 jde-db-option-heap-size))
	   (start-size (format "%d%s" (car start-cons) 
			       (cdr (assoc (cdr start-cons)
				      memory-unit-abbrevs))))
	   (max-cons (nth 1 jde-db-option-heap-size))
	   (max-size (format "%d%s" (car max-cons) 
			     (cdr (assoc (cdr max-cons)
				    memory-unit-abbrevs)))))
      (if (not (string= start-size "1m"))
	  (setq options 
		(nconc options (list (concat "-Xms" start-size)))))
      (if (not (string= max-size "16m"))
	  (setq options 
		(nconc options (list (concat "-Xmx" max-size))))))

    ;; Set stack size options.
    (let* ((c-cons (nth 0 jde-db-option-stack-size))
	   (c-size (format "%d%s" (car c-cons) 
			       (cdr (assoc (cdr c-cons)
				      memory-unit-abbrevs))))
	   (java-cons (nth 1 jde-db-option-stack-size))
	   (java-size (format "%d%s" (car java-cons) 
			     (cdr (assoc (cdr java-cons)
				    memory-unit-abbrevs)))))
      (if (not (string= c-size "128k"))
	  (setq options 
		(nconc options (list (concat "-Xss" c-size)))))
      (if (not (string= java-size "400k"))
	  (setq options 
		(nconc options (list (concat "-Xoss" java-size))))))

    ;; Set garbage collection options.
    (let ((no-gc-asynch (not 
			 (nth 0 jde-db-option-garbage-collection)))
	  (no-gc-classes (not 
			  (nth 1 jde-db-option-garbage-collection))))
      (if no-gc-asynch
	  (setq options (nconc options '("-Xnoasyncgc"))))
      (if no-gc-classes
	  (setq options (nconc options '("-Xnoclassgc")))))

    ;; Set Java profile option.
    (let ((profilep (car jde-db-option-java-profile))
	  (file (cdr jde-db-option-java-profile)))
      (if profilep
	  (if (string= file "./java.prof")
	      (setq options (nconc options '("-Xprof")))
	    (setq options 
		  (nconc options 
			 (list (concat "-Xprof:" file)))))))

    ;; Set heap profile option.
    (let* ((profilep (car jde-db-option-heap-profile))
	   (prof-options (cdr jde-db-option-heap-profile))
	   (file (nth 0 prof-options))
	   (depth (nth 1 prof-options))
	   (top (nth 2 prof-options))
	   (sort 
	    (downcase (substring (nth 3 prof-options) 0 1))))
      (if profilep
	  (if (and (string= file "./java.hprof")
		   (equal depth 5)
		   (equal top 20)
		   (string= sort "a"))
	      (setq options (nconc options '("-Xhprof")))
	    (setq options
		  (nconc options
			 (list
			  (format 
			   "-Xhprof:file=%s,depth=%d,top=%d,sort=%s"
			   file depth top sort)))))))

    ;; Set verify options.
    (let ((verify-all (nth 0 jde-db-option-verify))
	  (verify-remote (nth 1 jde-db-option-verify)))
      (if verify-all
	  (setq options (nconc options '("-Xverify"))))
;      (if verify-remote
;	  (setq options (concat options "-Xverifyremote")))
      (if (and
	   (not verify-all)
	   (not verify-remote))
	  (setq options (nconc options '("-Xnoverify")))))

    ;; Set command line args.
    (if jde-db-option-vm-args
	(let ((len (length jde-db-option-vm-args))
	      (n 0))
	  (while (< n len)
	    (setq options (nconc options
				 (jde-run-parse-args
				  (nth n jde-db-option-vm-args))))
	    (setq n (1+ n)))))
	      
    options))


(defvar jde-db-last-package ()
  "Package that the debugger is currently visiting.")

(defvar jde-db-xemacs-menu
  '(["Continue"          gud-cont t]
    ["Next Line"         gud-next t]
    ["Step Line"         gud-step t]
    ["Print"             gud-print t]
    ["Down Stack"        gud-down t]
    ["Up Stack"          gud-up t]
    ["Set Breakpoint"    gud-break t]
    ["Remove Breakpoint" gud-remove t]
    )
  "XEmacs 19 menu for java debugger.")

(defun jde-db-xemacs-menu ()
  (cons "Jdb" jde-db-xemacs-menu))

(defcustom jde-db-marker-regexp
  "^Breakpoint hit: .*(\\([^\$]*\\).*:\\([0-9]*\\))"
"*Regular expression used to find a jdb breakpoint position marker.
The regular expression must have two subexpressions. The first matches
the name of the class in which the breakpoint occurs; the second, the
line number at which the breakpoint occurs. The default expression
matches breakpoint messages emitted by jdb. You may need to change
the expression to accommodate other debuggers."
  :group 'jde-project
  :type 'string
)

(defcustom jde-db-nodebug-marker-regexp
  "^Breakpoint hit: .*(pc \\([0-9]*\\))"
"*Regular expression to match breakpoint message for which no
line number information is available.")

;; I'm not sure the following is necessary anymore. PK.

;; Thanks to "David J. Biesack" <sasdjb@unx.sas.com> for this function
;; and its use in jde-db-marker-filter.
(defun jde-db-make-qualified-class-name-regexp (class)
"Constructs a regular expression to extract a qualified class name from a jdb
breakpoint message."
  (concat "\\b\\(\\(\\(\\(\\w\\|[_]\\)*\\.\\)*\\)" class "\\)\\(\\b\\|\\$\\)"))

;; There's no guarantee that Emacs will hand the filter the entire
;; marker at once; it could be broken up across several strings.  We
;; might even receive a big chunk with several markers in it.  If we
;; receive a chunk of text which looks like it might contain the
;; beginning of a marker, we save it here between calls to the
;; filter.
(defvar jde-db-marker-acc "")
(make-variable-buffer-local 'jde-db-marker-acc)

(defun jde-db-marker-filter (input)

  ;; Accumulate next chunk of debugger output.
  (setq jde-db-marker-acc (concat jde-db-marker-acc input))

  ;; This is a hack to accommodate reorder of message chunks
  ;; on Solaris at debugger startup.
  (if (string-match "running ...\n" jde-db-marker-acc)
      (setq jde-db-marker-acc
	    (concat "running ...\n"
		    (substring jde-db-marker-acc 0 (match-beginning 0))
		    (substring jde-db-marker-acc (match-end 0)))))
		    
  (let ((output ""))

    ;; (message (concat "jdb output:" input))
    ;; (message (concat "acc = " jde-db-marker-acc))
    
    ;; Process all the complete markers in this chunk.
    (if (string-match jde-db-marker-regexp jde-db-marker-acc)
	;; Extract the frame position from the marker.
	(let ((premarker (substring jde-db-marker-acc 0 (match-beginning 0)))
	      (marker (substring jde-db-marker-acc (match-beginning 0) (match-end 0)))
	      (class (substring jde-db-marker-acc  (match-beginning 1) (match-end 1)))
	      (line-no (string-to-int (substring jde-db-marker-acc
						 (match-beginning 2)
						 (match-end 2))))
	      (rest (substring jde-db-marker-acc (match-end 0))))

	  (setq gud-last-frame (cons (concat class ".java") line-no))

	  ;; Extract package path from input.
	  (setq jde-db-last-package "")
	  (let ((case-fold-search nil)) ;; Make sure search is case-sensitive
	    (and (string-match (jde-db-make-qualified-class-name-regexp class) marker)
		 (setq jde-db-last-package
		       (substring marker (match-beginning 2) (match-end 2))))

	    ;; (message "jde-db package: %s. marker = %s" jde-db-last-package marker)
	    ;;(message "case-fold-search = %s" (if case-fold-search "true" "false"))
	    )

	  (setq output (concat premarker "Stopped at line " (int-to-string line-no) " in "
			       class ".java"))

	  ;; Set the accumulator to the remaining text.
	  (setq jde-db-marker-acc rest)
	  ))

    ;; Handle case where there is no line number info in current class.
    (if (string-match jde-db-nodebug-marker-regexp jde-db-marker-acc) 
	(let ((premarker (substring jde-db-marker-acc 0 (match-beginning 0)))
	      (marker (substring jde-db-marker-acc (match-beginning 0) (match-end 0)))
	      (pc (substring jde-db-marker-acc (match-beginning 1) (match-end 1)))
	      (rest (substring jde-db-marker-acc (match-end 0))))

	  (setq output (concat premarker marker))
	  ))

    ;; Does the remaining text look like it might end with the
    ;; beginning of another marker?  If it does, then keep it in
    ;; jde-db-marker-acc until we receive the rest of it.  Since we
    ;; know the full marker regexp above failed, it's pretty simple to
    ;; test for marker starts.
    (if (string-match "^Breakpoint hit:" jde-db-marker-acc)
	(progn
	  ;; Everything before the potential marker start can be output.
	  (setq output (concat output (substring jde-db-marker-acc
						 0 (match-beginning 0))))

	  ;; Everything after, we save, to combine with later input.
	  (setq jde-db-marker-acc
		(substring jde-db-marker-acc (match-beginning 0))))

      (setq output (concat output jde-db-marker-acc)
	    jde-db-marker-acc ""))    

    output))

(defun jde-db-contains-file-p (dir file)
  "Return t if DIR contains FILE."
  (let ((files (condition-case nil (directory-files dir) (error nil))))
    (catch 'found
      (let ((n (- (length files) 1)))
	(while (>= n 0)
	  (if (string= file (elt files n))
	      (throw 'found t))
	  (setq n (- n 1)))))))

(defun jde-db-contains-package-p (dir package)
  "Return t if DIR contains PACKAGE."
  (let ((files (condition-case nil (directory-files dir) (error nil)))
	(pkg-root (substring package
			     0
			     (string-match "\\." package))))
    (catch 'found
      (let ((n (- (length files) 1)))
	(while (>= n 0)
	  (let ((curr-file (elt files n)))
	    (if (string= pkg-root curr-file)
		(throw 'found t)))
	  (setq n (- n 1)))))))

(defun jde-db-pkg-to-path (package)
  "Return PACKAGE as a directory path."
  (let ((n (string-match "\\." package))
	(output (concat package)))
    (while n
      (aset output n ?/)
      (setq n (string-match "\\." output (+ n 1))))
    output))
      

(defun jde-db-search-src-dirs (file package)
  "Return the directory containing the source FILE for a class in PACKAGE."
  (catch 'found
    (let ((len (length jde-db-source-directories))
	  (n 0))
      (while (< n len)
	(let ((curr-dir (elt jde-db-source-directories n)))
	  (cond
	   ((jde-db-contains-file-p curr-dir file)
	    ;(message "jde-db-search-src-dirs found %s in %s" file curr-dir)
	    (throw 'found curr-dir))
	   ((and (jde-db-contains-package-p curr-dir package)
		 (jde-db-contains-file-p
		  (concat curr-dir (jde-db-pkg-to-path package)) file))
	    ;; (message "jde-db-search-src-dirs found %s in %s" file (concat curr-dir (jde-db-pkg-to-path jde-db-last-package)))
	    (throw 'found
		   (concat curr-dir 
			   (jde-db-pkg-to-path package))))
	   ;; (t (message "jde-db-search-src-dirs: %s not in %s" file curr-dir))
	    ))
	(setq n (1+ n))))))

;; Fixes a bug in gud-make-debug-menu
(defun jde-db-make-debug-menu ()
  "Make sure the current local map has a [menu-bar debug] submap.
If it doesn't, replace it with a new map that inherits it,
and create such a submap in that new map."
  (if (and (current-local-map)
	   (lookup-key (current-local-map) [menu-bar debug]))
      nil
    (use-local-map (gud-new-keymap (current-local-map)))
    (define-key (current-local-map) [menu-bar debug]
      (nconc (list "Jdb") gud-menu-map))))



(defun jde-db-find-file (f)
  ;;(interactive "sFile: " )
  (let ((source-dir 
	 (jde-db-search-src-dirs f jde-db-last-package)))
    (if source-dir
	(let ((source-file
	       (concat source-dir f)))	
	  (save-excursion
	    (let ((buf (find-file-noselect source-file)))
	      (set-buffer buf)
	      (if (string-match "XEmacs\\|Lucid" emacs-version)
		  (if (and (boundp 'current-menubar)
			   current-menubar)
		      (if (fboundp 'add-submenu)
			  (add-submenu nil (jde-db-xemacs-menu))
			(add-menu nil "Jdb" jde-db-xemacs-menu)))
		(progn
		  (jde-db-make-debug-menu)
		  (local-set-key [menu-bar debug refresh] nil)
		  (local-set-key [menu-bar debug stepi] nil)
		  (local-set-key [menu-bar debug up] '("Up Stack" . gud-up))
		  (local-set-key [menu-bar debug down] '("Down Stack" . gud-down))))
	      buf)))
	  (message (concat "Error: could not find %s. "
			   "See jde-db-source-directories.") f)
	  (if jde-xemacsp (window-buffer) nil))))

(defun jde-find-class-source (class)
  "*Find the source file for a specified class.
CLASS is the fully qualified name of the class. This
function searchs the source file paths specified by 
`jde-db-source-directories' for the source file 
corresponding to CLASS. If it finds the source file,
it opens the file in a buffer."
  (interactive "sClass: ")
  (string-match "^\\(\\(\\(\\w\\|[_]\\)*[.]\\)*\\)\\(\\(\\w\\|[_]\\)+$\\)" class)  
  (let* ((package-name
	  (substring class (match-beginning 1) (match-end 1)))
	 (class-name
	  (substring class (match-beginning 4) (match-end 4)))
	 (file-name (concat class-name ".java"))
	 (source-dir
	  (jde-db-search-src-dirs
	   file-name
	   package-name)))
    (if source-dir
	(find-file (concat source-dir file-name))
      (message (concat "JDE error: Could not find source for %s. "
		       "See `jde-db-source-directories' for more information." )
		       class))))

(defvar jde-db-minibuffer-local-map nil
  "Keymap for minibuffer prompting of jdb startup command.")
(if jde-db-minibuffer-local-map
    ()
  (setq jde-db-minibuffer-local-map (copy-keymap minibuffer-local-map))
  (define-key
    jde-db-minibuffer-local-map "\C-i" 'comint-dynamic-complete-filename))


(defun class-from-file-name (file-name)
  (file-name-sans-extension (file-name-nondirectory file-name)))

;;; The jde-db-call function must do the right thing whether its invoking
;;; keystroke is from the GUD buffer itself (via major-mode binding)
;;; or a Java buffer.  In the former case, we want to supply data from
;;; gud-last-frame.  Here's how we do it:
;;; Note: this is adapted from the gud-format-command function
;;; in gud.el.

(defun jde-db-format-command (str arg)
  (let ((insource (not (eq (current-buffer) gud-comint-buffer)))
	(frame (or gud-last-frame gud-last-last-frame))
	result)
    (while (and str (string-match "\\([^%]*\\)%\\([acdeflp]\\)" str))
      (let ((key (string-to-char (substring str (match-beginning 2))))
	    (group1 (substring str (match-beginning 1) (match-end 1)))
	    subst)
	(setq str (substring str (match-end 2)))
	(cond
	 ((eq key ?f)
	  (setq subst (file-name-nondirectory (if insource
						  (buffer-file-name)
						(car frame)))))
	 ((eq key ?c)
	  (setq subst (concat (jde-db-get-package)
			      (class-from-file-name (if insource
							(buffer-file-name)
						      (car frame))))))

	 ((eq key ?d)
	  (setq subst (file-name-directory (if insource
					       (buffer-file-name)
					     (car frame)))))
	 ((eq key ?l)
	  (setq subst (if insource
			  (save-excursion
			    (beginning-of-line)
			    (save-restriction (widen)
					      (1+ (count-lines 1 (point)))))
			(cdr frame))))
	 ((eq key ?e)
	  (setq subst (if (fboundp 'gud-find-c-expr)
			  (gud-find-c-expr)
			(find-c-expr))))
	 ((eq key ?a)
	  (setq subst (gud-read-address)))
	 ((eq key ?p)
	  (setq subst (if arg (int-to-string arg) ""))))
	(setq result (concat result group1 
			     (if (integerp subst) (int-to-string subst) subst)))))
    ;; There might be text left in STR when the loop ends.
    (concat result str)))

(defun jde-db-call (fmt &optional arg)
  (let ((msg (jde-db-format-command fmt arg)))
    (message "Command: %s" msg)
    (sit-for 0)
    (gud-basic-call msg)))

(defmacro jde-db-def (func cmd key &optional doc)
  "Define FUNC to be a command sending CMD and bound to KEY, with
optional doc string DOC.  Certain %-escapes in the string arguments
are interpreted specially if present.  These are:

  %f name (without directory) of current source file.
  %c fully qualified class name
  %d directory of current source file.
  %l number of current source line
  %e text of the C lvalue or function-call expression surrounding point.
  %a text of the hexadecimal address surrounding point
  %p prefix argument to the command (if any) as a number

  The `current' source file is the file of the current buffer (if
we're in a Java file) or the source file current at the last break or
step (if we're in the jdb buffer).
  The `current' line is that of the current buffer (if we're in a
source file) or the source line number at the last break or step (if
we're in the jdb buffer)."
  (list 'progn
	(list 'defun func '(arg)
	      (or doc "")
	      '(interactive "p")
	      (list 'jde-db-call cmd 'arg))
	(if key
	    (list 'define-key
		  '(current-local-map)
		  (concat "\C-c" key)
		  (list 'quote func)))
	(if key
	    (list 'global-set-key
		  (list 'concat 'gud-key-prefix key)
		  (list 'quote func)))))

(defun jde-db-get-vm-args-from-user ()
  (if jde-db-read-vm-args
      (jde-run-parse-args
       (read-from-minibuffer
	"Vm args: "
	jde-db-interactive-vm-args
	nil nil
	'(jde-db-interactive-vm-arg-history . 1)))))

(defun jde-db-get-app-args-from-user ()
  (if jde-db-read-app-args
      (progn
 	(setq jde-db-interactive-app-args
 	      (read-from-minibuffer
 	       "Application args: "
	       jde-db-interactive-app-args
	       nil nil
 	       '(jde-db-interactive-app-arg-history . 1)))
 	(jde-run-parse-args jde-db-interactive-app-args))))


(defun jde-db-init(app-class marker-filter find-file)
  (let ((debug-buf-name (concat "*debug" app-class "*"))
	(source-directory default-directory)
	(working-directory (if (string= jde-run-working-directory "")
			       default-directory
			     jde-run-working-directory)))
    (if (not (comint-check-proc debug-buf-name))
	(let* ((debug-buffer (get-buffer-create debug-buf-name))
	       (program (if (string= (cdr jde-db-debugger) "Executable")
			   (car jde-db-debugger)
			 jde-run-java-vm))
	       (prog-args (if (string= (cdr jde-db-debugger) "Executable")
			      (append 
			       (jde-db-get-vm-args)
			       (jde-db-get-vm-args-from-user)
			       (list app-class)
			       jde-db-option-application-args
			       (jde-db-get-app-args-from-user))
			    (append
			     (list (car jde-db-debugger))
			     (jde-db-get-vm-args)
			     (jde-db-get-vm-args-from-user)
			     (list app-class)
			     jde-db-option-application-args
			     (jde-db-get-app-args-from-user))))
	       (command-string (concat
				program " "
				(jde-run-make-arg-string prog-args) "\n\n")))
	  (save-excursion
	    (set-buffer debug-buffer)
	    (erase-buffer)
	    (cd working-directory)
	    (insert (concat "cd " working-directory "\n"))
	    (insert command-string)
	    (comint-mode))
	  (comint-exec debug-buffer app-class program nil prog-args)
	  (pop-to-buffer debug-buffer)
	  (cd source-directory)
	  (gud-mode)
	  (make-local-variable 'gud-marker-filter)
	  (setq gud-marker-filter marker-filter)
	  (make-local-variable 'gud-find-file)
	  (setq gud-find-file find-file)
	  (set-process-filter (get-buffer-process (current-buffer)) 'gud-filter)
	  (set-process-sentinel (get-buffer-process (current-buffer)) 'gud-sentinel)
	  (gud-set-buffer)
	  (if jde-db-startup-commands
	    (mapc 'gud-basic-call jde-db-startup-commands)
	   (when jde-db-set-initial-breakpoint
	    (gud-basic-call (concat "stop in " app-class ".main"))
	    (gud-basic-call "run")))
	  )
      (message "An instance of %s is running." app-class)			
      (pop-to-buffer debug-buf-name))))			   
	  

(defun jde-db-get-package ()
  "Return the package of the class whose source file resides in the current
buffer."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^[ \t]*\\<\\(package\\) *\\([^ ]*\\) *;" (point-max) t)
	(concat (buffer-substring-no-properties (match-beginning 2) (match-end 2))
		"."))))

;;;###autoload		   
(defun jde-db ()
  "Run jdb on Java class whose source resides in the current buffer.
This command creates a command buffer named *gud-CLASS* for the debug session.
CLASS is the name of the class you are debugging."
  (interactive)

  ;; test for XEmacs
  (if (string-match "XEmacs\\|Lucid" emacs-version)
      (gud-overload-functions
       '((gud-marker-filter . jde-db-marker-filter)
	 (gud-find-file . jde-db-find-file)
	 )))

  (let ((app-class jde-run-application-class))
    (if (or
	 (not app-class)
	 (string= app-class ""))
	(setq app-class
	      (concat (jde-db-get-package)
		      (file-name-sans-extension 
		       (file-name-nondirectory (buffer-file-name))))))
    (jde-db-init app-class 'jde-db-marker-filter 'jde-db-find-file))

  (jde-db-def gud-break  "stop at %c:%l"  "\C-b" "Set breakpoint at current line.")
  (jde-db-def gud-remove "clear %c:%l"    "\C-d" "Remove breakpoint at current line")
  (jde-db-def gud-step   "step"           "\C-s" "Step one source line with display.")
  (jde-db-def gud-next   "next"           "\C-n" "Step one line (skip functions).")
  (jde-db-def gud-cont   "cont"           "\C-r" "Continue with display.")
  (jde-db-def gud-up     "up %p"          "<" "Up N stack frames (numeric arg).")
  (jde-db-def gud-down   "down %p"        ">" "Down N stack frames (numeric arg).")
  (jde-db-def gud-print  "print %e"       "\C-p" "Print object.")
  (local-set-key [menu-bar debug up]   '("Up Stack" . gud-up))
  (local-set-key [menu-bar debug down] '("Down Stack" . gud-down))
  (make-local-variable 'comint-prompt-regexp)
  (setq comint-prompt-regexp "\\(^> *\\)\\|\\(^.*\\[[0-9]*\\] *\\)")
  (make-local-variable 'paragraph-start)
  (setq paragraph-start comint-prompt-regexp)
  (run-hooks 'jde-db-mode-hook)
  )

(defun check-source-path (path) 
  "Return a valid path or nil if path is not found."  
  ;; Ensure that path ends in a slash.
  (let ((p (if (not (string= (substring path (- (length path) 1)) "/"))
	       (concat path "/")
	     path)))
    (if (file-directory-p p)
	p
      nil)))

;;;###autoload
(defun jde-db-set-source-paths (paths)
 "Set the source path list used by JDE to locate Java source files visited
by the debugger. PATHS is a list of source paths separated by colons or semicolons,
depending on the operating system.. Note that for packages, you need enter only the 
directory containing the top-level package. For example, including '../jdk1.1/src/'
 enables the JDE to  locate all source files provided with JDK1.1. Note also that the 
paths must end in a slash."
 (interactive
  "sEnter source paths: ")
 (let ((m 0)
       (n (string-match jde-classpath-separator paths)))
   (setq jde-db-source-directories (list))
   (while n
     (let ((path (check-source-path (substring paths m n))))
       (if path
	   (setq jde-db-source-directories
		 (cons path jde-db-source-directories)))
       (setq m (+ n 1))
       (setq n (string-match jde-classpath-separator paths m))))
   (setq n (length paths))
   (if (and (> n 0) (< m n))
       (let ((path (check-source-path (substring paths m n))))
	 (if path
	     (setq jde-db-source-directories
		   (cons path jde-db-source-directories)))))
   (setq jde-db-source-directories (nreverse jde-db-source-directories))))


(defun jde-db-applet-init(applet-class marker-filter find-file applet-doc)
  (let ((debug-buf-name (concat "*debug" applet-class "*"))
	(source-directory default-directory)
	(working-directory (if (string= jde-run-working-directory "")
			       default-directory
			     jde-run-working-directory)))
    (if (not (comint-check-proc debug-buf-name))
	(let* ((debug-buffer (get-buffer-create debug-buf-name))
	       (program "appletviewer")
	       (prog-args (append
			   (jde-get-appletviewer-options)
			   (list "-debug" applet-doc)))
	       (command-string (concat
				program " "
				(jde-run-make-arg-string prog-args) "\n\n")))
	  (save-excursion
	    (set-buffer debug-buffer)
	    (erase-buffer)
	    (cd working-directory)
	    (insert (concat "cd " working-directory "\n"))
	    (insert command-string)
	    (comint-mode))
	  (comint-exec debug-buffer applet-class program nil prog-args)
	  (pop-to-buffer debug-buffer)
	  (cd source-directory)
	  (gud-mode)
	  (make-local-variable 'gud-marker-filter)
	  (setq gud-marker-filter marker-filter)
	  (make-local-variable 'gud-find-file)
	  (setq gud-find-file find-file)
	  (set-process-filter (get-buffer-process (current-buffer)) 'gud-filter)
	  (set-process-sentinel (get-buffer-process (current-buffer)) 'gud-sentinel)
	  (gud-set-buffer)
	  (if jde-db-startup-commands
	    (mapc 'gud-basic-call jde-db-startup-commands)
	   (when jde-db-set-initial-breakpoint
	    (gud-basic-call (concat "stop in " applet-class ".init"))
	    (gud-basic-call "run")))
	  )
      (message "An instance of %s is running." applet-class)			
      (pop-to-buffer debug-buf-name))))			   
	  
   
(defun jde-db-applet-internal (applet-doc)
  ;; test for XEmacs
  (if (string-match "XEmacs\\|Lucid" emacs-version)
      (gud-overload-functions
       '((gud-marker-filter . jde-db-marker-filter)
	 (gud-find-file . jde-db-find-file)
	 )))

  (let ((applet-class jde-run-application-class))
    (if (or
	 (not applet-class)
	 (string= applet-class ""))
	(setq applet-class
	      (concat (jde-db-get-package)
		      (file-name-sans-extension 
		       (file-name-nondirectory (buffer-file-name))))))
    (jde-db-applet-init applet-class 'jde-db-marker-filter 'jde-db-find-file applet-doc))

  (jde-db-def gud-break  "stop at %c:%l"  "\C-b" "Set breakpoint at current line.")
  (jde-db-def gud-remove "clear %c:%l"    "\C-d" "Remove breakpoint at current line")
  (jde-db-def gud-step   "step"           "\C-s" "Step one source line with display.")
  (jde-db-def gud-next   "next"           "\C-n" "Step one line (skip functions).")
  (jde-db-def gud-cont   "cont"           "\C-r" "Continue with display.")
  (jde-db-def gud-up     "up %p"          "<" "Up N stack frames (numeric arg).")
  (jde-db-def gud-down   "down %p"        ">" "Down N stack frames (numeric arg).")
  (jde-db-def gud-print  "print %e"       "\C-p" "Print object.")
  (local-set-key [menu-bar debug up]   '("Up Stack" . gud-up))
  (local-set-key [menu-bar debug down] '("Down Stack" . gud-down))
  (make-local-variable 'comint-prompt-regexp)
  (setq comint-prompt-regexp "\\(^> *\\)\\|\\(^.*\\[[0-9]*\\] *\\)")
  (make-local-variable 'paragraph-start)
  (setq paragraph-start comint-prompt-regexp)
  (run-hooks 'jde-db-mode-hook)
  )

;;;###autoload 
(defun jde-db-applet (&optional doc) 
  "Runs an applet in the debugger. This function prompts you to enter
the path of an html document that displays the applet. If you 
do not enter a path, this function next checks
whether `jde-run-applet-doc' specifies a document. If so, it displays
that specified document. Next, it checks whether the current directory
contains any html files. If so, it displays the first html file that
it finds. If if cannot find an html file, it signals an error.  This
function runs appletviewer in jdb to permit debugging. On startup, it
sets a breakpoint in the init method of the class specified by 
`jde-run-application-class' or in the class corresponding to the Java
file in the current buffer."
  (interactive
   (let ((insert-default-directory nil))
     (list (read-file-name "Applet doc: " nil nil nil jde-run-applet-document))))
  (setq jde-run-applet-document doc)
  (let ((applet-doc (if (and jde-run-applet-document
			     (not (string= jde-run-applet-document "")))
			jde-run-applet-document
		      (if (and jde-run-applet-doc
			       (not (string= jde-run-applet-doc "")))
			    jde-run-applet-doc
			  (car (jde-run-find-html-files))))))
    (if applet-doc 
	(jde-db-applet-internal applet-doc) 
      (signal 'error "Could not find html document to display applet."))))


(defun jde-db-menu-debug-applet ()
  (interactive)
  (jde-db-applet))

(provide 'jde-db)


;; Change History
;; $Log: jde-db.el,v $
;; Revision 1.26  2002/02/27 10:32:24  vltsccm
;; gchiozzi: Forced making for emacs. Problems on RH7.2 with xemacs automatically found.
;;
;; Revision 1.25  2002/02/21 14:59:57  vltsccm
;; emacs1.25
;;
;; Revision 1.24  2002/02/20 07:40:21  vltsccm
;; emacs1.24
;;
;; Revision 1.23  2002/02/09 17:14:07  vltsccm
;; emacs1.23
;;
;; Revision 1.22  2000/07/03 14:04:53  vltsccm
;; emacs1.22
;;
;; Revision 1.21  1999/11/21 21:05:02  vltsccm
;; emacs1.21
;;
;; Revision 1.20.1.1  1999/11/09 02:50:57  vltsccm
;; emacs1.20.1
;;
;; Revision 1.20  1999/06/09 14:55:43  vltsccm
;; emacs1.20
;;
;; Revision 1.19  1999/06/09 14:55:43  vltsccm
;; emacs1.19
;;
;; Revision 1.18  1999/06/09 14:55:42  vltsccm
;; emacs1.18
;;
;; Revision 1.17  1999/06/09 14:55:42  vltsccm
;; emacs1.17
;;
;; Revision 1.16  1999/06/09 14:55:42  vltsccm
;; emacs1.16
;;
;; Revision 1.15  1999/06/09 14:55:41  vltsccm
;; emacs1.15
;;
;; Revision 1.14  1999/06/09 14:55:41  vltsccm
;; emacs1.14
;;
;; Revision 1.13  1999/06/09 14:55:41  vltsccm
;; emacs1.13
;;
;; Revision 1.12  1999/06/09 14:55:40  vltsccm
;; emacs1.12
;;
;; Revision 1.11  1999/06/09 14:55:40  vltsccm
;; emacs1.11
;;
;; Revision 1.10  1999/06/09 14:55:40  vltsccm
;; emacs1.10
;;
;; Revision 1.9  1999/06/09 14:55:39  vltsccm
;; emacs1.9
;;
;; Revision 1.8  1999/06/09 14:55:39  vltsccm
;; emacs1.8
;;
;; Revision 1.7  1999/06/09 14:55:39  vltsccm
;; emacs1.7
;;
;; Revision 1.6  1999/06/09 14:55:38  vltsccm
;; emacs1.6
;;
;; Revision 1.5  1999/06/09 14:55:38  vltsccm
;; emacs1.5
;;
;; Revision 1.4  1999/06/09 14:55:38  vltsccm
;; emacs1.4
;;
;; Revision 1.3  1999/06/09 14:55:37  vltsccm
;; emacs1.3
;;
;; Revision 1.2  1999/06/09 14:55:37  vltsccm
;; emacs1.2
;;
;; Revision 1.52  1999/03/10 16:55:02  paulk
;; Fixed jde-db-find-file to return the current buffer if it cannot find a file and
;; XEmacs is the editor.
;;
;; Revision 1.51  1999/03/06 00:55:38  paulk
;; Changed default value of jde-db-source-directories to be nil.
;;
;; Revision 1.50  1999/03/06 00:44:08  paulk
;; Make sure that case-sensitive matching is used when extracting package names from
;; debugger breakpoint messages.
;;
;; Revision 1.49  1999/02/26 15:52:52  paulk
;; Catch non-existant directory errors when searching for source
;; files and packages. Thanks to Thanh Nguyen <Thanh.Nguyen@Eng.Sun.COM>
;; for finding and providing a fix for this bug.
;;
;; Revision 1.48  1999/02/25 15:24:43  paulk
;; Fixed jde-db-find-file so that it displays an error when it cannot find a file instead of
;; opening an empty source buffer.
;;
;; Provided a set-value function for jde-db-source-directories that appends a slash to
;; the end of each path if the path does not already end in a slash.
;;
;; Defined a new command, jde-find-class-source, that finds and opens the source file
;; for a specified class.
;;
;; Improved the regular expression used by jde-db-get-package to ignore tabs at the
;; beginning of a line.
;;
;; Revision 1.47  1999/02/15 02:02:35  paulk
;; Forgot to concatenate in last fix.
;;
;; Revision 1.46  1999/02/15 00:52:44  paulk
;; Fixed bug in qualified-class-name-regexp.
;;
;; Revision 1.45  1999/02/10 18:35:51  paulk
;; Added support for appletviewer -encoding and -J options.
;;
;; Revision 1.44  1999/02/08 17:18:17  paulk
;; jde-db-applet now supports file completion and remembers the last path entered.
;;
;; Revision 1.43  1999/02/06 03:55:11  paulk
;; Fixed bug and generalized regular expression in jde-db-make-qualified-class-name-regexp.
;;
;; Revision 1.42  1999/02/03 18:12:03  paulk
;; Fixed regular expression in jde-db-get-package to eliminate spurious hits, e.g.
;; commented out package statements. Thanks to Frederic Baumann <baumann@ilog.fr>
;; for reporting this bug.
;;
;; Revision 1.41  1999/02/03 17:48:34  paulk
;; Patched jde-db-get-app-args-from-user to remember arguments.
;; Thanks to Brian Burton <brian@burton-computer.com>
;;
;; Revision 1.40  1999/02/03 17:41:56  paulk
;; Fixed jde-db-make-qualified-class-name-regexp to handle packages with underscores.
;; Thanks to Brian Burton <brian@burton-computer.com>.
;;
;; Revision 1.39  1999/02/03 17:26:46  paulk
;; Changed jde-db-make-qualified-class-name-regexp to handle inner classes.
;; Thanks to Michael Lepore <lepore@process.com> for this fix.
;;
;; Revision 1.38  1999/02/03 01:53:49  paulk
;; Fixed jde-db-applet to check the current directory for the html file to run.
;;
;; Revision 1.37  1999/02/02 16:06:01  paulk
;; Added the jde-db-applet command. This command allows you to debug an applet, using
;; appletviewer.
;;
;; Revision 1.36  1999/02/02 15:25:28  paulk
;; Removed unwanted space in -D (properties) debug option.
;;
;; Revision 1.35  1999/01/17 00:36:43  paulk
;; Now uses gud-find-c-expr or find-c-expr, whichever is bound.
;;
;; Revision 1.34  1999/01/13 22:18:08  paulk
;; Added Andy Piper's NT/XEmacs 21 compatibility changes.
;; Changed find-c-expr to gud-findc-expr
;;
;; Revision 1.33  1998/11/22 18:18:36  paulk
;; Made comint-prompt-regexp and  paragraph-start local variables.
;;
;; Revision 1.32  1998/11/04 02:59:09  paulk
;; Corrected verbiage in Jde Debugger Options description.
;;
;; Revision 1.31  1998/09/12 00:05:57  paulk
;; Debugger now runs application from directory specified by jde-run-working-directory.
;;
;; Revision 1.30  1998/06/30 04:03:19  paulk
;; Added variables `jde-db-read-vm-args' and `jde-db-read-app-args'. The use of
;; these variables is the same as the corresponding jde-run variables.
;;
;; Revision 1.29  1998/06/29 02:50:44  paulk
;; Fixed bug in marker filter.
;;
;; Revision 1.28  1998/06/27 03:34:31  paulk
;; Provided a hack to handle reordering of threaded messages on Solaris.
;;
;; Provided code to handle case where current class has no line number
;; information.
;;
;; Revision 1.27  1998/06/25 04:27:23  paulk
;; Removed debug messages from jde-db-marker-filter.
;;
;; Revision 1.26  1998/06/25 04:21:10  paulk
;; Modified jde-db-marker-filter to accummulate debugger output
;; in chunks. Fixes bug reported by Eric Prud'hommeaux (eric@w3.org).
;;
;; Revision 1.25  1998/06/22 03:52:28  paulk
;; Added jde-db-startup-commands variable. This variable allows you to
;; specify debugger commands to run when the debugger is started.
;;
;; Revision 1.24  1998/06/21 00:09:43  paulk
;; Added a customizable feature, jde-db-set-initial-breakpoint, that causes
;; the JDE to set an initial breakpoint in an app's main routine and run
;; to the breakpoint on debugger startup. The feature is enabled by default.
;;
;; Revision 1.23  1998/06/20 23:42:07  paulk
;; Made jde-db-marker-regexp a custom variable to facilitate the use of the JDE
;; with debuggers other than jdb.
;;
;; Changed the marker regular expression to detect only jdb breakpoint messages,
;; i.e., messages of the form
;;
;;   Breakpoint hit: qualified.class.name (class:line)
;;
;; This should eliminate the problem of spurious hits when exceptions occur and
;; stack traces are printed.
;;
;; Revision 1.22  1998/05/27 06:09:46  paulk
;; Added autoload comments.
;;
;; Revision 1.21  1998/03/27 04:16:12  kinnucan
;; Fixed typo in the code that displays the jdb menu on XEmacs.
;;
;; Revision 1.20  1998/03/27 04:14:53  kinnucan
;; Modified jde-db-search-src-dirs to take current package as an
;; argument rather than use a global variable. This allows
;; it to be used by jde-java-build function.
;;
;; Revision 1.19  1998/03/18 03:54:06  kinnucan
;; Changed jde-db-marker-regexp to account for inner classes.
;; Thanks to Andreas Rasmusson <Andreas.Rasmusson@sics.se> for
;; providing this fix.
;;
;; Revision 1.18  1998/03/04 04:28:36  kinnucan
;; Added test for jde-run-application-class = "" to jde-db
;;
;; Revision 1.17  1998/02/27 22:16:34  kinnucan
;; Changed copyright to Paul Kinnucan.
;; Have not yet assigned rights to FSF.
;;
;; Revision 1.16  1998/02/27 22:15:24  kinnucan
;; Added support for Emacs customization feature.
;;
;; Revision 1.15  1998/02/17 04:16:38  kinnucan
;; Fixed bug in jde-deb-set-source-paths that caused the last
;; directory to not be normalized (i.e., slash appended).
;;
;; Revision 1.14  1998/02/12 05:15:38  kinnucan
;; Changed the jde-db-search-src-dirs to search the source directory list from
;; front to back instead of back to front. The former search order did not allow newer versions of the same class to shadow older versions. Thanks to "David J. Biesack" <sasdjb@unx.sas.com> for supplying this fix.
;;
;; Revision 1.13  1998/02/12 04:57:13  kinnucan
;; Fixed bug in jde-db-marker-filter that sometimes prevented the JDE from
;; loading the correct source file. Thanks to David J. Biesack
;; <sasdjb@unx.sas.com> for supplying the fix.
;;
;; Revision 1.12  1997/10/30 05:42:37  kinnucan
;; Made configuration variables settable.
;;
;; Revision 1.11  1997/10/26 05:49:59  kinnucan
;; Applied Derek Young's patch to cause jde to qualify class names
;; when setting a breakpoint.
;;
;; Revision 1.10  1997/10/20 05:27:48  kinnucan
;; Removed reference to deleted function jde-db-massage-args
;;
;; Revision 1.9  1997/10/11 01:36:05  kinnucan
;; Fixed bug in jde-db-search-src-dirs discovered by Jonathan Payne.
;;
;; Revision 1.8  1997/10/06 14:40:53  kinnucan
;; Fixed bugs in jde-db-set-debugger command.
;;
;; Revision 1.7  1997/10/05 21:20:15  kinnucan
;; 1. Added the variables jde-db-debugger and jde-db-debugger-is-executable
;;    and the associated setter function jde-db-set-debugger. These allow
;;    you to specify a custom debugger for the JDE>
;;
;; 2. Added jde-db-args and jde-db-app-args and the associated setter
;;    functions. These allow you to specify debugger and application
;;    command line arguments.
;;
;; Revision 1.6  1997/10/05 04:53:04  kinnucan
;; Fixed bug in print object menu item.
;;
;; Revision 1.5  1997/08/26 14:53:39  paulk
;; Fixed bug in check-source-path.
;;
;; Revision 1.4  1997/08/26 08:52:14  kinnucan
;; Tweaked JDE Version number for JDE 1.8 release.
;;
;; Revision 1.3  1997/07/05 04:18:10  kinnucan
;; Updated make-jdb-command to run either the class previously specifed with
;; the jde-run-set-app command or the class corresponding to the code in the
;; current buffer.
;;
;; Revision 1.2  1997/06/18 18:45:11  paulk
;; Added error-checking to jde-db-set-source-paths function. Now checks for
;; existence of specified directories and appends a terminal slash to paths
;; that lack it.
;;
;; Revision 1.1  1997/06/18 17:21:59  paulk
;; Initial revision
;;

;;; end of jde-db.el