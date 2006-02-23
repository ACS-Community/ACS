;;; jde.el -- Integrated Development Environment for Java.
;; $Revision: 1.26 $ $Date: 2002/02/27 10:32:23 $ 

;; Author: Paul Kinnucan <paulk@mathworks.com>
;; Maintainer: Paul Kinnucan
;; Keywords: java, tools

;; Copyright (C) 1997, 1998, 1999 Paul Kinnucan.

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
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This is one of a set of packages that make up the 
;; Java Development Environment (JDE) for Emacs. See the
;; JDE User's Guide for more information.

;; The latest version of the JDE is available at
;; <URL:http://sunsite.auc.dk/jde/>.

;; Please send any comments, bugs, or upgrade requests to
;; Paul Kinnucan at paulk@mathworks.com.

;;; Code:


;;;###autoload
(defconst jde-version "2.1.5"
  "JDE version number.")


(defvar jde-xemacsp (string-match "XEmacs" emacs-version)
  "Non-nil if we are running in the XEmacs environment.")

(defvar jde-xemacs20p (and jde-xemacsp (>= emacs-major-version 20)))

(require 'easymenu)
(require 'cl)
(require 'font-lock)
(require 'cc-mode)
(c-initialize-cc-mode)
(require 'cus-edit)
(require 'jde-compile)
(require 'jde-db)
(require 'jde-run)
(require 'jde-make)
(require 'jde-gen)
(require 'compile)
(require 'imenu)
(require 'speedbar)
(require 'browse-url)
(require 'beanshell)
(require 'jde-wiz)

;; This is copied straight out of andersl-java-font-lock.el
;; Necessary to set here because andersl assumes that the
;; buffer is in java-mode (it is actually in jde-mode).
(defun setup-fontlock()
  (if (not (assq 'jde-mode font-lock-defaults-alist))
      (setq font-lock-defaults-alist
	    (cons
	     (cons 'jde-mode

		   ;; jde-mode-defaults
		   '((java-font-lock-keywords java-font-lock-keywords-1
		      java-font-lock-keywords-2 java-font-lock-keywords-3)
		     nil nil ((?_ . "w") (?$ . "w")) nil
		     (font-lock-mark-block-function . mark-defun)))

	     font-lock-defaults-alist))))

(cond ((not jde-xemacsp)
       (if (< emacs-major-version 20)
	   (require 'andersl-java-font-lock))
       (setup-fontlock)))

;; From custom web page for compatibility between versions of custom:
(eval-and-compile
  (condition-case ()
      (require 'custom)
    (error nil))
  (if (and (featurep 'custom) (fboundp 'custom-declare-variable))
      nil ;; We've got what we needed
    ;; We have the old custom-library, hack around it!
    (defmacro defgroup (&rest args)
      nil)
    (defmacro defface (var values doc &rest args)
      (` (progn
	   (defvar (, var) (quote (, var)))
	   ;; To make colors for your faces you need to set your .Xdefaults
	   ;; or set them up ahead of time in your .emacs file.
	   (make-face (, var))
	   )))
    (defmacro defcustom (var value doc &rest args)
      (` (defvar (, var) (, value) (, doc))))))

(defgroup jde nil
  "Java Development Environment"
  :group 'tools
  :prefix "jde-")

(defgroup jde-project nil
  "JDE Project Options"
  :group 'jde
  :prefix "jde-")

(defcustom jde-key-bindings
  (list (cons "\C-c\C-v\C-c" 'jde-compile)
	(cons "\C-c\C-v\C-r" 'jde-run)
	(cons "\C-c\C-v\C-d" 'jde-db)
	(cons "\C-c\C-v\C-b" 'jde-build)
	(cons "\C-c\C-v\C-a" 'jde-run-menu-run-applet)
	(cons "\C-c\C-v\C-t" 'jde-db-menu-debug-applet)
	(cons "\C-c\C-v\C-j" 'bsh)
	(cons "\C-c\C-v\C-s" 'speedbar-frame-mode)
	(cons "\C-c\C-v\C-f" 'jde-wiz-implement-interface)
	(cons "\C-c\C-v\C-z" 'jde-wiz-import)
	(cons "\C-c\C-v\C-n" 'jde-browse-jdk-doc)
	(cons "\C-c\C-v\C-p" 'jde-save-project)
	(cons "\C-c\C-v\C-l" 'jde-gen-println))
  "*Specifies key bindings for the JDE.
The value of this variable is an association list. The car of
each element specifies a key sequence. The cdr specifies 
an interactive command that the key sequence executes. To enter
a key with a modifier, type C-q followed by the desired modified
keystroke. For example, to enter C-s (Control s) as the key to be
bound, type C-q C-s in the key field in the customization buffer."
  :group 'jde-project
  :type '(repeat
	  (cons :tag "Key binding"
	   (string :tag "Key")
	   (function :tag "Command")))
  :set '(lambda (sym val)
	  (if (boundp 'jde-mode-map)
	      (mapc (lambda (binding)
		      (define-key jde-mode-map (car binding) (cdr binding)))
		    val))
	  (set-default sym val)))

(defcustom jde-jdk-doc-url "http://www.javasoft.com/products/jdk/1.1/docs/index.html"
  "*URL of JDK documentation. 
This can point to a remote or local copy of the documentation. By
default, this variable points to the copy stored at JavaSoft's
website."
  :group 'jde-project
  :type 'string)

(defcustom jde-global-classpath nil
  "*Specify class paths for compile, run, and debug commands.
Use this option to specify the paths to the root directories of the
classes used by your project. The JDE uses the specified paths to
construct a classpath argument for the Java compile, run, and debug
commands. The JDE also allows you to set classpaths individually for
the compile, run, and debug commands. If you do not set a classpath
for those commands, the JDE uses the global classpath. If you do not
specify a global or a local classpath, the compile/run/debug commands
uses the value of the CLASSPATH environment variable, if set, as the
classpath."
  :group 'jde-project
  :type '(repeat (string :tag "Path")))

(defcustom jde-quote-classpath t
  "*Quote the classpath argument.
Set this option on when using the bash shell with Windows 95 or NT.
The semicolons in the classpath confuse the shell."
  :group 'jde-project
  :type 'boolean)

(defvar jde-project-name "default"
"Specifies name of project to which the current buffer belongs.")

(defcustom jde-project-file-name "prj.el"
  "*Specify name of JDE project file.
When it loads a Java source file, the JDE looks for a lisp file of
this name (the default is prj.el in the source file hierarchy. If it
finds such a file, it loads the file. You can use this file to set the
classpath, compile options, and other JDE options on a
project-by-project basis."
  :group 'jde-project
  :type 'string)

(defcustom jde-use-font-lock t
  "*Turn on font-locking if on.
	Set to nil to disable the use of font-locking."
  :group 'jde-project
  :type 'boolean)


(defcustom jde-compiler "javac"
  "*Java compiler.
Specifies the path to the compiler to be used to compile the source
in the current buffer. The default is the JDK compiler (javac)."
  :group 'jde-project
  :type 'string)

(defcustom jde-read-compile-args nil
"*Specify whether to prompt for additional compiler arguments.
If this variable is non-nil, the jde-compile command prompts
you to enter additional compiler arguments in the minibuffer.
These arguments are appended to those specified by customization
variables. The JDE maintains a history list of arguments 
entered in the minibuffer."
  :group 'jde-project
  :type 'boolean
)

(defvar jde-interactive-compile-args ""
"String of compiler arguments entered in the minibuffer.")

(defvar jde-interactive-compile-arg-history nil
"History of compiler arguments entered in the minibuffer.")


(defcustom jde-build-use-make nil
"*If true, use make to build JDE projects."
  :group 'jde-project
  :type 'boolean)

(defcustom jde-enable-abbrev-mode nil
"*Enable expansion of abbreviations in jde-mode.
See `jde-mode-abbreviations' for more information."
  :group 'jde-project
  :type 'boolean)

(defcustom jde-mode-abbreviations
  (list 
   (cons "ab" "abstract")
   (cons "bo" "boolean")
   (cons "br" "break")
   (cons "by" "byte")
   (cons "byv" "byvalue")
   (cons "cas" "cast")
   (cons "ca" "catch")
   (cons "ch" "char")
   (cons "cl" "class")
   (cons "co" "const")
   (cons "con" "continue")
   (cons "de" "default")
   (cons "dou" "double")
   (cons "el" "else")
   (cons "ex" "extends")
   (cons "fa" "false")
   (cons "fi" "final")
   (cons "fin" "finally")
   (cons "fl" "float")
   (cons "fo" "for")
   (cons "fu" "future")
   (cons "ge" "generic")
   (cons "go" "goto")
   (cons "impl" "implements")
   (cons "impo" "import")
   (cons "ins" "instanceof")
   (cons "in" "int")
   (cons "inte" "interface")
   (cons "lo" "long")
   (cons "na" "native")
   (cons "ne" "new")
   (cons "nu" "null")
   (cons "pa" "package")
   (cons "pri" "private")
   (cons "pro" "protected")
   (cons "pu" "public")
   (cons "re" "return")
   (cons "sh" "short")
   (cons "st" "static")
   (cons "su" "super")
   (cons "sw" "switch")
   (cons "sy" "synchronized")
   (cons "th" "this")
   (cons "thr" "throw")
   (cons "throw" "throws")
   (cons "tra" "transient")
   (cons "tr" "true")
   (cons "vo" "void")
   (cons "vol" "volatile")
   (cons "wh" "while")
   )
"*Abbreviations used for Java keywords.
To use these abbreviations, you must enable abbrev-mode (see
`jde-enable-abbrev-mode'). To use an abbreviation, enter the
abbreviation followed by a white-space character. To suppress
expansion, enter C-q white-space."
   :group 'jde-project
  :type '(repeat 
	  (cons :tag "jde-mode abbreviation"
		(string :tag "Abbreviation")
		(string :tag "Expansion"))))


;;;###autoload
(defun jde-set-compiler (compiler)
  "Specify the pathname of the compiler to be used to compile the
current buffer. Default is javac."
  (interactive
   "sEnter compiler (javac): ")
   (if (string= compiler "")
       (setq jde-compiler "javac")
     (setq jde-compiler compiler)))

(defvar jde-classpath-separator (if (eq system-type 'cygwin32) 
				    ";" path-separator)
  "The separator to use in a classpath.
This is usually the same as `path-separator'")

(defun jde-path-string-to-list (paths)
 "Converts a string of paths to a list of paths.
It is assumed that the default path separator for the
current platform (e.g., semicolon on Win32) separates
the paths."
 (let ((path-list (list))
       (m 0)
       (n (string-match jde-classpath-separator paths)))
   (while n
     (let ((path (substring paths m n)))
       (if path
	   (setq path-list
		 (cons path path-list)))
       (setq m (+ n 1))
       (setq n (string-match jde-classpath-separator paths m))))
   (setq n (length paths))
   (if (and (> n 0) (< m n))
       (let ((path (substring paths m n)))
	 (if path
	     (setq path-list
		   (cons path path-list)))))
   (setq path-list (nreverse path-list))))

;;;###autoload
(defun jde-set-global-classpath (classpath)
  "Specify the value of the -classpath argument for the Java compiler and
interpreter."
  (interactive 
   "sEnter classpath: ")
  (setq jde-global-classpath (jde-path-string-to-list classpath)))


(defun jde-build-compile-vm-args ()
  (let ((args " ")
	(len (length jde-compile-option-vm-args))
	(n 0))
    (while (< n len)
      (setq args (concat " -J"
			 (elt jde-compile-option-vm-args n)))
      (setq n (1+ n)))
    args))

;;;###autoload
(defun jde-browse-jdk-doc ()
  "Displays the JDK doc in a web browser. This function uses the URL
stored in the variable jde-jdk-doc-url to locate the JDK documentation."
  (interactive)
  (browse-url jde-jdk-doc-url browse-url-new-window-p))

(defun jde-make-compile-command (more-args)
  "Constructs the java compile command as: jde-compiler + options + buffer file name."
  (concat jde-compiler " " 
	  (jde-get-compile-options) 
	  (if (not (string= more-args ""))
	      (concat " " more-args))
	  " "
	  (file-name-nondirectory buffer-file-name)))


(defun jde-show-compile-options ()
  "Show the JDE Compile Options panel."
  (interactive)
  (customize-apropos "jde-compile-options" 'groups))

(defun jde-show-run-options ()
  "Show the JDE Run Options panel."
  (interactive)
  (customize-apropos "jde-run-options" 'groups))

(defun jde-show-debug-options ()
  "Show the JDE Debug Options panel."
  (interactive)
  (customize-apropos "jde-db-options" 'groups))

(defun jde-show-project-options ()
  "Show the JDE Debug Options panel."
  (interactive)
  (customize-apropos "jde-project" 'groups))

(defun jde-show-autocode-options ()
  "Show the JDE Autocode panel."
  (interactive)
  (customize-apropos "jde-gen" 'groups))


;;;###autoload
(defun jde-java-build ()
  "Use javac -depend to build the application whose main class is
specified by `jde-run-application-class'."
  (interactive)
  (cond 
   ((string= jde-run-application-class "")
    (message "No application main class specified."))
   (t
    (string-match "\\(\\(\\w*\\.\\)*\\)\\(\\w*\\b\\)"
		jde-run-application-class)
    (let* ((b1 (match-beginning 1))
	   (e1 (match-end 1))
	   (b2 (match-beginning 3))
	   (e2 (match-end 3))
	   (file (concat
		  (substring jde-run-application-class b2 e2)
		  ".java"))
	   (package (if e1
			(substring jde-run-application-class b1 e1)))
	   (directory (jde-db-search-src-dirs file package)))
      (cond
       (directory
	(let ((file-path 
	       (concat directory 
		       file))
	      (save-depend jde-compile-option-depend))
	  (find-file file-path)
	  (setq jde-compile-option-depend t)
	  (jde-compile)
	  (setq jde-compile-option-depend save-depend)))
       (t
	(message (concat "Could not find source for "
			 jde-run-application-class))))))))
    
;;;###autoload
(defun jde-build ()
  "Rebuild the entire project.
This command has two operating modes: java and make. In java mode,
this command uses javac's built-in make facility to rebuild a
project. In make mode, this command uses a user-specified make program
to rebuild the project. JDE configuration variables control which mode
is used.  In particular, if the variable `jde-build-use-make' is
non-nil, this command invokes the make program specified by the
variable `jde-make-program'. If the variable `jde-make-args' is a
non-empty string, this function uses its contents to invoke make;
otherwise, it prompts you to enter command-line arguments for make. If
`jde-build-use-make' is nil, this function invokes javac on the source
file specified by `jde-run-app-class', with the -depend option. This
causes javac to recompile all missing or out-of-date files required
to run the application's main class."
  (interactive)
  (if jde-build-use-make
      (jde-make
       (if (string= jde-make-args "")
	   (read-from-minibuffer (concat jde-make-program " ")
				 (nth 0 minibuffer-history))
	 jde-make-args))
    (jde-java-build)))

;; This is actually a no-op to get jde auto-loaded.
;;;###autoload
(defun jde-mode ()
  "Major mode for developing Java applications and applets."
  nil)

(define-derived-mode 
  jde-mode java-mode "JDE"
  "Major mode for developing Java applications and applets.
  \\{jde-mode-map}"

  ;; Define buffer-local variables.
  (make-local-variable 'jde-project-name)
  (make-local-variable 'jde-run-applet-document)


  ;; Enable support for automatic project switching.
  ;; This feature loads the appropriate project settings whenever
  ;; a user switches from a Java buffer belonging to one project
  ;; to a buffer belonging to another.
  (make-local-hook 'post-command-hook)
  (unless (find 'jde-detect-java-buffer-activation post-command-hook)
    (add-hook 'post-command-hook 'jde-detect-java-buffer-activation))

  (if jde-xemacsp
      (jde-insert-menu-in-XEmacs-menubar))

  (if jde-use-font-lock
      (jde-setup-syntax-coloring))

  (setq imenu-create-index-function 'jde-create-imenu-index)

  ;; Load the project file for this buffer. The project file
  ;; defines JDE options for a project.
  (jde-load-project-file)

  (setq jde-current-project jde-project-name)


  ;; Define abbreviations.
  (mapc (lambda (x) 
	  (define-mode-abbrev (car x) (cdr x)))
	jde-mode-abbreviations)

  (if jde-enable-abbrev-mode
      (abbrev-mode 1))

  ;; Reset the key bindings in case jde-mode-keymap
  ;; was not bound at startup.
  (custom-initialize-reset 'jde-key-bindings nil)
  )

(defun jde-setup-syntax-coloring() 
  ;; Set up syntax coloring.
  (cond (window-system

	 ;; If not XEmacs 20.1 turn on font lock.
	 ;; (XEmacs 21 has font-lock on by default.)
	 (if (or
	      (not jde-xemacsp)
	      (not
	       (and
		(eq emacs-major-version 21)
		(eq emacs-minor-version 0))))
	     (turn-on-font-lock))

	 (setq font-lock-maximum-decoration t)

	 (if (not jde-xemacsp)
	     (global-font-lock-mode 1))
	 )))

;; Setup jde-mode for font locking.
(if jde-xemacsp
    (put 'jde-mode 'font-lock-defaults
	 '((java-font-lock-keywords
	    java-font-lock-keywords-1 java-font-lock-keywords-2)
	   nil nil ((?_ . "w")) beginning-of-defun)))

;; Make jde-mode the default mode for Java source code buffers.
;; Prepend the jde-mode entry so that it shadows the java-mode
;; entry already in the list.
;;;###autoload
(setq auto-mode-alist
  (append
   '(("\\.java\\'" . jde-mode))
	auto-mode-alist))

(defvar jde-menu 
  (list "JDE"
	["Compile"           jde-compile t]
	["Run App"           jde-run t]
	["Debug App"         jde-db t]
	"-"
	;;["-"                 ignore nil]
	["Run Applet"        jde-run-menu-run-applet t]
	["Debug Applet"      jde-db-menu-debug-applet t]
	"-"  
	["Build"             jde-build t]
	["Interpret"         bsh t]
        "-" 
	(list "Templates"
	      ["Get/Set Pair..."  jde-gen-get-set t]
	      ["Println..."       jde-gen-println t]
	      (list "Listener"
		    ["Action"          jde-gen-action-listener t]
		    ["Window"          jde-gen-window-listener t]
		    ["Mouse"           jde-gen-mouse-listener t]
		    )
	      ["Other..."        jde-gen-code t]
	      )
	(list "Wizards"
	      ["Import class"        jde-wiz-import t]
	      ["Override Method"     jde-wiz-override-method t]
	      ["Implement Interface" jde-wiz-implement-interface t]
	      )
	["Speedbar"          speedbar-frame-mode t]
	["Browse JDK Doc"    jde-browse-jdk-doc t]
	(list "Options"
	      ["Compile"         jde-show-compile-options t]
	      ["Run"             jde-show-run-options t]
	      ["Debug"           jde-show-debug-options t]
	      ["Project"         jde-show-project-options t]
	      ["Autocode"        jde-show-autocode-options t]
	      "-"   
	      ["Save Project"    jde-save-project t]
	      )
	(list "Help"
	      ["Contents"        jde-show-help t]
	      "-"
	      (vector (concat "JDE " jde-version) 'ignore t)
	 )
	)
  "Menu for JDE.")

;; Define JDE menu for FSF Emacs.
(if (or (not jde-xemacsp) (featurep 'infodock))
    (easy-menu-do-define 'jde-menu 
			 jde-mode-map
			 "Menu for JDE."
			 jde-menu))

(defun jde-insert-menu-in-XEmacs-menubar ()
  "Insert JDE menu in the XEmacs menu bar."
  (if (and 
       (not (featurep 'infodock))
       (not (memq 'infodock c-emacs-features))
       (boundp 'current-menubar)
       current-menubar)
      (if (fboundp 'add-submenu)
	  (add-submenu nil jde-menu)
	(add-menu nil "JDE" (cdr jde-menu)))))


(defvar jde-new-buffer-menu
  (list
   "JDE New"
   ["Class..."         jde-gen-class-buffer t]
   ["Console..."       jde-gen-console-buffer t]
   ["Other..."         jde-gen-buffer t]
   )
  "Menu for creating new Java buffers.")

;; Add JDE New menu to Emacs Files menu.
(if (not jde-xemacsp)
    (let* ((mb (assq 'menu-bar global-map))
	   (files (assq 'files mb))
	   (menu (if (fboundp 'easy-menu-create-menu)
		     (easy-menu-create-menu 
		      (car jde-new-buffer-menu) (cdr jde-new-buffer-menu))
		   (easy-menu-create-keymaps 
		    (car jde-new-buffer-menu) (cdr jde-new-buffer-menu))))     
	   (menu-name (car jde-new-buffer-menu)))
      (define-key-after (cdr (cdr files)) [jde-new]
	(cons menu-name menu)
	'open-file))
  (unless (featurep 'infodock)
    (add-submenu '("File") jde-new-buffer-menu "Insert File...")))

;; Project File Functions

(defun jde-root-dir-p (dir)
  (let ((parent (concat dir "../")))
    (if (eq system-type 'windows-nt)
	(not (file-exists-p parent))
      (and 
       (string= (file-truename dir) "/")
       (string= (file-truename parent) "/")))))

(defun jde-find-project-file (dir)
  "Finds the project file for the Java source file in the current
buffer. Returns nil if it cannot find a project file in the
source file directory or an ascendant directory."
  (let ((file (find jde-project-file-name
		    (directory-files dir) :test 'string=)))
    (if file
	(concat dir file)
      (if (not (jde-root-dir-p dir))
	  (jde-find-project-file (concat dir "../"))))))

(defun jde-load-project-file ()
  "Loads the project file for the Java source file in the current
directory. Searches for the project file first in the source directory,
then in ascendant directories. Uses the first file that it encounters."
  (let ((prj-file (jde-find-project-file default-directory)))
    (if prj-file
	(load-file prj-file)
      (jde-set-variables-init-value))))

;;;###autoload
(defun jde-open-project-file ()
  "Opens the project file for the Java source file in the
current buffer."
  (interactive)
  (let ((prj-file (jde-find-project-file default-directory)))
    (if prj-file
	(find-file prj-file)
      (message "%s" "Project file not found."))))


(defun jde-save-delete (symbol)
  "Delete the call to SYMBOL from project file.
Leave point at the location of the call, or after the last expression."
  (save-excursion
    (let ((project-file (or
			 (jde-find-project-file default-directory)
			 (concat "./" jde-project-file-name))))
      (set-buffer (find-file-noselect project-file)))

    (goto-char (point-min))
    (catch 'found
      (while t
	(let ((sexp (condition-case nil
			(read (current-buffer))
		      (end-of-file (throw 'found nil)))))
	  (when (and (listp sexp)
		     (eq (car sexp) symbol))
	    (delete-region (save-excursion
			     (backward-sexp)
			     (point))
			   (point))
	    (throw 'found nil)))))
    (unless (bolp)
      (princ "\n"))))


(defun jde-save-variables ()
  "Save all JDE variables in project file."
  (jde-save-delete 'jde-set-project-name)
  (jde-save-delete 'jde-set-variables)
  (let ((standard-output (get-buffer jde-project-file-name)))
    (unless (bolp)
      (princ "\n"))

    (princ "(jde-set-project-name ")
    (prin1 jde-project-name)
    (princ ")\n")

    (princ "(jde-set-variables ")
    (mapatoms
     (lambda (symbol)
       (when 
	   (and (string-match "jde-" (symbol-name symbol))
		(get symbol 'custom-type))
	 (let ((value (symbol-value symbol)))	   
	     (princ "\n '(")
	     (princ symbol)
	     (princ " ")
	     (prin1 (custom-quote value))
	     ;; Check whether the user has changed the value of this
	     ;; variable in a customization buffer. If so, save flag
	     ;; so that custom knows that this value differs from
             ;; standard value.
	     (if (get symbol 'customized-value)
		 (princ " t)")
	       (princ ")"))		 
	     ))))
      (princ ")")
      (save-excursion
	(set-buffer (get-buffer jde-project-file-name))
	(unless (looking-at "\n")
	  (princ "\n"))
	(save-buffer))))

(defun jde-set-project-name (name)
  (setq jde-project-name name))

(defun jde-set-variables (&rest args)
  "Initialize JDE customization variables.  

Takes a variable number of arguments. Each argument 
should be of the form:

  (SYMBOL VALUE)

The value of SYMBOL is set to VALUE.
"
  (while args 
    (let ((entry (car args)))
      (if (listp entry)
	  (let* ((symbol (nth 0 entry))
		 (value (nth 1 entry))
		 (customized (nth 2 entry))
		 (set (or (get symbol 'custom-set) 'set-default)))
	    (if customized
		(put symbol 'customized-value (list value)))
	    (when (default-boundp symbol)
		   ;; Something already set this, overwrite it
		   (funcall set symbol (eval value)))
	    (setq args (cdr args)))))))

(defun jde-set-variables-init-value ()
  "Set each JDE variable to the value it has at Emacs startup."
  (interactive)
  (message "Setting JDE variables to startup values...")
  (mapatoms 
   (lambda (symbol) 
     (when 
	 (and (string-match "jde-" (symbol-name symbol))
	      (get symbol 'custom-type))
       (let ((saved-val (get symbol 'saved-value))
	     (std-val (get symbol 'standard-value))
	     (set (or (get symbol 'custom-set) 'set-default)))
	 (if saved-val
	     (funcall set symbol (eval (car saved-val)))
	   (funcall set symbol (eval (car std-val)))))))))
 
;;;###autoload
(defun jde-save-project (proj-name)
  "Saves local source file buffer options in project file.
This command provides an easy way to create and update a
project file for a Java project. Simply open a source file,
set the desired options, using the JDE Options menu, then
save the settings in the project file, using this command.
Now, whenever you open a source file from the same directory
tree, the saved settings will be restored for that file."
  (interactive
   (list 
    (let (prompt)
      (if (string= jde-project-name "")
	  (setq prompt "Enter project name: ")
	(setq prompt
	      (format "Enter project name (%s): " 
		      jde-project-name)))
      (read-string prompt))))
  (unless (string= proj-name "")
      (setq jde-project-name proj-name))
  (jde-save-variables))

(defun jde-convert-prj-file (file) 
"Converts a pre-JDE-2.0.7 project file to JDE-2.0.7 format.
Note: old project files did not preserve information about 
whether a saved value differed from the standard (JDE-defined)
value of a variable. Thus, all values are saved in the
converted file as though they were standard values. This means
that when JDE reloads the file, a custom buffer will customized
values as though they were standard. If you want to restore
a customized value to a standard value, simply make some
innocuous edit to the customized value and choose 
'Set for current session' from the customization buffer's
Set menu. Custom will then enable the Set menu option that
allows you to restore the value to its default value."
  (interactive "F")
  (let ((olddef (symbol-function 'jde-set-variables))
	(newdef 
	 (lambda (&rest args)
	   (while args 
	     (let ((entry (car args)))
	       (if (listp entry)
		   (let* ((symbol (nth 0 entry))
			  (value (nth 1 entry))
			  (set (or (get symbol 'custom-set) 'set-default)))
		     (when (default-boundp symbol)
		       ;; Something already set this, overwrite it
		       (funcall set symbol value))
		     (setq args (cdr args)))))))))
    (defalias 'jde-set-variables newdef)
    (require 'cus-edit)
    (load-file file)
    (jde-save-project jde-project-name)
    (defalias 'jde-set-variables olddef)))

;; Code to update JDE customization variables when a user switches
;; from a Java source buffer belonging to one project to a buffer
;; belonging to another.

(setq jde-current-project "")

(defun jde-reload-project-file ()
"Reloads the project file for a newly activated Java buffer when
the new buffer's project differs from the old buffer's."
  (if (not (string= jde-current-project jde-project-name))
      (progn
	(setq jde-current-project jde-project-name)
	(jde-load-project-file))))

;; (add-hook 'jde-entering-java-buffer-hooks 'jde-reload-project-file)

(defcustom jde-entering-java-buffer-hooks '(jde-reload-project-file)
"*Lists functions to run when entering a Java source buffer"
  :group 'jde-project
  :type 'hook)


(setq jde-current-buffer (current-buffer))

(defun jde-detect-java-buffer-activation ()
"Detects when a user activates a buffer.
If the activated buffer is a Java buffer, runs the 
`jde-entering-java-buffer' hooks."
  (let ((curr-buff (current-buffer)))
    (if (not
	 (equal curr-buff jde-current-buffer))
	(progn
	  (setq jde-current-buffer curr-buff)
	  (if (eq major-mode 'jde-mode)
		(run-hooks 'jde-entering-java-buffer-hooks))))))


(defun jde-count-open-java-buffers ()
  "Returns non-nil if any java buffers are open."
  (count 
   ".java"
   (buffer-list)
   :test
   (lambda (file-type buffer)
     (let ((file-name (buffer-file-name buffer)))
       (if file-name
	   (string-match file-type file-name))))))
	 

(defun jde-remove-jde-hook ()
  "Removes `jde-detect-java-buffer-activation-hook' when
all Java source buffers have been closed."
  (unless (> (jde-count-open-java-buffers) 1)
  (remove-hook 'post-command-hook 'jde-detect-java-buffer-activation)))

(add-hook 'kill-buffer-hook 'jde-remove-jde-hook)


;; JDE help

(defun jde-find-jde-data-directory ()
  "Return the path of the JDE data directory.
Returns  nil if the directory cannot be found. At some
point, XEmacs will include the JDE. Versions of XEmacs
that include JDE will store the JDE doc and Java clasees in a data
directory called jde. On all other Emacs versions, the JDE
expects to find the documentation and Java classes in subdirectories
of the directory that contains the file jde.el."
  (let ((dir (if jde-xemacsp
		 (locate-data-directory "jde"))))
    (if dir
	(replace-in-string dir "[\\]" "/")
      (file-name-directory (locate-library "jde")))))

(defun jde-find-jde-doc-directory ()
  "Return the path of the JDE documentation directory.
Returns  nil if the directory cannot be found. At some
point, XEmacs will include the JDE. Versions of XEmacs
that include JDE will store the JDE doc in a data
directory called jde. On all other Emacs versions, the JDE
expects to find the documentation in a subdirectory 
named doc of the directory that contains the file
jde.el."
  (jde-find-jde-data-directory))

 
;;;###autoload
(defun jde-show-help ()
  "Displays the JDE User's Guide in a browser."
  (interactive)
  (let* ((jde-dir (jde-find-jde-doc-directory))
	 (jde-help
	  (if jde-dir
	      (if (and jde-xemacsp
		       (locate-data-directory "jde"))
		  (expand-file-name "jde-ug.html" jde-dir)
		(expand-file-name "doc/jde-ug.html" jde-dir)))))	  
    (if jde-help
	(if (eq system-type 'windows-nt)
	    (browse-url jde-help browse-url-new-window-p)
	  (browse-url (concat "file:" jde-help)  browse-url-new-window-p))
      (signal 'error '("Cannot find JDE help file.")))))


;; speedbar

(defun jde-make-imenu-patterns ()
  "Makes a replacement for the regular expression indexing  
patterns in imenu, which are too slow for the JDE's
speedbar. See `imenu-generic-expression'."
  (let* ((capital "A-Z\300-\326\330-\337")
	 (letter "a-zA-Z_$\300-\326\330-\366\370-\377")
	 (digit "0-9")
	 (white-space "\\s-")
	 (optional-white-spaces
	  (concat white-space "*"))
	 (bol "^")
	 (eol "$")
	 (not-comment (concat optional-white-spaces "[^.*/]*"))
	 (anything ".*")

	 (primitive-type 
	  (concat "\\<\\(b\\(oolean\\|yte\\)"
		  "\\|char\\|double\\|float\\|int"
		  "\\|long\\|short\\|void\\)\\>"))
	 (primitive-type-count 2)

	 (primitive-type-no-void 
	  (concat "\\<\\(b\\(oolean\\|yte\\)"
		  "\\|char\\|double\\|float\\|int"
		  "\\|long\\|short\\)\\>"))
	 (primitive-type-no-void-count 2)

	 (identifier
	  (concat "\\<\\([" letter "][" letter digit "]*\\)\\>"))
	 (identifier-count 1)

	 ;; Class types are assumed to begin with a capital letter.
	 (class-type
	  (concat
	   "\\<\\([" capital "][a-zA-Z_" digit "]*\\)\\>"))
	 (class-type-count 1)

	 (modifier
	  (concat 
	   "\\<\\(abstract\\|const\\|final\\|native\\|"
	   "p\\(r\\(ivate\\|otected\\)\\|ublic\\)\\|"
	   "s\\(tatic\\|ynchronized\\)\\|transient\\|volatile\\)\\>"))
	 (modifier-count 4)

	 (optional-modifiers
	  (concat
	   "\\(" modifier optional-white-spaces "\\)*"))
	 (optional-modifiers-count 5)

	 (modifiers
	  (concat
	   "\\(" modifier optional-white-spaces "\\)+"))
	 (modifiers-count 5)

	 (optional-array-modifier
	  (concat
	   "\\(\\[" optional-white-spaces "\\]" optional-white-spaces "\\)*"))
	 (optional-array-modifier-count 1)

	 (class
	  (concat
	   bol
	   optional-white-spaces
	   optional-modifiers
	   "\\<class\\>"
	   optional-white-spaces
	   identifier))

	 (class-count (+ optional-modifiers-count
			  identifier-count))
	   
	 (interface
	  (concat
	   bol
	   optional-white-spaces
	   optional-modifiers
	   "\\<interface\\>"
	   optional-white-spaces
	   identifier))

	 (interface-count (+ optional-modifiers-count
			      identifier-count))

	 (constructor
	  (concat
	   bol
	   optional-white-spaces
	   modifiers                 ;; e.g., public
	   class-type                ;; e.g., Foo
	   optional-white-spaces
	   "("))

	 (constructor-count (+ optional-modifiers-count
			       class-type-count))

	 ;; Pattern for methods that return a primitive type
	 (method1
	  (concat
	   bol
	   not-comment
	   primitive-type          ;; e.g., int
	   optional-white-spaces
	   optional-array-modifier ;; e.g., []
	   identifier              ;; e.g., foo
	   optional-white-spaces
	   "("))

	 (method1-count (+ primitive-type-count
			   optional-array-modifier-count
			   identifier-count))
	
	 ;; Pattern for methods that return a class type
	 (method2
	  (concat
	   bol
	   not-comment
	   class-type
	   optional-white-spaces
	   optional-array-modifier
	   identifier
	   optional-white-spaces
	   "("))

	 (method2-count (+ class-type-count
			   optional-array-modifier-count
			   identifier-count))

	 (variable1
	  (concat
	   bol
	   optional-white-spaces
	   optional-modifiers
	   optional-white-spaces
	   class-type
	   optional-white-spaces
	   optional-array-modifier
	   optional-white-spaces
	   identifier
	   optional-white-spaces
	   "\\(;\\|=\\)"))

	 (variable1-count (+ optional-modifiers-count
			     class-type-count
			     optional-array-modifier-count
			     identifier-count))

	 (variable2
	  (concat
	   bol
	   optional-white-spaces
	   optional-modifiers
	   optional-white-spaces
	   primitive-type-no-void
	   optional-white-spaces
	   optional-array-modifier
	   optional-white-spaces
	   identifier
	   optional-white-spaces
	   "\\(;\\|=\\)"))

	 (variable2-count (+ optional-modifiers-count
			     primitive-type-no-void-count
			     optional-array-modifier-count
			     identifier-count))

	 (exp 
	  (`
	   (
	    (nil ;; methods index
	     (,  method1) (, method1-count))
	    (nil ;; methods index
	     (,  method2) (, method2-count))
	    ("Constructors" ;; constructors index
	     (,  constructor) (, constructor-count))
	    ("Variables"
	     (, variable1) (, variable1-count))
	    ("Variables"
	     (, variable2) (, variable2-count))
	    ("Classes"
	     (, class) (, class-count))
	    ("Interfaces"
	     (, interface) (, interface-count))
	    )
	   ))
	 )
    exp))


;;;
;;; Java index gathering function.
;;;

(defun jde-create-imenu-index ()
;; Based on imenu--generic-function
  "Return an index of the current buffer as an alist.

PATTERN is an alist with elements that look like this: (MENU-TITLE
REGEXP INDEX).

MENU-TITLE is a string used as the title for the submenu or nil if the
entries are not nested.

REGEXP is a regexp that should match a construct in the buffer that is
to be displayed in the menu; i.e., function or variable definitions,
etc.  It contains a substring which is the name to appear in the
menu.  See the info section on Regexps for more information.

INDEX points to the substring in REGEXP that contains the name (of the
function, variable or type) that is to appear in the menu.

For emacs-lisp-mode for example PATTERN would look like:

'((nil \"^\\\\s-*(def\\\\(un\\\\|subst\\\\|macro\\\\|advice\\\\)\\\\s-+\\\\([-A-Za-z0-9]+\\\\)\" 2)
  (\"*Vars*\" \"^\\\\s-*(def\\\\(var\\\\|const\\\\)\\\\s-+\\\\([-A-Za-z0-9]+\\\\)\" 2)
  (\"*Types*\" \"^\\\\s-*(def\\\\(type\\\\|struct\\\\|class\\\\|ine-condition\\\\)\\\\s-+\\\\([-A-Za-z0-9]+\\\\)\" 2))'

Returns an index of the current buffer as an alist.  The elements in
the alist look like: (INDEX-NAME . INDEX-POSITION).  They may also be
nested index lists like (INDEX-NAME . INDEX-ALIST) depending on
pattern.

\(imenu--generic-function PATTERN\)."

  (let* ((patterns (jde-make-imenu-patterns))
	 (case-fold-search nil)
	 (index-alist (list 'dummy))
	 (found nil)
	 (global-regexp 
	  (concat "\\(" 
		  (mapconcat
		   (function (lambda (pattern) (identity (cadr pattern)))) 
		   patterns "\\)\\|\\(") 
		  "\\)"))
	 prev-pos
	 (ppos (point-max)))

    (goto-char (point-max))

    (imenu-progress-message prev-pos 0 t)
    (save-match-data
      (while (re-search-backward global-regexp nil t)
	(imenu-progress-message prev-pos nil t)
        (setq found nil)
	(save-excursion
	  (goto-char (match-beginning 0))
	  (mapcar 
	   (function 
	    (lambda (pat) 
	      (let ((menu-title (car pat))
		    (regexp (cadr pat))
		    (index (caddr pat)))
		    (if (and (not found) ; Only allow one entry;
			     (re-search-forward regexp ppos t))
			(let ((beg (match-beginning index))
			      (end (match-end index)))
			  (setq found t)
			  (setq ppos beg)
			  (push 
			   (cons (buffer-substring-no-properties beg end) beg)
			   (cdr 
			    (or (assoc menu-title index-alist)
				(car (push 
				      (cons menu-title '()) 
				      index-alist))))))))))
	   patterns))))
    (imenu-progress-message prev-pos 100 t)
    (let ((main-element (assq nil index-alist)))
      (nconc (delq main-element (delq 'dummy index-alist)) main-element))))

(provide 'jde)

;; Change History
;;
;; $Log: jde.el,v $
;; Revision 1.26  2002/02/27 10:32:23  vltsccm
;; gchiozzi: Forced making for emacs. Problems on RH7.2 with xemacs automatically found.
;;
;; Revision 1.25  2002/02/21 14:59:56  vltsccm
;; emacs1.25
;;
;; Revision 1.24  2002/02/20 07:40:20  vltsccm
;; emacs1.24
;;
;; Revision 1.23  2002/02/09 17:14:06  vltsccm
;; emacs1.23
;;
;; Revision 1.22  2000/07/03 14:04:53  vltsccm
;; emacs1.22
;;
;; Revision 1.21  1999/11/21 21:05:01  vltsccm
;; emacs1.21
;;
;; Revision 1.20.1.1  1999/11/09 02:50:57  vltsccm
;; emacs1.20.1
;;
;; Revision 1.20  1999/06/09 14:55:25  vltsccm
;; emacs1.20
;;
;; Revision 1.19  1999/06/09 14:55:25  vltsccm
;; emacs1.19
;;
;; Revision 1.18  1999/06/09 14:55:24  vltsccm
;; emacs1.18
;;
;; Revision 1.17  1999/06/09 14:55:24  vltsccm
;; emacs1.17
;;
;; Revision 1.16  1999/06/09 14:55:24  vltsccm
;; emacs1.16
;;
;; Revision 1.15  1999/06/09 14:55:23  vltsccm
;; emacs1.15
;;
;; Revision 1.14  1999/06/09 14:55:23  vltsccm
;; emacs1.14
;;
;; Revision 1.13  1999/06/09 14:55:23  vltsccm
;; emacs1.13
;;
;; Revision 1.12  1999/06/09 14:55:22  vltsccm
;; emacs1.12
;;
;; Revision 1.11  1999/06/09 14:55:22  vltsccm
;; emacs1.11
;;
;; Revision 1.10  1999/06/09 14:55:21  vltsccm
;; emacs1.10
;;
;; Revision 1.9  1999/06/09 14:55:21  vltsccm
;; emacs1.9
;;
;; Revision 1.8  1999/06/09 14:55:21  vltsccm
;; emacs1.8
;;
;; Revision 1.7  1999/06/09 14:55:20  vltsccm
;; emacs1.7
;;
;; Revision 1.6  1999/06/09 14:55:20  vltsccm
;; emacs1.6
;;
;; Revision 1.5  1999/06/09 14:55:20  vltsccm
;; emacs1.5
;;
;; Revision 1.4  1999/06/09 14:55:19  vltsccm
;; emacs1.4
;;
;; Revision 1.3  1999/06/09 14:55:19  vltsccm
;; emacs1.3
;;
;; Revision 1.2  1999/06/09 14:55:19  vltsccm
;; emacs1.2
;;
;; Revision 1.90  1999/03/10 18:56:43  paulk
;; Fixed in bug in jde-find-jde-data-directory
;;
;; Revision 1.89  1999/03/10 17:00:09  paulk
;; Changed version to 2.1.5.
;;
;; Revision 1.88  1999/02/26 15:56:38  paulk
;; Version 2.1.5b4
;;
;; Revision 1.87  1999/02/17 19:17:11  paulk
;; 2.1.5b3 version number.
;;
;; Revision 1.86  1999/02/15 01:15:09  paulk
;; Updated version number.
;;
;; Revision 1.85  1999/02/12 15:26:09  paulk
;; Added menu item (Wizards->Import Class) for generating import statements.
;;
;; Revision 1.84  1999/02/08 18:02:57  paulk
;; *** empty log message ***
;;
;; Revision 1.83  1999/02/04 01:38:37  paulk
;; Provided a fix for ensuring that key bindings are always set. The fix is
;; to do a custom-initialize-reset on the jde-key-bindings variable in the jde-mode
;; function. The jde-mode-map is updated with the key bindings as a side effect of
;; resetting the variable.
;;
;; Revision 1.82  1999/02/04 01:22:23  paulk
;; Fixed some keybindings. Also backed out Matthew Moore's fix for ensuring
;; that jde-mode-keymap gets set at startup since it seems to break
;; java-mode. I'll try to come up with another fix later.
;;
;; Revision 1.81  1999/02/03 22:57:31  paulk
;; *** empty log message ***
;;
;; Revision 1.80  1999/02/03 01:54:46  paulk
;; Minor fix to debug applet item.
;;
;; Revision 1.79  1999/02/03 01:12:11  paulk
;; Fixed a bug in the initialization code for jde-key-bindings.
;; Thanks to Matthew Moore <matthew.moore@Schwab.COM> for this fix.
;;
;; Added a menu item for debugging applets.
;;
;; Revision 1.78  1999/01/15 22:11:04  paulk
;; Some XEmacs patches that I missed.
;;
;; Revision 1.77  1999/01/15 21:57:48  paulk
;; Added XEmacs compatibility changes from Andy Piper.
;;
;; Revision 1.76  1998/12/09 00:56:31  paulk
;; Put jde-compile variables in a new file jde-compile.el.
;;
;; Revision 1.75  1998/11/27 10:10:03  paulk
;; Updated JDE version number to 2.1.3.
;;
;; Revision 1.74  1998/11/22 18:13:57  paulk
;; Added menu items for the BeanShell and method override and interface wizards.
;;
;; Revision 1.73  1998/09/13 02:01:53  paulk
;; Fixed a small bug in key binding code.
;;
;; Revision 1.72  1998/09/13 01:49:35  paulk
;; Added support for customization of JDE key bindings via the
;; variable jde-key-bindings.
;;
;; Revision 1.71  1998/09/13 00:32:48  paulk
;; Added System.out.println template to the Generate menu.
;;
;; Revision 1.70  1998/09/07 02:50:31  paulk
;; This version includes the latest version of jde-gen.el, which was inadvertently
;; replaced by an older version in the last release. This version also includes
;; a newer version of speedbar.el that seems to work better with NT/Emacs 20.3.1
;; than the one that comes with the 20.3.1 distribution.
;;
;; Revision 1.69  1998/08/28 12:56:06  paulk
;; *** empty log message ***
;;
;; Revision 1.68  1998/08/28 12:52:52  paulk
;; Updated version number.
;;
;; Revision 1.67  1998/07/28 03:15:33  paulk
;; Removed a diagnostic message.
;;
;; Revision 1.66  1998/07/28 03:12:40  paulk
;; Updated version number to 2.0.9.
;;
;; Revision 1.65  1998/07/28 03:12:05  paulk
;; Fixed the following project file bugs:
;;
;;   * JDE does not store the project name in the project file.
;;   * JDE does not save variables whose value is nil.
;;   * JDE does not reset variables to initial values when
;;     switching to a buffer that is not part of a project.
;;
;; Revision 1.64  1998/07/22 00:10:07  paulk
;; Now requires cus-edit. This fixes custom-quote is void bug.
;;
;; Fixed bug in jde-set-variables that prevented loading of
;; project files in the new format.
;;
;; Revision 1.63  1998/07/10 00:49:24  paulk
;; Changed jde-save-variables to mark variables that have been customized\n in the current session. Changed jde-set-variables to store the value\n of a customized variable in the customized-value property of the\n variable. This enables Custom to recognize the variable as customized.\n\n  Added jde-convert-prj-file, a function that converts old project files to \n \
;; JDE-2.0.7 format.\n\n Fixed a bug in the function that finds the JDE documentation.
;;
;; Revision 1.62  1998/07/09 04:33:57  paulk
;; Change the way that the JDE saves and restores project-specific values of
;; customization variables to be compatible with custom. This fixes the bug
;; that caused errors when loading customized JDE variables from a .emacs file.
;;
;; Revision 1.61  1998/07/04 05:25:15  paulk
;; Should have been does not turn on font-lock if XEmacs 21.0.
;;
;; Revision 1.60  1998/07/04 00:53:58  paulk
;; Now does not turn on font-lock if XEmacs 20.1.
;;
;; Revision 1.59  1998/07/02 05:33:13  paulk
;; Fixed bugs in the jde-show-help function that prevented display
;; of help on XEmacs and NT/Emacs.
;;
;; Revision 1.58  1998/06/30 03:35:10  paulk
;; Added the customization variable `jde-read-compile-args'. If non-nil,
;; this variable causes the jde-compile command to read compilation options
;; from the minibuffer and append them to the options specified by
;; the `jde-compile-option group of customization' variables. The JDE
;; maintains a history of compiler options entered in the minibuffer.
;;
;; Revision 1.57  1998/06/29 03:18:11  paulk
;; Use fboundp instead of Emacs version to determine whether
;; easy-menu-create-menu is bound.
;;
;; Revision 1.56  1998/06/27 03:43:10  paulk
;; Updated release to 2.0.3
;;
;; Revision 1.55  1998/06/27 03:40:20  paulk
;; Fixed bug where the JDE was invoking global-font-lock-mode on XEmacs,
;; where it is not defined.
;;
;; Updated JDE to call easy-menu-create-menu instead of easy-menu-create-keymaps
;; on Emacs 20. (The former replaces the latter as of Emacs 20.x);
;;
;; Revision 1.54  1998/06/21 05:23:56  paulk
;; Updated JDE version number to 2.0.2.
;;
;; Revision 1.53  1998/06/21 05:22:59  paulk
;; Changed buffer change code to reload a project file
;; when a user changed jde-mode buffers, not just .java
;; buffers. This allows using extensions other than .java
;; for Java source code files.
;;
;; Made post-command-hook buffer local to improve performance.
;;
;; Thanks to David J. Biesack( sasdjb@unx.sas.com) for the above changes.
;;
;; Revision 1.52  1998/06/18 18:14:08  paulk
;; Added XEmacs compatibility changes requested by xemacs.org.
;;
;; Revision 1.51  1998/06/17 03:49:58  paulk
;; Added support for abbreviations.
;;
;; Revision 1.50  1998/05/29 01:42:08  paulk
;; Added no-op function for jde-mode to facilitate autoloading.
;; Thanks to Andy Piper <andyp@parallax.co.uk> for the suggestion.
;;
;; Revision 1.49  1998/05/27 06:33:29  paulk
;; Updated JDE version number to 2.01.
;;
;; Revision 1.48  1998/05/27 05:49:12  paulk
;; Added autoload comments for JDE functions.
;;
;; Revision 1.47  1998/05/17 06:21:58  paulk
;; Changed names of the Files->JDE New->Custom and JDE-Autocode->Custom
;; to Other...
;;
;; Revision 1.46  1998/04/19 13:07:19  kinnucan
;; Updated version number.
;;
;; Revision 1.45  1998/04/19 13:05:56  kinnucan
;; Updated version number.
;;
;; Revision 1.44  1998/04/18 14:06:43  kinnucan
;; Replace imenu--generic-function with jde-create-imenu-index
;; as the indexing function for Java source buffers.
;; jde-create-imenu-index is basically imenu--generic-function
;; with a fix for a bug that generates multiple indexes for
;; the same symbol.
;;
;; Revision 1.43  1998/04/09 04:52:47  kinnucan
;; * Added menu items for inserting custom code templates in buffers.
;;   The items are:
;;
;;   Files->JDE New->Custom
;;
;;   JDE->Generate->Custom
;;
;; Revision 1.42  1998/04/08 04:40:01  kinnucan
;; * Fixed jde-save-variables and jde-set-variables so that they
;;   operate more like the corresponding custom functions. This
;;   was necessary to support project-by-project customization
;;   of autocode templates.
;;
;; Revision 1.41  1998/04/06 05:57:25  kinnucan
;; * Removed extraneous New option from JDE menu.
;;
;; Revision 1.40  1998/04/06 03:44:36  kinnucan
;; * Added JDE New submenu to the Emacs Files menu. The new submenu has
;;   commands for creating buffers with skeleton code for the following
;;   types of classes:
;;
;;   - Generic class
;;   - Main class for a console application
;;
;; Revision 1.39  1998/04/01 05:32:55  kinnucan
;; * Added code generation for
;;
;;   - new Java source file
;;   - Get/set variable method pair
;;   - Action listener
;;   - Window listener
;;   - Mouse listener
;;
;; Revision 1.38  1998/03/30 22:20:24  kinnucan
;; * Fixed separator code in JDE menu definition.
;;
;;   Thanks to Kai Grossjohann <grossjohann@ls6.cs.uni-dortmund.de>
;;   for providing this fix.
;;
;; Revision 1.37  1998/03/27 04:46:19  kinnucan
;; Added the jde-build command.
;;
;; Revision 1.36  1998/03/23 06:44:23  kinnucan
;; * Set up to activate project tracking when the first Java buffer
;;   is loaded and deactivate project tracking when the last
;;   Java buffer is closed.
;;
;; * Removed update buffer command as it is no longer necessary
;;   because all customization variables are now global.
;;
;; * Changed save project command to prompt for a project name,
;;   which is required for automatic project tracking.
;;
;; Revision 1.35  1998/03/22 07:21:07  kinnucan
;; * Changed the way the JDE maintains project settings. Previously
;;   most JDE customization variables were buffer local. This was basically
;;   a way of letting Emacs manage project-dependent customization
;;   settings. However, this approach conflicts with the new (as of
;;   Emacs 20) Emacs customization feature. To avoid the conflict,
;;   the JDE now manages the task of keeping buffers up-to-date.
;;   In particular, all variables are global to permit easy
;;   customization. Whenever a user switches from one Java buffer to
;;   another, the JDE checks to see if the "to" buffer is part of the
;;   same project as the "from" buffer. If not the JDE loads the
;;   project file for the "to" buffer, thus updating the customization
;;   variables to the specific JDE settings for the "to" buffer's project.
;;
;; * Fixed bug that prevented jde-compile-option-command-line-args
;;   from working correctly.
;;
;; Revision 1.34  1998/03/19 20:55:59  kinnucan
;; - Fixed bug that prevented JDE->Options->Debug from working.
;;
;; - Updated version number.
;;
;; Revision 1.33  1998/03/05 07:49:40  kinnucan
;; Made jde-db-source-directories non-global again to
;; eliminate the problem of project files changing
;; its value.
;;
;; Revision 1.32  1998/03/05 07:14:36  kinnucan
;; Updated version number to 1.9.5
;;
;; Revision 1.31  1998/03/03 23:10:43  kinnucan
;; - Fixed bug in imenu regexp for speedbar that falsely taggex
;;   method-like constructs in comments
;;
;; - Added file: prefix to path to User's Guide.
;;
;; - Fixed bug that caused setting jde-compile-option-vm-args to wipe
;;   out all other compile options.
;;
;; Revision 1.30  1998/02/25 17:11:47  paulk
;; Added jde-show-help command. This command displays the
;; JDE User's Guide in a browser.
;;
;; Revision 1.29  1998/02/23 23:35:34  kinnucan
;; * Reorganized JDE menu. Eliminated the Compile Options item
;;   and added the following items:
;;
;;   JDE->Options->Compile       Shows Compile Options buffer
;;   JDE->Options->Run           Shows Run Options Buffer
;;   JDE->Options->Debug         Shows Debug Options Buffer
;;   JDE->Options->Project       Show Project Options Buffer
;;   JDE->Options->Update Buffer Updates buffer to global options values
;;
;; * Added the jde-save-project command.
;;
;;   This command saves the values of all local JDE options (i.e.,
;;   customization) variables in the project file. This provides
;;   an easy way of creating a project file for a project.
;;   Simply set the desired options, using the JDE Options menu.
;;   Then, save the results in the project file.
;;
;; Revision 1.28  1998/02/18 03:14:04  kinnucan
;; Corrected some doc strings.
;;
;; Revision 1.27  1998/02/18 02:33:07  kinnucan
;; * Added customization support by redefining all customization
;;   variables, using defcustom.
;;
;; * Defined two customization groups: jde and jde-compile-options.
;;
;; * Replaced the jde-classpath variable with jde-global-classpath.
;;
;; * Added customization option jde-quote-classpath.
;;
;; * Added variable jde-project-name.
;;
;; * Replace variable jde-compile-options with
;;   jde-compile-option-command-line-args.
;;
;; * Added the following compile option variables:
;;
;;   jde-compile-option-classpath
;;   jde-compile-option-directory
;;   jde-compile-option-deprecation
;;   jde-compile-option-debug
;;   jde-compile-option-nodebug
;;   jde-compile-option-optimize
;;   jde-compile-option-optimize-interclass
;;   jde-compile-option-option-depend
;;   jde-compile-option-vm-args
;;   jde-compile-option-verbose
;;   jde-compile-option-nowarn
;;   jde-compile-option-encoding
;;
;;   All of these variables are made buffer local.
;;
;; * Replaced jde-set-classpath function with
;;   jde-set-global-classpaht function.
;;
;; * Added the following functions
;;
;;   jde-path-string-to-list
;;   jde-build-classpath-arg
;;   jde-build-compile-vm-args
;;   jde-get-compile-options
;;
;; Revision 1.26  1998/02/13 10:23:52  kinnucan
;; Fixed so that the JDE menu appears in the XEmacs menu bar
;; only when a Java buffer is active.
;;
;; Revision 1.25  1998/02/13 09:36:59  kinnucan
;; Added jde-use-font-lock variable. If t (the default), jde turns on
;; font-locking for java files.
;;
;; Revision 1.24  1998/02/12 21:28:47  kinnucan
;; Advised imenu-default-create-index-function to set case-fold-search
;; to nil (case-sensitive) when creating indexes.
;;
;; Revision 1.23  1998/02/12 06:34:37  kinnucan
;; Fixed some bugs in imenu regular expressions, including lack of a re
;; for indexing primitive type variables. Thanks to
;; David J. Biesack <sasdjb@unx.sas.com> for spotting some bugs.
;;
;; Revision 1.22  1998/02/12 05:46:45  kinnucan
;; Added fix to bug that prevented fontlocking on Emacs 20.2
;;
;; Revision 1.21  1998/01/29 11:24:43  paulk
;; Fixed typo.
;;
;; Revision 1.20  1998/01/29 11:23:10  paulk
;; Made various changes to ensure compatibility with XEmacs.
;;
;; Revision 1.19  1998/01/20 13:35:51  paulk
;; Use browse-url instead of browse-url-of-file.
;;
;; Revision 1.18  1998/01/20 12:45:40  paulk
;; Require cc-mode.
;;
;; Revision 1.17  1998/01/20 05:19:26  kinnucan
;; Added code to set up andersl font locking. Necessary because
;; andersl assumes that the buffer is in java-mode.
;;
;; Revision 1.16  1998/01/19 05:13:54  kinnucan
;; * Made JDE into a major mode (jde-mode) derived from java-mode.
;; * The JDE now uses the browse-url package to display JDK documentation.
;; * Deleted the variable jde-hook (it is replaced by jde-mode-hook).
;; * Deleted the variables jde-web-browser and jde-doc-dir as they duplicate
;;   functionality provided by browse-url.
;;
;; Revision 1.15  1998/01/19 01:34:04  kinnucan
;; *** empty log message ***
;;
;; Revision 1.14  1997/10/30 05:38:00  kinnucan
;; 1) Made configuration variables settable.
;; 2) Made jde-db-source-directories buffer local.
;;
;; Revision 1.13  1997/10/20 05:21:20  kinnucan
;; Now requires andersl-java-font-lock only for Emacs versions < 20.
;;
;; Revision 1.12  1997/10/18 05:24:52  kinnucan
;; 1. Changed key bindings to use the two prefix keys C-c C-v.
;;
;; 2. Fixed infinite recursion bug in jde-find-project-file.
;;
;; Revision 1.11  1997/10/07 03:44:24  kinnucan
;; Required cl.
;;
;; Revision 1.10  1997/10/06 13:17:25  kinnucan
;; Removed last usage of obsolete bashify function.
;;
;; Revision 1.9  1997/10/06 04:02:27  kinnucan
;; Added jde-compiler variable and associated set command. Lets you
;; configure the JDE to use the compiler of your choice on a buffer
;; by buffer basis.
;;
;; Revision 1.8  1997/10/04 10:13:10  kinnucan
;; Added key bindings for menu commands.
;;
;; Revision 1.7  1997/10/03 05:57:54  kinnucan
;; 1. Revamped imenu regular expressions.
;; 2. Stopped quoting compile command arguments for bash under Win32.
;;
;; Revision 1.6  1997/10/01 03:13:04  kinnucan
;; Changed name of JDE menu from "Java" to "JDE" to avoid conflict
;; with cc-mode 5.18 menu, which is named "Java".
;;
;; Revision 1.5  1997/09/04 03:40:01  kinnucan
;; Updated version number.
;;
;; Revision 1.4  1997/09/04 03:38:13  kinnucan
;; 1. Made jde configuration variables buffer local to support automatic
;;    loading of project files.
;;
;; 2. Added Run Applet command to the jde menu.
;;
;; Revision 1.3  1997/08/28 02:54:17  kinnucan
;; Eliminated single quotes around path in jde-browse-jdk-doc.
;;
;; Revision 1.2  1997/08/26 08:50:29  kinnucan
;; Added jde-set-classpath command, which lets you set the classpath for
;; compiling and running applications.
;;
;; Revision 1.1  1997/06/18 17:25:57  paulk
;; Initial revision
;;
;; Revision 1.8  1997/06/18 17:20:00  paulk
;; Initial checkin.
;;


;;; jde.el ends here.







