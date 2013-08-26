;;; jde-wiz.el
;; $Revision: 1.26 $ 

;; Author: Paul Kinnucan <paulk@mathworks.com>
;; Maintainer: Paul Kinnucan
;; Keywords: java, tools

;; Copyright (C) 1997, 1998 Paul Kinnucan.

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

(require 'beanshell)

(defun jde-wiz-get-imports()
  (let ((import-re "import[ ]+\\(.*\\)[ ]*;")
	(imports nil))    
    (save-excursion
      (goto-char (point-max))
      (while (re-search-backward import-re (point-min) t)
	(looking-at import-re)
	(setq imports (nconc imports 
			     (list (buffer-substring-no-properties 
				    (match-beginning 1) 
				    (match-end 1)))))))
    imports))

(defun jde-wiz-get-package-name ()
  (let ((package-re "package[ \t]+\\(.*\\)[ \t]*;"))
    (save-excursion
      (goto-char (point-max))
      (when (re-search-backward package-re (point-min) t)
	(looking-at package-re)
	(buffer-substring-no-properties
		       (match-beginning 1)
		       (match-end 1))))))

(defun jde-wiz-get-import-insertion-point ()
  (let ((ip-re
	 (list (cons "import[ ]+\\(.*\\)[ ]*;" 'backward)
	       (cons "package[ \t]+\\(.*\\)[ \t]*;" 'backward)
	       (cons "^$" 'forward)))
	insertion-point n)
    (save-excursion
      (setq i 0)
      (setq n (length ip-re))
      (while (and
	      (not insertion-point)
	      (< i n))
	(let ((re (car (nth i ip-re)))
	      (direction (cdr (nth i ip-re))))
	  (if (eq direction 'forward)
	      (progn
		(goto-char (point-min))
		(setq insertion-point (re-search-forward re (point-max) t)))
	    (goto-char (point-max))
	    (setq insertion-point (re-search-backward re (point-min) t)))
	    (when insertion-point
	      (forward-line 1)
	      (setq insertion-point (point))))
	(setq i (+ i 1))))
    insertion-point))

(defun jde-wiz-insert-imports (new-imports) 
  (let ((existing-imports
	 (jde-wiz-get-imports))
	i n)
    (save-excursion
      (goto-char (jde-wiz-get-import-insertion-point))
      (setq i 0)
      (setq n (length new-imports))
      (while (< i n)
	(let ((new-import 
	       (nth i new-imports)))
	  (when (not (find new-import existing-imports :test 'string=))
	    (insert
	     (concat "import " new-import ";\n"))))
	(setq i (+ i 1))))))

(defun jde-wiz-import (class) 
  "*Insert an import statement for a class in the current buffer.
CLASS is the fully qualified name of the class to be imported. This
function allows you to enter an import at the head of your buffer
from any point in the buffer. The function does nothing if an import
statement for the specified class alrady exists."
  (interactive
   "sClass: ")
  (jde-wiz-insert-imports (list class)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Method Interface Implementation wizard                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jde-wiz-get-unqualified-name (name)
  (string-match "[^.]+$" name)
  (substring name (match-beginning 0) (match-end 0)))


(defun jde-wiz-update-implements-clause (interface-name)
;   (interactive
;    "sEnter interface: ")
  (let ((interface 
	 (jde-wiz-get-unqualified-name interface-name)))
    (save-excursion
      (let* ((class-re "class[ \t]+\\([a-zA-z]+[a-zA-Z0-9._]*\\).*[ \n]*")
	     (open-brace-pos
	      (scan-lists (point) -1 1))
	     (class-name-end-pos
	      (when open-brace-pos
		(goto-char open-brace-pos)
		(when (re-search-backward class-re (point-min) t)
		  (looking-at class-re)
		  (match-end 1))))
	     (implements-keyword-end-pos
	      (when (and open-brace-pos class-name-end-pos)
		(goto-char open-brace-pos)
		(if (re-search-backward "implements" class-name-end-pos t)
		    (match-end 0)))))
	(if implements-keyword-end-pos
	    (progn
	      (goto-char implements-keyword-end-pos)
	      (insert (concat " " interface ", ")))
	  (when class-name-end-pos
	    (goto-char (- open-brace-pos 1))
	      (insert (concat " implements " interface " "))))))))


(defun jde-wiz-implement-interface (interface-name)
  "*Generate a skeleton implementation of a specified interface."
  (interactive
   "sInterface name: ")
  (condition-case err
      (let* ((nl-brace-p
	      (find 'before 
		    (cdr (assoc 'defun-open c-hanging-braces-alist))))
	     (code
	      (bsh-eval-r
	       (concat
		"jde.wizards.InterfaceFactory.makeInterface(\""
		interface-name "\", true, true, "
		(if nl-brace-p "true" "false") ");"))))
	(if code 
	    (let ((required-imports
		   (bsh-eval-r
		    "jde.wizards.InterfaceFactory.getImportedClasses();")))
	      (insert code)
	      (if required-imports
		  (jde-wiz-insert-imports required-imports))
	      (jde-wiz-update-implements-clause interface-name))))	  
    (error
     (message "%s" (error-message-string err)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Method override wizard                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun jde-wiz-get-method-class ()
  (let ((class-re "class[ \t]+\\([a-zA-z]+[a-zA-Z0-9._]*\\).*[ \n]*"))
    (save-excursion
      (let ((open-brace-pos
	     (scan-lists (point) -1 1)))
	(when open-brace-pos
	  (goto-char open-brace-pos)
	  (when (re-search-backward class-re (point-min) t)
	    (looking-at class-re)
	    (buffer-substring-no-properties
		     (match-beginning 1)
		     (match-end 1))))))))


(defun jde-wiz-override-method (method-name)
  "Overrides a method whose name you specify.
This command creates a skeleton implementation of the
overridden method at point. This command infers the
qualified name of the class of the overriden method by 
prepending the package name of the current buffer to
the class containing point. If the class defines
more than one method of the same name, this command
prompts you to select the desired method from a list
of method prototypes.

This command also generates import statements for 
the parameter and return types of the overridden method.
The import statements are inserted after the last 
existing import statement or the package statement
or the first blank line in the source file. Import
statements are generated only for types for which an
import statement does not already exist in the file.

NOTE: this command works only if the overriding class 
      has been previously compiled."
  (interactive
   "sMethod name: ")
  (condition-case err
      (let* ((package-name (jde-wiz-get-package-name))
	     (class-name (jde-wiz-get-method-class))
	     (qualified-class-name 
	      (if (and package-name class-name)
		  (concat package-name "." class-name)
		class-name)))
	(if qualified-class-name
	    (let ((signatures
		   (bsh-eval
		    (concat 
		     "jde.wizards.MethodOverrideFactory.getCandidateSignatures(\""
		     qualified-class-name "\",\"" method-name "\");") t)))
	      (if signatures
		  (if (> (length signatures) 1)
		      (jde-wiz-override-variant-method signatures)
		    (jde-wiz-override-method-internal (car signatures)  signatures))))))
    (error
     (message "%s" (error-message-string err)))))

(defun jde-wiz-override-method-internal (selected-method methods)
  (let* ((variant
	 (position selected-method methods :test 'string=))
	 (nl-brace-p
	  (find 'before 
		(cdr (assoc 'defun-open c-hanging-braces-alist))))
	 (skeleton
	  (bsh-eval-r
	   (concat
	    "jde.wizards.MethodOverrideFactory.getMethodSkeleton("
	    variant 
	    (if nl-brace-p
		", true"
	      ", false")
	    ");")))
	 (required-imports
	  (bsh-eval-r
	   "jde.wizards.MethodOverrideFactory.getImportedClasses();")))
    (insert skeleton)
    (if required-imports
	(jde-wiz-insert-imports required-imports))))


(defun jde-wiz-override-variant-method (methods) 
  (let ((buf (get-buffer-create "*Choose Method*")))
    (setq jde-wiz-source-buffer (current-buffer))
    (setq jde-wiz-method-variants methods)
    (setq jde-wiz-selected-method (car methods))
    (set-buffer buf)
    (widget-insert "Select the method you want to override.\n")
    (widget-insert "Then click the Ok button.\n\n")
    (let ((args (list
		 'radio-button-choice
		 :value (car methods)
		 :notify (lambda (widget &rest ignore)
			   (setq jde-wiz-selected-method (widget-value widget))
			   (message "You selected: %s"
				    (widget-value widget))))))
	  (setq args (nconc
		      args
		       (mapcar (lambda (x) (list 'item x)) methods)))
	  (apply 'widget-create args)
	  )
    (widget-insert "\n")
    (widget-create 'push-button
		   :notify (lambda (&rest ignore)
			     (let ((dialog-buffer
				    (current-buffer)))
			       (set-buffer jde-wiz-source-buffer)
			       (delete-window)
			       (kill-buffer dialog-buffer)
			       (jde-wiz-override-method-internal
				jde-wiz-selected-method 
				jde-wiz-method-variants)
			       (message "Method inserted.")
			     ))
     		 "Ok")
    (use-local-map widget-keymap)
    (widget-setup)
    (pop-to-buffer buf)))

	  
(provide 'jde-wiz);

;; $Log: jde-wiz.el,v $
;; Revision 1.26  2002/02/27 10:32:24  vltsccm
;; gchiozzi: Forced making for emacs. Problems on RH7.2 with xemacs automatically found.
;;
;; Revision 1.25  2002/02/21 14:59:58  vltsccm
;; emacs1.25
;;
;; Revision 1.24  2002/02/20 07:40:22  vltsccm
;; emacs1.24
;;
;; Revision 1.23  2002/02/09 17:14:08  vltsccm
;; emacs1.23
;;
;; Revision 1.22  2000/07/03 14:04:54  vltsccm
;; emacs1.22
;;
;; Revision 1.21  1999/11/21 21:05:02  vltsccm
;; emacs1.21
;;
;; Revision 1.20.1.1  1999/11/09 02:50:58  vltsccm
;; emacs1.20.1
;;
;; Revision 1.20  1999/06/09 14:56:00  vltsccm
;; emacs1.20
;;
;; Revision 1.19  1999/06/09 14:56:00  vltsccm
;; emacs1.19
;;
;; Revision 1.18  1999/06/09 14:56:00  vltsccm
;; emacs1.18
;;
;; Revision 1.17  1999/06/09 14:55:59  vltsccm
;; emacs1.17
;;
;; Revision 1.16  1999/06/09 14:55:59  vltsccm
;; emacs1.16
;;
;; Revision 1.15  1999/06/09 14:55:59  vltsccm
;; emacs1.15
;;
;; Revision 1.14  1999/06/09 14:55:58  vltsccm
;; emacs1.14
;;
;; Revision 1.13  1999/06/09 14:55:58  vltsccm
;; emacs1.13
;;
;; Revision 1.12  1999/06/09 14:55:58  vltsccm
;; emacs1.12
;;
;; Revision 1.11  1999/06/09 14:55:57  vltsccm
;; emacs1.11
;;
;; Revision 1.10  1999/06/09 14:55:57  vltsccm
;; emacs1.10
;;
;; Revision 1.9  1999/06/09 14:55:57  vltsccm
;; emacs1.9
;;
;; Revision 1.8  1999/06/09 14:55:57  vltsccm
;; emacs1.8
;;
;; Revision 1.7  1999/06/09 14:55:56  vltsccm
;; emacs1.7
;;
;; Revision 1.6  1999/06/09 14:55:55  vltsccm
;; emacs1.6
;;
;; Revision 1.5  1999/06/09 14:55:55  vltsccm
;; emacs1.5
;;
;; Revision 1.4  1999/06/09 14:55:55  vltsccm
;; emacs1.4
;;
;; Revision 1.3  1999/06/09 14:55:55  vltsccm
;; emacs1.3
;;
;; Revision 1.2  1999/06/09 14:55:54  vltsccm
;; emacs1.2
;;
;; Revision 1.10  1999/02/17 19:16:07  paulk
;; Provided more robust error handling for the interface wizard. The wizard
;; no longer kills the bsh when it cannot create an interface and provides
;; meaningfull error messages.
;;
;; Revision 1.9  1999/02/15 01:12:54  paulk
;; Fixed bug in jde-wiz-get-method-class that caused it to fail when the open bracket
;; for the class was not on the same line as the class keyworkd. Thanks to
;; P.Lord@mdx.ac.uk (Phillip Lord) for diagnosing this bug.
;;
;; Revision 1.8  1999/02/12 15:13:00  paulk
;; Added jde-wiz-import function.
;;
;; Revision 1.7  1999/02/11 19:14:50  paulk
;; Fixed bug in jde-wiz-update-implements-clause.
;;
;; Revision 1.6  1999/02/11 18:28:40  paulk
;; Corrected missing parentheses.
;;
;; Revision 1.5  1998/11/22 22:03:43  paulk
;; Fixed bug in interface wizard.
;;
;; Revision 1.4  1998/11/22 21:55:33  paulk
;; Fixed bug in interface wizard.
;;
;; Revision 1.3  1998/11/21 02:41:34  paulk
;; Fixed bug.
;; Added implements clause update function to interface implementation wizard.
;;
;; Revision 1.2  1998/11/10 00:46:39  paulk
;; Added smart import insertion to interface wizard.
;;
;; Revision 1.1  1998/11/08 00:39:24  paulk
;; Initial revision
;;


;; End of jde-wiz.el