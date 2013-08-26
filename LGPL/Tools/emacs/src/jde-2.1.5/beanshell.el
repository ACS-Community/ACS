;;; beanshell.el
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

(defun bsh()
"*Starts BeanShell, a Java interpreter developed by Pat Niemeyer."
  (interactive)
  (bsh-internal t))

(defun bsh-internal (&optional display-buffer) 
  (let ((bsh-buffer-name "*bsh*"))
    (if (not (comint-check-proc bsh-buffer-name))
	(let* ((bsh-buffer (get-buffer-create bsh-buffer-name))
	       (jde-java-directory
		(concat
		 (jde-find-jde-data-directory)
		 "java/"))
	       (vm (if (eq system-type 'windows-nt)
		       jde-run-java-vm-w
		     jde-run-java-vm))
	       (vm-args
		(list
		 "-classpath"
		 (concat
		  jde-java-directory "lib/jde.jar" jde-classpath-separator
		  jde-java-directory "lib/bsh.jar" jde-classpath-separator
		  (if jde-global-classpath
		      (jde-run-build-classpath-arg jde-global-classpath)
		    (getenv "CLASSPATH")))
		 "bsh.Interpreter")))
	  (save-excursion
	    (set-buffer bsh-buffer)
	    (erase-buffer)
	    (comint-mode))
	 (save-w32-show-window
	   ;; (message "%s" (nth 1 vm-args))
	   (message "%s" "Starting the BeanShell. Please wait...")
	   (comint-exec bsh-buffer "bsh" vm nil vm-args))
	  (if display-buffer
	      (pop-to-buffer bsh-buffer-name)))
      (when display-buffer
	  (message "The Java interpreter is already running.")
	  (pop-to-buffer bsh-buffer-name)))))

(setq bsh-tq-reply nil)

(defun bsh-eval-filter (process result)
  (let ((end-of-result (string-match ".*bsh % " result)))
    (if end-of-result
	(setq bsh-tq-reply (concat bsh-tq-reply (substring result 0 end-of-result)))
      (setq bsh-tq-reply (concat bsh-tq-reply result))
      (accept-process-output process 5 5))))

(defun bsh-eval (expr &optional eval-return)
  "Uses the BeanShell Java interpreter to evaluate a Java statement.
If the interpreter is not already running, this function starts
the interpreter. This function returns any text output by the
Java interpreter's standard out or standard error pipes.
If the optional argument eval-return is non-nil, this function
returns the result of evaluating the Java output as a Lisp
expression."
  (let* ((bsh-process
	  (if (get-process "bsh")
	      (get-process "bsh")
	    (progn
	      (bsh-internal)
	      (accept-process-output (get-process "bsh"))
	      (get-process "bsh")))) 
	 (comint-filter (process-filter bsh-process)))
    (setq bsh-tq-reply nil)
    (set-process-filter bsh-process 'bsh-eval-filter)
    (process-send-string bsh-process (concat expr "\n"))
    (if (not (accept-process-output bsh-process 100 100))
	(message "No reply from BeanShell"))
    (set-process-filter bsh-process comint-filter)
    (if eval-return
	(eval (read bsh-tq-reply))
      bsh-tq-reply)))

(defun bsh-eval-r(java-statement) 
  "Convenience function for evaluating Java statements
that return Lisp expressions as output. This function 
invokes bsh-eval with the evaluate-return option set to
t."
  (bsh-eval java-statement t))

(provide 'beanshell);

;; $Log: beanshell.el,v $
;; Revision 1.26  2002/02/27 10:32:25  vltsccm
;; gchiozzi: Forced making for emacs. Problems on RH7.2 with xemacs automatically found.
;;
;; Revision 1.25  2002/02/21 14:59:59  vltsccm
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
;; Revision 1.20  1999/06/09 14:56:06  vltsccm
;; emacs1.20
;;
;; Revision 1.19  1999/06/09 14:56:05  vltsccm
;; emacs1.19
;;
;; Revision 1.18  1999/06/09 14:56:05  vltsccm
;; emacs1.18
;;
;; Revision 1.17  1999/06/09 14:56:05  vltsccm
;; emacs1.17
;;
;; Revision 1.16  1999/06/09 14:56:05  vltsccm
;; emacs1.16
;;
;; Revision 1.15  1999/06/09 14:56:04  vltsccm
;; emacs1.15
;;
;; Revision 1.14  1999/06/09 14:56:04  vltsccm
;; emacs1.14
;;
;; Revision 1.13  1999/06/09 14:56:04  vltsccm
;; emacs1.13
;;
;; Revision 1.12  1999/06/09 14:56:04  vltsccm
;; emacs1.12
;;
;; Revision 1.11  1999/06/09 14:56:03  vltsccm
;; emacs1.11
;;
;; Revision 1.10  1999/06/09 14:56:03  vltsccm
;; emacs1.10
;;
;; Revision 1.9  1999/06/09 14:56:03  vltsccm
;; emacs1.9
;;
;; Revision 1.8  1999/06/09 14:56:02  vltsccm
;; emacs1.8
;;
;; Revision 1.7  1999/06/09 14:56:02  vltsccm
;; emacs1.7
;;
;; Revision 1.6  1999/06/09 14:56:01  vltsccm
;; emacs1.6
;;
;; Revision 1.5  1999/06/09 14:56:01  vltsccm
;; emacs1.5
;;
;; Revision 1.4  1999/06/09 14:56:01  vltsccm
;; emacs1.4
;;
;; Revision 1.3  1999/06/09 14:56:01  vltsccm
;; emacs1.3
;;
;; Revision 1.2  1999/06/09 14:56:00  vltsccm
;; emacs1.2
;;
;; Revision 1.7  1999/01/15 22:18:41  paulk
;; Added Andy Piper's NT/XEmacs compatibility changes.
;;
;; Revision 1.6  1998/12/13 22:10:04  paulk
;; Add check for chunked traffic between Emacs and the BeanShell.
;;
;; Revision 1.5  1998/12/09 00:59:43  paulk
;; Added a startup message for beanshell.
;;
;; Revision 1.4  1998/11/27 10:07:57  paulk
;; Use CLASSPATH environment variable if jde-global-classpath is nil.
;;
;; Revision 1.3  1998/11/22 23:14:28  paulk
;; Fixed path separator bug.
;;
;; Revision 1.2  1998/11/22 18:11:56  paulk
;; Changed path to use jde.jar.
;;
;; Revision 1.1  1998/10/22 00:07:56  paulk
;; Initial revision
;;


;; End of bsh.el