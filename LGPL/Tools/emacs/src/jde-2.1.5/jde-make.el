;;; jde-make.el -- Integrated Development Environment for Java.
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

(require 'compile)

(defcustom jde-make-program "make"
  "*Specifies name of make program."
 :group 'jde-project
 :type 'string)

(defcustom jde-make-args ""
  "*Specifies arguments to be passed to make program."
  :group 'jde-project
  :type 'string)

;;;###autoload
(defun jde-make (args)
  "Run the JDE make program."
  (interactive
   (list (if (string= jde-make-args "")
	     (read-from-minibuffer (concat jde-make-program " ")
				   (nth 0 minibuffer-history))
	   jde-make-args)))

  (compile 
   (concat
    jde-make-program
    " "
    (jde-run-make-arg-string
     (jde-run-parse-args args)))))

(provide 'jde-make)

;; $Log: jde-make.el,v $
;; Revision 1.26  2002/02/27 10:32:24  vltsccm
;; gchiozzi: Forced making for emacs. Problems on RH7.2 with xemacs automatically found.
;;
;; Revision 1.25  2002/02/21 14:59:58  vltsccm
;; emacs1.25
;;
;; Revision 1.24  2002/02/20 07:40:21  vltsccm
;; emacs1.24
;;
;; Revision 1.23  2002/02/09 17:14:07  vltsccm
;; emacs1.23
;;
;; Revision 1.22  2000/07/03 14:04:54  vltsccm
;; emacs1.22
;;
;; Revision 1.21  1999/11/21 21:05:02  vltsccm
;; emacs1.21
;;
;; Revision 1.20.1.1  1999/11/09 02:50:57  vltsccm
;; emacs1.20.1
;;
;; Revision 1.20  1999/06/09 14:55:48  vltsccm
;; emacs1.20
;;
;; Revision 1.19  1999/06/09 14:55:48  vltsccm
;; emacs1.19
;;
;; Revision 1.18  1999/06/09 14:55:48  vltsccm
;; emacs1.18
;;
;; Revision 1.17  1999/06/09 14:55:48  vltsccm
;; emacs1.17
;;
;; Revision 1.16  1999/06/09 14:55:47  vltsccm
;; emacs1.16
;;
;; Revision 1.15  1999/06/09 14:55:47  vltsccm
;; emacs1.15
;;
;; Revision 1.14  1999/06/09 14:55:47  vltsccm
;; emacs1.14
;;
;; Revision 1.13  1999/06/09 14:55:46  vltsccm
;; emacs1.13
;;
;; Revision 1.12  1999/06/09 14:55:46  vltsccm
;; emacs1.12
;;
;; Revision 1.11  1999/06/09 14:55:46  vltsccm
;; emacs1.11
;;
;; Revision 1.10  1999/06/09 14:55:46  vltsccm
;; emacs1.10
;;
;; Revision 1.9  1999/06/09 14:55:45  vltsccm
;; emacs1.9
;;
;; Revision 1.8  1999/06/09 14:55:45  vltsccm
;; emacs1.8
;;
;; Revision 1.7  1999/06/09 14:55:45  vltsccm
;; emacs1.7
;;
;; Revision 1.6  1999/06/09 14:55:45  vltsccm
;; emacs1.6
;;
;; Revision 1.5  1999/06/09 14:55:44  vltsccm
;; emacs1.5
;;
;; Revision 1.4  1999/06/09 14:55:44  vltsccm
;; emacs1.4
;;
;; Revision 1.3  1999/06/09 14:55:44  vltsccm
;; emacs1.3
;;
;; Revision 1.2  1999/06/09 14:55:44  vltsccm
;; emacs1.2
;;
;; Revision 1.5  1999/01/17 00:43:57  paulk
;; Removed two line feeds at the end of make command as they appeared to
;; confuse GNU make for NT.
;;
;; Revision 1.4  1998/11/27 09:38:23  paulk
;; Changed to use compile mode as suggested by Robert Grace <rmg2768@draper.com>.
;;
;; Revision 1.3  1998/05/29 01:46:39  paulk
;; Added dummy function for jde-make-mode to facilitate autoloading.
;;
;; Revision 1.2  1998/05/27 06:04:52  paulk
;; Added autoload comments.
;;
;; Revision 1.1  1998/03/27 04:44:36  kinnucan
;; Initial revision
;;

;; End of jde-make.el