;;; hscroll.el: Minor mode to automatically scroll truncated lines horizontally
;;; Copyright (C) 1992, 1993 Wayne Mesard
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; The GNU General Public License is available by anonymouse ftp from
;;; prep.ai.mit.edu in pub/gnu/COPYING.  Alternately, you can write to
;;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139,
;;; USA.
;;--------------------------------------------------------------------

;;; DESCRIPTION
;;    N.B. This version works with Emacs19 only!
;;
;;    Automatically scroll horizontally when the point moves off the
;;    left or right edge of the window.  Type "M-x hscroll-mode" to
;;    invoke it in the current buffer.  This only has effect when
;;    the current line is truncated by Emacs.  Say "Control-h f 
;;    hscroll-truncate-lines" for details.
;;
;;    HScroll's sensitivity is controlled by the variable hscroll-margin.
;;    How much HScroll adjusts the window is determined by hscroll-step.
;;
;;    Most users won't have to mess with the other variables and functions 
;;    defined here.  But they're all documented, and they all start with 
;;    "hscroll-" if you're curious.
;;
;;    Oh, you should also know that if you set the hscroll-margin and
;;    hscroll-step-percent large enough, you can get an interesting, but
;;    undesired ping-pong effect as the point bounces from one edge to
;;    the other.
;;
;;    WMesard@cs.stanford.edu

;;; HISTORY
;;    1.5 wmesard - Nov 27, 1993: Rob Riepel changes.
;;    1.4 wmesard - Nov 27, 1993: Cleaned up docstrings.
;;                                Added hscroll-mode-name.
;;    1.3 wmesard - Nov 14, 1993: Added (provide) at end of file.
;;				  screen-width -> frame-width
;;    1.2 wmesard - Nov 11, 1993: Emacs19 version
;;    1.1 wmesard - Aug 18, 1992: Fixed setq-default bug
;;    1.0 wmesard - Aug 11, 1992: Created

;;; 
;;; PUBLIC VARIABLES
;;; 

(defvar hscroll-margin 5 
  "*How many columns away from the edge of the window point is allowed to get
before HScroll will horizontally scroll the window.")

(defvar hscroll-step-percent 25
  "*How far away to place the point from the window's edge when scrolling.
Expressed as a percentage of the window's width.")

(defvar hscroll-mode nil 
  "Whether hscroll-mode is enabled for the current buffer.
Ordinarily set indirectly (via \\[hscroll-mode]).  However, you may want to
use setq-default to have hscroll-mode turned on all the time (say ``\\[describe-function]
hscroll-mode'' for details).  Automatically becomes local when set.")
(make-variable-buffer-local 'hscroll-mode)

(defvar hscroll-mode-name " Hscr"
  "*Horizontal scrolling mode line indicator.
Set this to nil to conserve valuable mode line space.")


;;; 
;;; PUBLIC COMMANDS
;;; 

;;;###autoload
(defun hscroll-mode (&optional onoff)
  "Toggle HScroll mode in the current buffer.
With arg, turn HScroll mode on if arg is positive, off otherwise.
In HScroll mode, truncated lines will automatically scroll left or right
when point gets near either edge of the window.
  The first time \\[hscroll-mode] is called, it enables hscrolling for all 
buffers in which hscroll-mode is non-nil.  So to have truncation and hscrolling 
enabled in all buffers by default, you could add this to your ~/.emacs file:
   (setq-default truncate-lines t)
   (setq-default hscroll-mode t)
   (hscroll-mode)"

  (interactive "P")
  (if (not (member 'hscroll-window-ifactive post-command-hook))
      (progn
	(add-hook 'post-command-hook (function hscroll-window-ifactive))
	(setq minor-mode-alist 
	      (cons '(hscroll-mode hscroll-mode-name)
		    minor-mode-alist))
	;; So that the last line in this func will do the right thing
	;; when default value is t and this is the first buffer.
	(setq hscroll-mode nil)
	))
  (setq hscroll-mode (if onoff
			 (> (if (numberp onoff) onoff
			      (prefix-numeric-value onoff))
			    0)
		       (not hscroll-mode))
	))


(defun hscroll-truncate-lines (&optional onoff)
  "Toggle the value of the Emacs variable truncate-lines in the current buffer.  
With arg, set to t if arg is positive, nil otherwise.  This is just a
convenience function and not really part of HScroll.  Without it, you'd
have to use set-variable to change the value of truncate-lines.

Say \\[describe-variable] truncate-lines and \\[describe-variable] \
truncate-partial-width-windows for details."
  (interactive "P")
  (setq truncate-lines (if onoff
			   (> (if (numberp onoff) onoff 
				(prefix-numeric-value onoff))
			      0)
			 (not truncate-lines))
	))


(defun hscroll-window-maybe ()
  "Scroll horizontally if point is off or nearly off the edge of the window.
This is called automatically when in HScroll mode, but it can be explicitly
invoked as well (i.e., it can be bound to a key)."
  (interactive)
  ;; Only consider scrolling if truncate-lines is true, 
  ;; the window is already scrolled or partial-widths is true and this is
  ;; a partial width window.  See display_text_line() in xdisp.c.
  (if (or truncate-lines
	  (not (zerop (window-hscroll)))
	  (and truncate-partial-width-windows
	       (< (window-width) (frame-width))))
      (let ((linelen (save-excursion (end-of-line) (current-column)))
	    (rightmost-char (+ (window-width) (window-hscroll)))
	    )
	(if (>= (current-column)
		(- rightmost-char hscroll-margin
		   ;; Off-by-one if the left edge is scrolled
		   (if (not (zerop (window-hscroll))) 1 0)
		   ;; Off by one if the right edge is scrolled
		   (if (> linelen rightmost-char) 1 0)
		   ))
	    ;; Scroll to the left a proportion of the window's width.
	    (set-window-hscroll 
	     (selected-window) 
	     (- (+ (current-column) 
		   (/ (* (window-width) hscroll-step-percent) 100))
		(window-width)))
	  (if (< (current-column) (+ (window-hscroll) hscroll-margin))
	      ;; Scroll to the right a proportion of the window's width.
	      (set-window-hscroll
	       (selected-window)
	       (- (current-column) (/ (* (window-width) hscroll-step-percent) 100)))
	    ))
	)))


;;; 
;;; PRIVATE FUNCTIONS
;;; 

(defun hscroll-window-ifactive ()
  ;; Don't even bother if we're not in the mode.
  (if hscroll-mode
      (hscroll-window-maybe)))


;;; 
;;; Keep religious zealots from sending me email.
;;; 

(provide 'hscroll)
