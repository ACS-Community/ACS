;;; hilit-change-log.el - Emacs Highlighting for ChangeLog files
;;;
;;; "@(#) $Id: hilit-change-log.el,v 1.26 2002/02/27 10:32:55 vltsccm Exp $"
;;;
;;; who       when       what
;;; -------- ---------- -----------------------------------------
;;; ssandroc 29.10.1994	Created (structure taken from `hilit19.el' c-mode)
;;; ssandroc 16.07.1995 Revised, added `sub-header'.
;;;

;;(require 'hilit19) ; batch compiler bails out with that!

(let (
      (header     '(("^[MTWFS].*$" nil comment)))	; daily header
      (sub-header '(("^\t[^*(].*:[ \t]*$" nil string)))	; group-header
      (main-entry '(("^\t\\*[^,(:]*" nil define)))	; file-name
      (revision   '(("^\t\\*[^(:]*" nil include)))	; RCS revision
      (sub-entry  '(("^\t[(*].*\)" nil defun)))		; function-name
      (item       '(("`" "'" type)))			; word enclosed in `'
      )

  (hilit-set-mode-patterns
   '(change-log-mode)
   (append header sub-header main-entry revision sub-entry item)))

(provide 'hilit-change-log)

;;; hilit-change-log.el ends here
