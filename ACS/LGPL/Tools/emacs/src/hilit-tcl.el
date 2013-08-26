;;; hilit-tcl.el - Emacs Highlighting for Tcl files (tcl-mode)
;;;
;;; "@(#) $Id: hilit-tcl.el,v 1.26 2002/02/27 10:32:58 vltsccm Exp $"
;;;
;;; who        when       what
;;; -------- ---------- -----------------------------------------
;;; ssandroc ??.??.1994	Created (structure taken from `hilit19.el' c-mode)
;;; ssandroc 29.10.1994	Revised
;;; ssandroc 16.07.1995	Revised
;;; ssandroc 25.04.1996	Revised for incrTcl/Tk 2
;;;

;;(require 'hilit19) ; batch compiler bails out with that!

(let (
      (tcl-comments '(("#.*$" nil comment)
		      (";#.*$" nil comment)))
      (tcl-varsub   '(("\\$" nil string)))
      )

  (hilit-set-mode-patterns
   'tcl-mode
   (append
    tcl-comments tcl-varsub
    '(
      ;; Tcl key words for control-flow
      ("[^_-]\\<\\(if\\|else\\|elseif\\|default\\|switch\\|break\\|continue\\|while\\|do\\|for\\|foreach\\|eval\\|source\\)\\>[^_]" 1 keyword)
      ;; Tcl key words for procedures
      ("[^_-]\\<\\(proc\\|global\\|uplevel\\|upvar\\|return\\)\\>[^_]" 1 defun)
      ;; incrTcl key words for classes
      ("[^_-]\\<\\(class\\|inherit\\|constructor\\|destructor\\|method\\|variable\\|body\\|configbody\\|public\\|protected\\|private\\|common\\|virtual\\|previous\\|namespace\\)\\>[^_]" 1 defun)
      ;; incrTk key words
      ("[^_-]\\<\\(keep\\|ignore\\|rename\\)\\>[^_]" 1 defun)
      ;; Tcl key words for commands
      ("[^_-]\\<\\(set\\|unset\\|format\\|regsub\\|scan\\|string\\|cd\\|close\\|eof\\|file\\|flush\\|gets\\|glob\\|open\\|puts\\|pwd\\|read\\|seek\\|tell\\|exec\\|exit\\|pid\\|catch\\|error\\|rename\\|incr\\)\\>[^_]" 1 type)
      ;; Tk key words for widget options
      ("[^_][-]\\<\\(accelerator\\|command\\|cursor\\|height\\|label\\|menu\\|onvalue\\|offvalue\\|relief\\|text\\|textvariable\\|value\\|variable\\|width\\)\\>[^_]" 1 include)
      ;; Tk key words for packer options
      ("[^_][-]\\<\\(after\\|anchor\\|before\\|expand\\|fill\\|fillx\\|filly\\|in\\|ipadx\\|ipady\\|padx\\|pady\\|side\\|left\\|right\\|top\\|bottom\\)\\>[^_]" 1 include)
      ;; Tk key words for widgets and commands
      ("[^_-]\\<\\(wm\\|pack\\|bind\\|destroy\\|frame\\|label\\|button\\|checkbutton\\|radiobutton\\|listbox\\|scale\\|entry\\|menubutton\\|menu\\)\\>[^_]" 1 define)
;;decl
      ))))

(provide 'hilit-tcl)

;;; hilit-tcl.el ends here
