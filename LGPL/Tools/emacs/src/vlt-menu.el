;;; vlt-menu.el --- a menu-interface to the VLT utilities

;;; Author: S.Sandrock <ssandroc@eso.org>
;;; Created: 03.10.1994
;;; Version: 1.0

; who	       when     what
; ---------- --------	-----------------------------------------------
; ssandrock  1999-11-08 header date now in ISO-format yyyy-mm-dd
; G.Chiozzi  14.03.1996 Added new shortcuts for navigation in module 
;                       structure
; S.Sandrock 19.10.1994 Added SPR-functions.
;            21.10.1994 Changed entry `RCS Toggle Lock'
;            27.02.1995 Added new usefull menus. Deleted RCS
; G.Chiozzi  28.03.1995 Added new usefull menus

(defun vlt-menu-setup ()
  "Set up menu for VLT."

  (defvar vlt-menu (make-sparse-keymap "VLT")
    "Keymap for VLT menu.")
  (define-key global-map [menu-bar vlt] (cons "VLT" vlt-menu))

; Add additional menu entries here (reverse order).
; The format is:
;  (define-key vlt-menu [ANY_MARKER]
;    '("ITEM_STRING" . COMMAND))
; Template:
;  (define-key vlt-menu [vlt-]
;    '("" .  ))

;;
;; VLT Sub-menus - must be defined first
;;

; Submenu for SPR Tools
  (defvar vlt-spr (make-sparse-keymap "SPR Tools")
    "Keymap for VLT SPR Tools menu.")
  (define-key vlt-spr [vlt-spr-submit]
    '("SPR Submit..." . vlt-spr-submit ))
  (define-key vlt-spr [vlt-spr-browse]
    '("SPR Browse..." . vlt-spr-browse ))
  (define-key vlt-spr [vlt-spr-add-remark]
    '("SPR Add Remark..." . vlt-spr-add-remark ))

; Submenu for Templates
  (defvar vlt-templates (make-sparse-keymap "Insert template")
    "Keymap for Insert of VLT templates .")
  (define-key vlt-templates [vlt-ins-tcl-header]
    '("TCL script header" . vlt-ins-tcl-header ))
  (define-key vlt-templates [vlt-ins-sh-header]
    '("script header" . vlt-ins-sh-header ))
  (define-key vlt-templates [vlt-ins-c++-small-main]
    '("C++ small main (no VLT includes)" . vlt-ins-c++-small-main ))
  (define-key vlt-templates [vlt-ins-dbl-header]
    '("dbl header" . vlt-ins-dbl-header ))
  (define-key vlt-templates [vlt-ins-class-header]
    '("class header" . vlt-ins-class-header ))
  (define-key vlt-templates [vlt-ins-function-header]
    '("function header" . vlt-ins-function-header ))
  (define-key vlt-templates [vlt-ins-h-header]
    '("h header" . vlt-ins-h-header ))
  (define-key vlt-templates [vlt-ins-c-header]
    '("c header" . vlt-ins-c-header ))
  (define-key vlt-templates [vlt-ins-c++-h-header]
    '("C++ h header" . vlt-ins-c++-h-header ))
  (define-key vlt-templates [vlt-ins-c++-header]
    '("C++ header" . vlt-ins-c++-header ))

; Submenu for CMM
  (defvar vlt-cmm (make-sparse-keymap "CMM Tools")
    "Keymap for VLT CMM Tools menu.")

  (define-key vlt-cmm [vlt-cmm-who]
    '("cmm Who..." .  vlt-cmm-who ))
  (define-key vlt-cmm [vlt-cmm-what]
    '("cmm What..." .  vlt-cmm-what ))
  (define-key vlt-cmm [vlt-cmm-modify]
    '("cmm Modify..." .  vlt-cmm-modify ))
  (define-key vlt-cmm [vlt-cmm-history]
    '("cmm History..." .  vlt-cmm-history ))
  (define-key vlt-cmm [vlt-cmm-copy]
    '("cmm Copy..." .  vlt-cmm-copy ))
  (define-key vlt-cmm [vlt-cmm-compare]
    '("cmm Compare..." .  vlt-cmm-compare ))
  (define-key vlt-cmm [vlt-cmm-check]
    '("cmm Check..." .  vlt-cmm-check ))
  (define-key vlt-cmm [vlt-cmm-cancel]
    '("cmm Cancel..." .  vlt-cmm-cancel ))
  (define-key vlt-cmm [vlt-cmm-archive]
    '("cmm Archive..." .  vlt-cmm-archive ))

; Submenu for TAGS code browser
  (defvar vlt-tags (make-sparse-keymap "TAGS Tools")
    "Keymap for VLT TAGS Tools menu.")

  (define-key vlt-tags [find-class]
    '("Find class" .  find-class ))
  (define-key vlt-tags [find-instance]
    '("Find instance" .  find-instance ))
  (define-key vlt-tags [find-tag]
    '("Find TAG" .  find-tag ))
  (define-key vlt-tags [visit-tags-table]
    '("Visit a TAGS table" .  visit-tags-table ))
  (define-key vlt-tags [vlt-tags-rewrite]
    '("Generates new TAGS" .  vlt-tags-rewrite ))

; Submenu for VLT utilities
  (defvar vlt-tools (make-sparse-keymap "VLT Tools")
    "Keymap for VLT Tools menu.")

  (define-key vlt-tools [vlt-panel]
    '("Panel editor" .  vlt-panel ))
  (define-key vlt-tools [vlt-logmon]
    '("Log monitor" .  vlt-logmon ))
  (define-key vlt-tools [vlt-erredit]
    '("Error editor" .  vlt-erredit ))
  (define-key vlt-tools [vlt-lccei]
    '("lccei" .  vlt-lccei ))
  (define-key vlt-tools [vlt-ccsei]
    '("ccsei" .  vlt-ccsei ))

;;
;; Main VLT menu definition
;;

; VLT Browser Tools are in a sub-menu
  (define-key vlt-menu [vlt-tags] (cons "Emacs code browser" vlt-tags)) 

; VLT Templates to be inserted are in a sub-menu
  (define-key vlt-menu [vlt-templates] (cons "Ins. template" vlt-templates)) 

; VLT SPR Tools are in a sub-menu
  (define-key vlt-menu [vlt-spr] (cons "SPR Tools" vlt-spr)) 

; VLT Tools are in a sub-menu
  (define-key vlt-menu [vlt-tools] (cons "Develop. tools" vlt-tools)) 

; VLT CMM Tools are in a sub-menu
  (define-key vlt-menu [vlt-cmm] (cons "CMM Tools" vlt-cmm)) 

  (define-key vlt-menu [vlt-mod]
    '("VLT mod..." . vlt-mod ))
  (define-key vlt-menu [vlt-man]
    '("VLT man..." . vlt-man ))
  (define-key vlt-menu [vlt-get-template]
    '("VLT getTemplate..." . vlt-get-template ))
  (define-key vlt-menu [vlt-doc]
    '("VLT doc..." . vlt-doc ))
  (define-key vlt-menu [vlt-act]
    '("VLT Activity..." . vlt-act ))

  (define-key vlt-menu [vlt-compile]
    '("Run Make..." . compile ))
  (define-key vlt-menu [vlt-grep]
    '("Run Grep..." . grep ))
  (define-key vlt-menu [vlt-gdb]
    '("Run Gdb..." . gdb ))
  (define-key vlt-menu [vlt-command]
    '("Run any command..." . vlt-command ))
  (define-key vlt-menu [vlt-xcoral]
    '("Run C++ Browser..." . vlt-xcoral ))

  (define-key vlt-menu [vlt-speedbar]
    '("Speedbar browser" . speedbar ))
  (define-key vlt-menu [vlt-add-change-log-entry]
    '("Add ChangeLog Entry" . add-change-log-entry ))
  (define-key vlt-menu [vlt-file-add-log-entry]
    '("Add File Log Entry" . vlt-file-add-log-entry ))
  (define-key vlt-menu [ps-print-buffer-with-faces]
    '("Print Buffer (ps)" . ps-print-buffer-with-faces ))
  (define-key vlt-menu [vlt-cycle-buffer]
    '("Cycle Buffers" . cycle-buffer ))
  (define-key vlt-menu [vlt-hilit-repaint-command]
    '("Re-Hilite" .  hilit-repaint-command))

)

;;
;; Other shortcuts
;;
(define-key global-map "\C-x\M-s" 'vlt-dired-src     )
(define-key global-map "\C-x\M-t" 'vlt-dired-test    ) 
(define-key global-map "\C-x\M-d" 'vlt-dired-dbl     )  
(define-key global-map "\C-x\M-i" 'vlt-dired-include )

(defun vlt-command (cmd)
  "Run any command in a window"
  (interactive "sCommand to be executed: ")
  (let* ((module (if (string-equal cmd "")
		     "undefined"
		   cmd))))
  (compile-internal cmd
	    "Command failed" "Command" nil "Error")
)


(defun vlt-get-template ()
  "Interface to VLT getTemplate"
  (interactive)
  (vlt-shell-command "unsetenv EDITOR; getTemplate") ; would invoke Emacs again
)

(defun vlt-act ()
  "Interface to VLT wpActivity Planning tool"
  (interactive)
  (vlt-shell-command "wpActivity")
)

(defun vlt-ccsei ()
  "Interface to CCS Engineering Interface"
  (interactive)
  (vlt-shell-command "ccsei")
)

(defun vlt-doc ()
  "Interface to VLT doc"
  (interactive)
  (vlt-shell-command "doc")
)

(defun vlt-erredit ()
  "Interface to VLT Error Editor"
  (interactive)
  (vlt-shell-command "errEditor")
)

(defun vlt-lccei ()
  "Interface to LCC Engineering Interface"
  (interactive)
  (vlt-shell-command "lccei")
)

(defun vlt-logmon ()
  "Interface to VLT Log Monitor"
  (interactive)
  (vlt-shell-command "logMonitor")
)

(defun vlt-mod ()
  "Interface to VLT mod"
  (interactive)
  (vlt-shell-command "mod")
)

(defun vlt-man ()
  "Interface to vltMan"
  (interactive)
  (vlt-shell-command "vltMan &") ; doesn't work without '&', don't know why.
)

(defun vlt-panel ()
  "Interface to VLT Panel Editor"
  (interactive)
  (vlt-shell-command "panel")
)

(defun vlt-spr-submit ()
  "Interface to sprSubmit"
  (interactive)
  (vlt-shell-command "sprSubmit")
)

(defun vlt-spr-browse ()
  "Interface to sprBrowse"
  (interactive)
  (vlt-shell-command "sprBrowse")
)

(defun vlt-spr-add-remark ()
  "Interface to sprAddRemark"
  (interactive)
  (vlt-shell-command "sprAddRemark")
)

(defun vlt-xcoral ()
  "Interface to xcoral"
  (interactive)
  (vlt-shell-command "xcoral")
)


(defun vlt-shell-command (command)
  "Starts an asynchronous process for a VLT-command under the current shell"
  (setq proc (start-process "VLT" nil shell-file-name "-c" command))
)


;;
;; Insert template functions
;;

;; Standard templates
(defun vlt-ins-c-header ()
  "Interface to "
  (interactive)
  (vlt-ins-any-template ( substitute-in-file-name 
			  "$VLTROOT/templates/forCoding/c-main.template"))
)
(defun vlt-ins-h-header ()
  "Interface to "
  (interactive)
  (vlt-ins-any-template ( substitute-in-file-name 
			  "$VLTROOT/templates/forCoding/h-file.template"))
)
(defun vlt-ins-function-header ()
  "Interface to "
  (interactive)
  (vlt-ins-any-template ( substitute-in-file-name 
			  "$VLTROOT/templates/forCoding/c-procedure.template"))
)
(defun vlt-ins-sh-header ()
  "Interface to "
  (interactive)
  (vlt-ins-any-template ( substitute-in-file-name 
			  "$VLTROOT/templates/forCoding/script.template"))
)
(defun vlt-ins-tcl-header ()
  "Interface to "
  (interactive)
  (vlt-ins-any-template ( substitute-in-file-name 
			  "$VLTROOT/templates/forCoding/tclScript.template"))
)


;; New templates
(defun vlt-ins-class-header ()
  "Interface to "
  (interactive)
  (vlt-ins-any-template ( substitute-in-file-name 
			  "$VLTROOT/templates/forCoding/c++-class-file.template"))
)

(defun vlt-ins-c++-h-header ()
  "Interface to "
  (interactive)
  (vlt-ins-any-template ( substitute-in-file-name
			  "$VLTROOT/templates/forCoding/c++-h-file.template"))
)

(defun vlt-ins-c++-header ()
  "Interface to "
  (interactive)
  (vlt-ins-any-template ( substitute-in-file-name 
			  "$VLTROOT/templates/forCoding/c++-file.template"))

)
(defun vlt-ins-c++-small-main ()
  "Interface to "
  (interactive)
  (vlt-ins-any-template ( substitute-in-file-name 
			  "$VLTROOT/templates/forCoding/c++-small-main.template"))
)
(defun vlt-ins-dbl-header ()
  "Interface to "
  (interactive)
  (vlt-ins-any-template ( substitute-in-file-name 
			  "$VLTROOT/templates/forCoding/dbl-file.template"))
)

(defun vlt-ins-any-template(temp_name)
  "Generic function to insert and clean a template"
  (interactive)
  (insert-file-contents temp_name nil)
  (flush-lines "^#%#.*")
  (while (search-forward ">-<" nil t)
    (replace-match "" nil t)
    )
  (goto-char (point-min))
  (if (search-forward "NNNNNNNN" nil t)
      (progn  (replace-match "" nil t) 
	      (call-process "whoami" nil t)
	      (delete-backward-char 1)
	)
    )
  (if (search-forward "dd/mm/yy" nil t)
      (progn  (replace-match "" nil t) 
	      (delete-backward-char 1)
	      (call-process "date" nil t nil "+%Y-%m-%d")
	      ;;(delete-backward-char 1)
	      (forward-char 1)
	      (delete-backward-char 2)
	)
    )
  (if (search-forward "yyyy-mm-dd" nil t)
      (progn  (replace-match "" nil t) 
	      (call-process "date" nil t nil "+%Y-%m-%d")
	      (delete-backward-char 1)
	      (forward-char 1)
	)
    )
)

;;
;; Functions for other shortcuts
;;

(defun vlt-dired-src ()
  "Load ../src module's directory"
  (interactive)
  (dired "../src")
)

(defun vlt-dired-test ()
  "Load ../test module's directory"
  (interactive)
  (dired "../test")
)

(defun vlt-dired-dbl ()
  "Load ../dbl module's directory"
  (interactive)
  (dired "../dbl")
)

(defun vlt-dired-include ()
  "Load ../include module's directory"
  (interactive)
  (dired "../include")
)


(load "vlt-cmm")  ; Load vlt-cmm functions
(load "vlt-tags") ; Load vlt-tags functions for code browser

(vlt-menu-setup) ; Install the 'VLT' menu in the menu-bar

(provide 'vlt-menu)

;;; vlt-menu.el ends here








