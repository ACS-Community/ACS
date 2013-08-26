;
; cmm interface
;
(require 'compile)



(defun vlt-cmm-archive (mod reason)
  "Interface to cmmArchive"
  (interactive "scmmArchive - Module name: \nsReason: ")
  (let* ((module (if (string-equal mod "")
		     "undefined"
		   mod))))
  (compile-internal (concat "cmmArchive " mod " \"" reason "\"")
	    "VLT-CMM Command failed" "VLT-CMM" nil "Error")
)


(defun vlt-cmm-cancel (mod)
  "Interface to cmmCancel"
  (interactive "scmmCancel - YOUR WORK WILL BE LOST!! - Module name: ")
  (let* ((module (if (string-equal mod "")
		     "undefined"
		   mod))))
  (compile-internal (concat "cmmCancel " mod)
	    "VLT-CMM Command failed" "VLT-CMM" nil "Error")
)


(defun vlt-cmm-check (mod)
  "Interface to cmmCheck"
  (interactive "scmmCheck - Module name: ")
  (let* ((module (if (string-equal mod "")
		     "undefined"
		   mod))))
  (compile-internal (concat "cmmCheckForArchive " mod)
	    "VLT-CMM Command failed" "VLT-CMM" nil "Error")
)


(defun vlt-cmm-compare (mod nv ov)
  "Interface to cmmCompare"
  (interactive "scmmCompare - Module name: \nsNew version: \nsOld version: ")
  (let* ((module (if (string-equal mod "")
		     "undefined"
		   mod))))
  (compile-internal (concat "cmmCompare " mod " " nv " " ov)
	    "VLT-CMM Command failed" "VLT-CMM" nil "Error")
)


(defun vlt-cmm-copy (mod)
  "Interface to cmmCopy"
  (interactive "scmmCopy - Module name: ")
  (let* ((module (if (string-equal mod "")
		     "undefined"
		   mod))))
  (compile-internal (concat "cmmCopy " mod)
	    "VLT-CMM Command failed" "VLT-CMM" nil "Error")
)


(defun vlt-cmm-history (mod)
  "Interface to cmmHistory"
  (interactive "scmmHistory - Module name: ")
  (let* ((module (if (string-equal mod "")
		     "undefined"
		   mod))))
  (compile-internal (concat "cmmHistory " mod)
	    "VLT-CMM Command failed" "VLT-CMM" nil "Error")
)


(defun vlt-cmm-modify (mod)
  "Interface to cmmModify"
  (interactive "scmmModify - Module name: ")
  (let* ((module (if (string-equal mod "")
		     "undefined"
		   mod))))
  (compile-internal (concat "cmmModify " mod)
	    "VLT-CMM Command failed" "VLT-CMM" nil "Error")
)



(defun vlt-cmm-what (mod)
  "Interface to cmmWhat"
  (interactive "scmmWhat - Module name: ")
  (let* ((module (if (string-equal mod "")
		     "undefined"
		   mod))))
  (compile-internal (concat "cmmWhat " mod)
	    "VLT-CMM Command failed" "VLT-CMM" nil "Error")
)


(defun vlt-cmm-who ()
  "Interface to cmmWho"
  (interactive)
  (compile-internal (concat "cmmWho ")
	    "VLT-CMM Command failed" "VLT-CMM" nil "Error")
)










