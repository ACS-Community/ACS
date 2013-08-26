;
; TAGS browser interface
;
;;; vlt-tags.el --- an interface between etags and the VLT utilities
;
; who	       when     what
; ---------- --------	-----------------------------------------------
; mnastvog   01.03.1996 Added browsing functionality for subroutines
; S.Sandrock 03.10.1994 Created

(if (file-exists-p "../TAGS")
    (visit-tags-table "../TAGS")
)

(setq tags-table-list
      '("../" "~/"))

(defun vlt-tags-rewrite ()
  "Generates new VLT module TAGS file"
  (interactive)
  (message "Generating new ../TAGS file")
  (shell-command 
   "etags -b -f $PWD/../TAGS $PWD/../src/*.C $PWD/../test/*.C $PWD/../TAGS $PWD/../src/*.c $PWD/../test/*.c $PWD/../include/*.h $PWD/../include/*.icc ")
  (if (getenv "INTROOT")
      (shell-command "etags -a -b -f $PWD/../TAGS $INTROOT/include/*.h")
    )
  (if (getenv "VLTROOT")
      (shell-command "etags -a -b -f $PWD/../TAGS $VLTROOT/include/*.h")
    )
  (message "../TAGS file generated")
)

;
; TAGS browsing subroutines
; This establishes a stack of marks.
; With "goto subroutine" the old cursor location will be pushed onto the stack
; With "go up" the latest cursor location will be resumed
;
(defvar stack)
(setq stack nil)

(defun push-marker (marker)
   (setq stack (cons marker stack)))

(defun top-marker ()
   (if (not stack)
      (progn       
        (message "Stack empty")
        (point-marker))
   (car stack)))

(defun pop-marker ()
   (let ((val (top-marker)))
      (if val
        (setq stack (cdr stack)))
      val))

(defun clear-marker-stack()
   (setq stack nil))


(defun vlt-push-bookmark ()
    (interactive)
    (push-marker (point-marker))
)
    


(defun vlt-pop-bookmark ()
    (interactive)
    (let ((bm nil))
       (setq bm (pop-marker))
       (switch-to-buffer (marker-buffer bm))
    (goto-char (marker-position bm))
    )
)
;; Note 
;; here would come the key bindings for browsing
;; this has been moved to the "Emacs" file
;;
;;(define-key global-map [C-up] 'vlt-pop-bookmark)
;;(define-key global-map [C-down] 'vlt-find-tag)










