;;;w3-imenu.el,v 1.1.1.1 1998/12/01 22:11:57 wmperry Exp
;;;Description: Build up navigation index for W3 documents:
(require 'cl)
(require 'imenu)
;;{{{ Tags to index

(defvar w3-imenu-index-html-elements
  (list 'h1 'h2 'h3)
  "*List of HTML tags whose buffer positions in the W3 presentation
should appear in the index")

(make-variable-buffer-local 'w3-imenu-index-html-elements)
;;}}}
;;{{{ helpers 

(defsubst w3-html-stack () (get-text-property (point) 'html-stack))

(defsubst w3-html-stack-top-element (stack)
  (first (first stack )))

;;}}}
;;{{{  Move to an element position

(defun w3-imenu-goto-next-element (element)
  "Move forward in the W3 buffer to point where
the next occurrence of element element starts.
Return nil and leave point at end of buffer  if not found."
  (let ((position nil)
        (found nil)
        (stack (w3-html-stack)))
    (while  (and (not (eobp))
                 (not found))
      (setq found
            (or (eq (w3-html-stack-top-element stack)  element)
                (and (eq (w3-html-stack-top-element stack) 'a)
                     (eq (first (second stack)) element))))
      (setq position  (point))
      (goto-char
       (next-single-property-change  (point)  'html-stack
                                     (current-buffer) (point-max)))
      (setq stack (w3-html-stack)))
    (if found position nil)))

;;}}}
;;{{{  create an index 

(defun w3-imenu-create-index ()
  "Returns an alist suitable for use by imenu"
  (declare (special w3-imenu-index-html-elements))
  (let ((index nil)
        (position nil)
        (marker nil))
    (save-excursion
      (loop for element in w3-imenu-index-html-elements
            do 
            (goto-char (point-min))
            (while (setq position
                         (w3-imenu-goto-next-element element))
              (setq marker (make-marker))
              (set-marker marker position)
            (push
             (cons
              (buffer-substring-no-properties position (point))
              marker)
             index))))
    index))

;;}}}
;;{{{ Tell W3 to start using it:
(declaim (special imenu-create-index-function))
(add-hook
 'w3-mode-hook
 (function
  (lambda ()
    (setq imenu-create-index-function 'w3-imenu-create-index)
    (define-key w3-mode-map "j" 'imenu))))

;;}}}
(provide 'w3-imenu)
;;{{{ end of file 

;;; local variables:
;;; folded-file: t
;;; end: 

;;}}}
