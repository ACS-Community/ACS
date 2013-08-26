;;; w3-keyword.el --- Emacs-W3 binding style sheet mechanism
;; Author: wmperry
;; Created: 1999/10/01 20:22:11
;; Version: 1.3
;; Keywords: hypermedia

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1996 by William M. Perry <wmperry@cs.indiana.edu>
;;; Copyright (c) 1996 - 1999 Free Software Foundation, Inc.
;;;
;;; This file is part of GNU Emacs.
;;;
;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA 02111-1307, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Lots of generic keywords for use by Emacs-W3
;;;
;;; This is in a separate file just for sanity's sake - I cannot rely on
;;; keywords being automatically recognized (ala XEmacs), and doing a
;;; defconst doesn't work either, because the byte-compiler gets too
;;; smart for us, and the .elc files are no longer portable.  Joy oh joy!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((keywords '(
		  :align
		  :average-pitch
		  :link-title
		  :background
		  :center
		  :data
		  :depth
		  :left-volume
		  :right-volume
		  :pitch-range
		  :stress
		  :richness
		  :figalt
		  :figdata
		  :fillcol
		  :form
		  :formnum
		  :gain
		  :header-start
		  :help-echo
		  :href
		  :link-args
		  :image
		  :lists
		  :map
		  :name
		  :needspace
		  :next-break
		  :nofill
		  :nowrap
		  :optarg
		  :options
		  :pre-start
		  :select
		  :secret
		  :table
		  :text-mangler
		  :title
		  :w3-graphic
		  :w3-form-data
		  :zone
		  :label-text
		  :seen-this-url

		  ;; These are duplicated from the font.el code
		  ;; so that we can share .elc files...

		  :family
		  :weight
		  :extra-light
		  :light
		  :demi-light
		  :medium
		  :normal
		  :demi-bold
		  :bold
		  :extra-bold
		  :style
		  :size
		  :registry
		  :encoding

		  ;; These are duplicated from the widget code
		  ;; so that we can share .elc files
		  :from
		  :action
		  :to
		  :group
		  :args
		  :tag
		  :tab-order
		  :notify
		  :button-keymap
		  :ignore-case
		  :parent
		  :type
		  :src
		  :button-extent
		  :active-face
		  :visited-face
		  :emacspeak-help
		  :target
  )))
  (while keywords
    (or (boundp (car keywords))
	(set (car keywords) (car keywords)))
    (setq keywords (cdr keywords))))

(provide 'w3-keyword)
