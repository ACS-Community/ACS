;;; w3-about.el --- About pages for emacs-w3
;; Author: wmperry
;; Created: 1998/12/27 01:54:10
;; Version: 1.3
;; Keywords: hypermedia

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1993 - 1996 by William M. Perry <wmperry@cs.indiana.edu>
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

;;;###autoload
(defun w3-about (url)
  ;; Fetch an about page url
  (let* ((data (url-generic-parse-url url))
	 (node (downcase (url-filename data))))
    (if (string= "document" node)
	(w3-document-information)
      (save-excursion
	(set-buffer (get-buffer-create url-working-buffer))
	(erase-buffer)
	(setq url-current-mime-viewer (mm-mime-info "text/html" nil 5)
	      url-current-mime-headers '(("content-type" . "text/html")))
	(cond
	 ((string= "" node)
	  (insert (format
		   "
<html>
  <head>
    <title>Emacs-W3 v%s</title>
    <link rel=\"made\" href=\"mailto:wmperry+w3@cs.indiana.edu\">
    <link rel=\"stylesheet\" href=\"about:style\">
  </head>
  <body>
    <h1>Emacs-W3 &trade;<br>v%s</h1>
    <p align=\"center\">
      Copyright &copy; 1993-1995 William M. Perry<br>
      All rights reserved.
    </p>
    <hr width=\"50%%\">
    <p>
      Welcome to Emacs-w3!  Please see the <a href=\"info:w3#Top\">info
	documentation</a>, or the <a
	href=\"http://www.cs.indiana.edu/elisp/w3/docs.html\">HTML
	version</a> online.
    </p>
    <p>
      Information about the <a href=\"authors\">authors</a>, <a
	href=\"emacs\">versions of emacs</a>, and <a
	href=\"eggs\">easter eggs</a> is also available.
    </p>
    <address>
      Please send any bugs/comments to<br>
      <a href=\"mailto:%s\">%s</a>
    </address>
    <hr>
    <wired><pinhead></wired>
  </body>
</html>
"
		   w3-version-number w3-version-number w3-bug-address w3-bug-address)))
	 ((string= "style" node)
	  (insert
	   "
/* This is the stylesheet for the about pages for Emacs-w3 */

address,h1,h2,h3,h4,h5,h6 { text-align: center }
wired { color: yellow }
wired { background: red }
"))
	 ((string= "license" node)
	  (kill-buffer (current-buffer))
	  (describe-copying))
	 ((string= "warranty" node)
	  (kill-buffer (current-buffer))
	  (describe-no-warranty))
	 ((string= "arena.xpm" node)
	  (insert
	   "/* XPM */
static char *arena[] = {
/* width height num_colors chars_per_pixel */
\"   109    84        3            1\",
/* colors */
\". c #d2c7b1\",
\"# c #dcd1ba\",
\"a c #e6dac2\",
/* pixels */
\"a.a.aa.a##a###aa##a.a.#.aa.....a##a#aa.#a#a###.#.#aa.aa.#aa#aa#.a.a.aa.a##a###aa##a.a.#.aa.....a##a#aa.#a#a##\",
\"#a.a#a##a###...a#aa#aa..aa.aa###aa#.a...a.aa#..a##..aa#a.##aa.a.#a.a#a##a###...a#aa#aa..aa.aa###aa#.a...a.aa#\",
\"a.aaa.aaa#aa#aa#.aa#....aa..###a.#a...#.#.#.....a.#.#aaa#aa..#a#a.aaa.aaa#aa#aa#.aa#....aa..###a.#a...#.#.#..\",
\".#a#.##a..a#aa#.###a.#a..a#.aa.##.#.a#.#..a#.a#a#a.aa#.##a####.#.#a#.##a..a#aa#.###a.#a..a#.aa.##.#.a#.#..a#.\",
\"####.###a##a.#aa#.#..a..##a.a#a##a##..aa##.#..#.....a..##..a.a..####.###a##a.#aa#.#..a..##a.a#a##a##..aa##.#.\",
\"..aa..a#aa#.#a#aaa.a#aa.#....#a#a.###a.#a.a.aaa.###.aaaa#.##aa.#..aa..a#aa#.#a#aaa.a#aa.#....#a#a.###a.#a.a.a\",
\"##a##aa#a#..#a..a.##a..#..##..aaa#a.aaa.#a..###.a.#.#aa#.##.a.a###a##aa#a#..#a..a.##a..#..##..aaa#a.aaa.#a..#\",
\".aaaaaaa#a#.aaa#.a#.aa.#a.a#aa..##aa#.##..#.a..#.a#.#a####aaaaa#.aaaaaaa#a#.aaa#.a#.aa.#a.a#aa..##aa#.##..#.a\",
\".a#.aa#.#a..a#aa###.aa#.a.a#.#.a.##.aaaa#####aaa.a.a#.a..a.aa#...a#.aa#.#a..a#aa###.aa#.a.a#.#.a.##.aaaa#####\",
\"#.#.##.##a.#.a#..#a###a.##aa..a.#a#a.##a#.##...a#.aa.aa#a..aa####.#.##.##a.#.a#..#a###a.##aa..a.#a#a.##a#.##.\",
\".#.##a..a..#a.#.#aa##a.#..#aa#.#a#...#..#.a#a#aa#.#a.aa..##.aaaa.#.##a..a..#a.#.#aa##a.#..#aa#.#a#...#..#.a#a\",
\".#aa...a.a##aaa..#a.aa.#a##...#a##aa#aa#..#...#.##...a.a...aa..a.#aa...a.a##aaa..#a.aa.#a##...#a##aa#aa#..#..\",
\".aaa###.a#..a#.#..aa.a##..##.#a#.a##a#a.#aa#....#..#a.a.a#aa.aa#.aaa###.a#..a#.#..aa.a##..##.#a#.a##a#a.#aa#.\",
\"#.#...aa.#aaaa.#####.##..a..a.##.#aa##aaaaa.aa.aa#.#....##aa.a###.#...aa.#aaaa.#####.##..a..a.##.#aa##aaaaa.a\",
\".a...a.aa.#a#aa#.aa.a.a...#a#..a#a.a.a##.#aaaa.#a##aa.#aa.#a.#.#.a...a.aa.#a#aa#.aa.a.a...#a#..a#a.a.a##.#aaa\",
\"#.a.a#aaaaa#aa#.a#.aaa.#aa#..a##aa#a#.aaa..##.a.aa.aa.a....##a.##.a.a#aaaaa#aa#.a#.aaa.#aa#..a##aa#a#.aaa..##\",
\"..##.aaaa#aa.aa.#a#a#a#.a##.###..##.a#.#aa.a#.a#.###a.###a#.#aa...##.aaaa#aa.aa.#a#a#a#.a##.###..##.a#.#aa.a#\",
\".a.#.aaa######a##.##a#..a#.#.aa.aa#aa...a#.a#.aaaa#a..a.aa#...a#.a.#.aaa######a##.##a#..a#.#.aa.aa#aa...a#.a#\",
\".a#.##a#a#####.##a.#.a#aaa#a.##a.aa##aa##aa.aa##..a###a#a.aaa..a.a#.##a#a#####.##a.#.a#aaa#a.##a.aa##aa##aa.a\",
\"a.aa#.##.a.a#aaa...aa.aaaa.#a#a.a.a.#aa..#a#aa#.aaaa.aa#....#a.#a.aa#.##.a.a#aaa...aa.aaaa.#a#a.a.a.#aa..#a#a\",
\"#.aaa.###aa.#.aaa##aa#.aaaa#.###a#.a.#..aaa#.a.a#.#....a#.#a#a.##.aaa.###aa.#.aaa##aa#.aaaa#.###a#.a.#..aaa#.\",
\"##a.aa..#.a.##.a.a###a##a#....a#...a#a..#a..#.aa..a.#a.#..a..a.a##a.aa..#.a.##.a.a###a##a#....a#...a#a..#a..#\",
\"a#.a#a..#a.####..#aa###.#aa#a..aa.a.aa#aa#a..a#..aaa#a..a#a.aaa.a#.a#a..#a.####..#aa###.#aa#a..aa.a.aa#aa#a..\",
\"#a..aaa.a.#.a##aa##aa#aa.aaa##.###...aa.aa#a.##a##a.a##.#..#.##a#a..aaa.a.#.a##aa##aa#aa.aaa##.###...aa.aa#a.\",
\".a.a.a#a#a#.#a...aaa##...#.#aa#aaa...##.###.a#a.#.#a.a..#aaa.a.a.a.a.a#a#a#.#a...aaa##...#.#aa#aaa...##.###.a\",
\"a.#a#.##...#.aa#####aaa#.###a##..a.a.##..#.#a####.#.###.a....##aa.#a#.##...#.aa#####aaa#.###a##..a.a.##..#.#a\",
\".#a#a.###....##..a#a#.a##..#aaa#a.#aa###.#..a####a..###aaa..#.##.#a#a.###....##..a#a#.a##..#aaa#a.#aa###.#..a\",
\".##aa#a.#..##aa.a.#..###a#.aa.#.##..#.##...#..a.####.#.a.a##aa.#.##aa#a.#..##aa.a.#..###a#.aa.#.##..#.##...#.\",
\".#a#.#a#a#..a#a.a#a.a#.aa#.#aa##aaa##.##...aa#..#.##...##.####aa.#a#.#a#a#..a#a.a#a.a#.aa#.#aa##aaa##.##...aa\",
\"..aaaa.a#a.#.aa.....a.aa...aa#aa.a####aa##a.##a####.aa.#a.a..#.#..aaaa.a#a.#.aa.....a.aa...aa#aa.a####aa##a.#\",
\".#...#..####.#.a.a.aa.#aa...#aa#a.###a#aa..a#.#a.##.#a##.aaaa.##.#...#..####.#.a.a.aa.#aa...#aa#a.###a#aa..a#\",
\"aa##..a.a..a.aa.###aa.##aa.a##a..#.###.a.#a#a#...##a.aaaa#..#.aaaa##..a.a..a.aa.###aa.##aa.a##a..#.###.a.#a#a\",
\"#aaaaa#aa#a##.a#aa...aaa.aaaa..a#.a.##.a.#.a#.a#a###.##a#aa.aaa##aaaaa#aa#a##.a#aa...aaa.aaaa..a#.a.##.a.#.a#\",
\"..a#..#####aa#a.aa.a##.a.a#.#aa#.a.#a.#aa#...a...#.a#.a##a.aa..#..a#..#####aa#a.aa.a##.a.a#.#aa#.a.#a.#aa#...\",
\"#aa#a....#aa####a.#a.#aa#a#..#.a#.aa#.aa##a.#a.#..a#a#..#a.a.aaa#aa#a....#aa####a.#a.#aa#a#..#.a#.aa#.aa##a.#\",
\"#aa...#aa#a.#.#.a.a#a#####a#.a.#a#a##.aa.#aaaa#.aaa..#..a...aa.a#aa...#aa#a.#.#.a.a#a#####a#.a.#a#a##.aa.#aaa\",
\"....#..#a##.#.##aa###aaaaa..a..#.#..##.##..#aa.#.aa#a.a.#aa.a.aa....#..#a##.#.##aa###aaaaa..a..#.#..##.##..#a\",
\"#a#a..#a#aa.a..a...##.a##.#a##.....a.##.aa.aa....aa..###a##..##a#a#a..#a#aa.a..a...##.a##.#a##.....a.##.aa.aa\",
\"aa#aa.a##.#a##a#...#.#a#aa#.#a#aaa.##a.##.aa.#.aaa##.a#a.##a#.a.aa#aa.a##.#a##a#...#.#a#aa#.#a#aaa.##a.##.aa.\",
\"a.##.a##.a#a.###..aa.#..a#.a#.a#.a#a..#a##a.a#..a...aaaaa###....a.##.a##.a#a.###..aa.#..a#.a#.a#.a#a..#a##a.a\",
\"#a......##.##a.aaaa#.##a...###.#.#a.a.aa#.#a..aaa...#.#aaaa###a##a......##.##a.aaaa#.##a...###.#.#a.a.aa#.#a.\",
\".aa#a#..a.#a...a..a##.###...####a..#aa#a..a..a.a#....a##a.a#..a..aa#a#..a.#a...a..a##.###...####a..#aa#a..a..\",
\".#..#.aa#aaaa.aa..a.aa#.#.#..a##.##a##a.aaaaa...#.##a#.aaaa##.a..#..#.aa#aaaa.aa..a.aa#.#.#..a##.##a##a.aaaaa\",
\"aa..#.a##a.a##aaaa#aa#aa#a.#a...a#a##a#.a.#a.a#..a.a..####a.a##.aa..#.a##a.a##aaaa#aa#aa#a.#a...a#a##a#.a.#a.\",
\"a#...##..#.#aa#.aa...#..#.aa#a...###.##a###a#.a#.#.a.#.a.aa#.#a#a#...##..#.#aa#.aa...#..#.aa#a...###.##a###a#\",
\"#a.aa.##a.aa#.a.###.a#a##aaa.aa###.a.#a####...a.a...#.a#a##.###a#a.aa.##a.aa#.a.###.a#a##aaa.aa###.a.#a####..\",
\"a##.a.#.a..a#...###aa###a.a.aaaa.##.#a.#.#.a.##aa.a#.a.###.#aa.#a##.a.#.a..a#...###aa###a.a.aaaa.##.#a.#.#.a.\",
\"#a##..##.#.aa..######.a#aaa#a.aa#a.###a.###.aaa.a.a#a#aaaa##a#aa#a##..##.#.aa..######.a#aaa#a.aa#a.###a.###.a\",
\".a#a#.a###a##.a....##aa..a.a...#a#.a#a..###.aa#.a..a#.a.aa#a.a##.a#a#.a###a##.a....##aa..a.a...#a#.a#a..###.a\",
\"a.a#.#aa#aa#.#aaa.#a.a##.aa..aa#a.a..a..a##.a#aa..#a.aa#a##a.a#.a.a#.#aa#aa#.#aaa.#a.a##.aa..aa#a.a..a..a##.a\",
\"..a##a.##.####a##..a#a.aa.aa.a.aa##.##a##.###a.aaa..#aaa#.#.a#....a##a.##.####a##..a#a.aa.aa.a.aa##.##a##.###\",
\".#..#....a..a#..####.#a##.#a#a.aa.##a.a.a#a.aaa#a#...a#.....a..a.#..#....a..a#..####.#a##.#a#a.aa.##a.a.a#a.a\",
\"..#.#.a...a.#.a#.a#a##.a...#a.#.###a....a#....a#.#.#a.##..#a...#..#.#.a...a.#.a#.a#a##.a...#a.#.###a....a#...\",
\"aa###.aaaa##aa..a#aaa.#a..a.a#.a#a..#..#.##aa#.##.a#.#.aaa#.aa#aaa###.aaaa##aa..a#aaa.#a..a.a#.a#a..#..#.##aa\",
\"#.a#a..#aaa..#aa##aa#..aa...a#####a..###.aa..a.#.#a.#a#.a#a.a#aa#.a#a..#aaa..#aa##aa#..aa...a#####a..###.aa..\",
\"a#a.aa##a..#.a#aa..#a#.#a#..#.#.###a#.#...aaaa##aa.#aaa...###.#aa#a.aa##a..#.a#aa..#a#.#a#..#.#.###a#.#...aaa\",
\"aa.#...##...a#.##.a..a###..#..#.#.a.a#aa.a##.aa.aaa..a.a##.a.#a#aa.#...##...a#.##.a..a###..#..#.#.a.a#aa.a##.\",
\"a###..##aa##.#.####.###..aa.a#..aa.###a#a##a#a#aa.##a.a.a#.#aaa#a###..##aa##.#.####.###..aa.a#..aa.###a#a##a#\",
\"#a#...a.aaa#.#a.a#aa#a#a#a.aa....a#.#..a.##....aa#.a.#a..#.##aaa#a#...a.aaa#.#a.a#aa#a#a#a.aa....a#.#..a.##..\",
\"a.aaa#a######a##.a#aaa.###.##aaa.aa##a#.#a#aaa###a..#a##a..aa.a.a.aaa#a######a##.a#aaa.###.##aaa.aa##a#.#a#aa\",
\"a##...##aaaa##a##..#aa#..##.#.a####a.......#a#a##a###a####a..a.aa##...##aaaa##a##..#aa#..##.#.a####a.......#a\",
\".a.a..#a....#a#...a#..#.a.#.aa.a..##a...#a.#aa...#.a##.aa.#.aa#..a.a..#a....#a#...a#..#.a.#.aa.a..##a...#a.#a\",
\".a..aa#..a.aa#.aaaaa##.a.a.a.a#.a.#..a....#.#a..aa...a...a#aaa#a.a..aa#..a.aa#.aaaaa##.a.a.a.a#.a.#..a....#.#\",
\"##.a.a.a.##...#..#aaa.a.#.a#.#a#.a..#aa.#a##aaaa.aa#.##.aa..aa####.a.a.a.##...#..#aaa.a.#.a#.#a#.a..#aa.#a##a\",
\"a.a.aa.a##a###aa##a.a.#.aa.....a##a#aa.#a#a###.#.#aa.aa.#aa#aa#.a.a.aa.a##a###aa##a.a.#.aa.....a##a#aa.#a#a##\",
\"#a.a#a##a###...a#aa#aa..aa.aa###aa#.a...a.aa#..a##..aa#a.##aa.a.#a.a#a##a###...a#aa#aa..aa.aa###aa#.a...a.aa#\",
\"a.aaa.aaa#aa#aa#.aa#....aa..###a.#a...#.#.#.....a.#.#aaa#aa..#a#a.aaa.aaa#aa#aa#.aa#....aa..###a.#a...#.#.#..\",
\".#a#.##a..a#aa#.###a.#a..a#.aa.##.#.a#.#..a#.a#a#a.aa#.##a####.#.#a#.##a..a#aa#.###a.#a..a#.aa.##.#.a#.#..a#.\",
\"####.###a##a.#aa#.#..a..##a.a#a##a##..aa##.#..#.....a..##..a.a..####.###a##a.#aa#.#..a..##a.a#a##a##..aa##.#.\",
\"..aa..a#aa#.#a#aaa.a#aa.#....#a#a.###a.#a.a.aaa.###.aaaa#.##aa.#..aa..a#aa#.#a#aaa.a#aa.#....#a#a.###a.#a.a.a\",
\"##a##aa#a#..#a..a.##a..#..##..aaa#a.aaa.#a..###.a.#.#aa#.##.a.a###a##aa#a#..#a..a.##a..#..##..aaa#a.aaa.#a..#\",
\".aaaaaaa#a#.aaa#.a#.aa.#a.a#aa..##aa#.##..#.a..#.a#.#a####aaaaa#.aaaaaaa#a#.aaa#.a#.aa.#a.a#aa..##aa#.##..#.a\",
\".a#.aa#.#a..a#aa###.aa#.a.a#.#.a.##.aaaa#####aaa.a.a#.a..a.aa#...a#.aa#.#a..a#aa###.aa#.a.a#.#.a.##.aaaa#####\",
\"#.#.##.##a.#.a#..#a###a.##aa..a.#a#a.##a#.##...a#.aa.aa#a..aa####.#.##.##a.#.a#..#a###a.##aa..a.#a#a.##a#.##.\",
\".#.##a..a..#a.#.#aa##a.#..#aa#.#a#...#..#.a#a#aa#.#a.aa..##.aaaa.#.##a..a..#a.#.#aa##a.#..#aa#.#a#...#..#.a#a\",
\".#aa...a.a##aaa..#a.aa.#a##...#a##aa#aa#..#...#.##...a.a...aa..a.#aa...a.a##aaa..#a.aa.#a##...#a##aa#aa#..#..\",
\".aaa###.a#..a#.#..aa.a##..##.#a#.a##a#a.#aa#....#..#a.a.a#aa.aa#.aaa###.a#..a#.#..aa.a##..##.#a#.a##a#a.#aa#.\",
\"#.#...aa.#aaaa.#####.##..a..a.##.#aa##aaaaa.aa.aa#.#....##aa.a###.#...aa.#aaaa.#####.##..a..a.##.#aa##aaaaa.a\",
\".a...a.aa.#a#aa#.aa.a.a...#a#..a#a.a.a##.#aaaa.#a##aa.#aa.#a.#.#.a...a.aa.#a#aa#.aa.a.a...#a#..a#a.a.a##.#aaa\",
\"#.a.a#aaaaa#aa#.a#.aaa.#aa#..a##aa#a#.aaa..##.a.aa.aa.a....##a.##.a.a#aaaaa#aa#.a#.aaa.#aa#..a##aa#a#.aaa..##\",
\"..##.aaaa#aa.aa.#a#a#a#.a##.###..##.a#.#aa.a#.a#.###a.###a#.#aa...##.aaaa#aa.aa.#a#a#a#.a##.###..##.a#.#aa.a#\",
\".a.#.aaa######a##.##a#..a#.#.aa.aa#aa...a#.a#.aaaa#a..a.aa#...a#.a.#.aaa######a##.##a#..a#.#.aa.aa#aa...a#.a#\",
\".a#.##a#a#####.##a.#.a#aaa#a.##a.aa##aa##aa.aa##..a###a#a.aaa..a.a#.##a#a#####.##a.#.a#aaa#a.##a.aa##aa##aa.a\",
\"a.aa#.##.a.a#aaa...aa.aaaa.#a#a.a.a.#aa..#a#aa#.aaaa.aa#....#a.#a.aa#.##.a.a#aaa...aa.aaaa.#a#a.a.a.#aa..#a#a\"
};
")
	  (setq url-current-mime-headers
		'(("content-type"              . "image/x-pixmap"))
		url-current-mime-type "image/x-pixmap"
		url-current-mime-viewer (mm-mime-info "image/x-pixmap" nil 5)))
	 ((string= "cXVvdGVz" (base64-encode-string node))
	  (insert
	   "
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
")
	  (setq url-current-mime-headers
		'(("content-transfer-encoding" . "base64")
		  ("content-encoding" . "x-gzip"))))
	 ((string= "emlwcHk=" (base64-encode-string node))
	  (insert
	   "
/9j/4AAQSkZJRgABAQAAAQABAAD//gBLCgpDUkVBVE9SOiBYViBWZXJzaW9uIDMuMTBhICBSZXY6
IDEyLzI5Lzk0ICBRdWFsaXR5ID0gNTAsIFNtb290aGluZyA9IDE0Cv/bAEMAEAsMDgwKEA4NDhIR
EBMYKBoYFhYYMSMlHSg6Mz08OTM4N0BIXE5ARFdFNzhQbVFXX2JnaGc+TXF5cGR4XGVnY//bAEMB
ERISGBUYLxoaL2NCOEJjY2NjY2NjY2NjY2NjY2NjY2NjY2NjY2NjY2NjY2NjY2NjY2NjY2NjY2Nj
Y2NjY2NjY//AABEIAJ0AWwMBIgACEQEDEQH/xAAfAAABBQEBAQEBAQAAAAAAAAAAAQIDBAUGBwgJ
Cgv/xAC1EAACAQMDAgQDBQUEBAAAAX0BAgMABBEFEiExQQYTUWEHInEUMoGRoQgjQrHBFVLR8CQz
YnKCCQoWFxgZGiUmJygpKjQ1Njc4OTpDREVGR0hJSlNUVVZXWFlaY2RlZmdoaWpzdHV2d3h5eoOE
hYaHiImKkpOUlZaXmJmaoqOkpaanqKmqsrO0tba3uLm6wsPExcbHyMnK0tPU1dbX2Nna4eLj5OXm
5+jp6vHy8/T19vf4+fr/xAAfAQADAQEBAQEBAQEBAAAAAAAAAQIDBAUGBwgJCgv/xAC1EQACAQIE
BAMEBwUEBAABAncAAQIDEQQFITEGEkFRB2FxEyIygQgUQpGhscEJIzNS8BVictEKFiQ04SXxFxgZ
GiYnKCkqNTY3ODk6Q0RFRkdISUpTVFVWV1hZWmNkZWZnaGlqc3R1dnd4eXqCg4SFhoeIiYqSk5SV
lpeYmZqio6Slpqeoqaqys7S1tre4ubrCw8TFxsfIycrS09TV1tfY2dri4+Tl5ufo6ery8/T19vf4
+fr/2gAMAwEAAhEDEQA/AKF84W+nHA+duewOTUY5A9fpU14o+3zkE/6xu/v6UyIBpUVskE81xStc
9umpJczJI0iaFnZgpBPU+3GOv+fzEHOOhAx371S+2SHkhcjrxTXu5Sv8K4PH+fzrf2UjBYuEW7lw
sMn5uRjHb/PSmiRd2O/+f1rNa7k3cgAgenP+elNN1IzDAUdOOfy/z/Sh0mDxsTWBPc/nSqAUY+Zg
g4CEdemfyrOF7N6g+nGP8/40v2yQjbhccDG3H+f8/iKlImWLg1oXwxxnn8Of1oJP69apfa5ck8dO
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
D/epxqqSuHtY9j//2Q==
")
	  (setq url-current-mime-viewer (mm-mime-info "image/jpeg" nil 5)
		url-current-mime-type "image/gif"
		url-current-mime-headers
		'(("content-transfer-encoding" . "base64")
		  ("content-type" . "image/gif"))))
	 ((string= "emacs" node)
	  (insert
	   "
<html>
  <head>
    <title>Versions of Emacs</title>
    <link rel=\"made\" href=\"mailto:wmperry+w3@cs.indiana.edu\">
    <link rel=\"stylesheet\" href=\"about:style\">
  </head>
  <body>
    <h1>Pointers to versions of emacs</h1>
    <dl>
      <dt> XEmacs
      <dd> An extremely X-aware version of emacs 19 from Sun and UIUC.
	This is the recommended emacsen to run Emacs-W3 on, allowing
	inlined images, inlined mpegs, and variable height fonts,
	among lots of other cool features.  Distribution is at <a
	href=\"ftp://ftp.xemacs.org/pub/xemacs/\">
	ftp://ftp.xemacs.org/pub/xemacs/</a>, or check out the <a
	href=\"http://www.xemacs.org/\">XEmacs web page</a>.
      <dt> Emacs 19
      <dd> A slightly-less X-aware version, direct from the FSF.  This
	is the second choice for most capable version of emacsen for
	emacs-w3, and allows for different fonts (of the same height),
	coloring of links, mouse, and menu support.  Distribution is
	at <a href=\"ftp://ftp.gnu.ai.mit.edu/pub/gnu/\">
	  ftp://ftp.gnu.ai.mit.edu/pub/gnu/</a>.
      <dt> Emacs 19 for NeXTStep
      <dd> A version of Emacs 19 that runs as a native NeXTStep
	application.  Fonts/colors/menus/mouse support.  Distribution
	is at <a
	  href=\"ftp://lynx.ps.uci.edu/pub/NeXT/emacs-19-for-NeXTstep\">
	  ftp://lynx.ps.uci.edu/pub/NeXT/emacs-19-for-NeXTstep</a>.
      <dt> OEmacs for DOS
      <dd> A version of Emacs-19 for MS-DOG.  Distribution at <a
	  href=\"ftp://oak.oakland.edu/pub/msdos/oemacs/\">OEmacs
	  4.1</a>.  Color and mouse support.  NOTE: Emacs-19 now has
	native support for MS-DOS, so this version will likely
	disappear.
      <dt> AmigaDOS
      <dd> A version of emacs 19 are available for AmigaDOS.  Emacs 19
	  is at <a href=\"ftp://ftp.wustl.edu:/pub/aminet/utils/gnu/\">
	  ftp://ftp.wustl.edu:/pub/aminet/utils/gnu/</a> under the
	  filename a2.0bEmacs-bin.LHA.
    </dl>
    <hr width=\"75%\">
    <p><cookie src=\"about:quotes\"></p>
  </body>
</html>
"))
	 ((string= "eggs" node)
	  (insert "
<html>
 <head>
  <title>Emacs-W3 Easter Eggs</title>
  <link rel=\"stylesheet\" href=\"about:style\">
 </head>
 <body>
  <h1>Emacs-W3 Easter Eggs</h1>
  <hr width=\"75%\">
  <p>
   Did you really think it would be that easy?  Read the source, or
experiment! :)
  </p>
  <hr width=\"75%\">
  <cookie href=\"about:quotes\">
 </body>
</html>
"))
	 ((string= "authors" node)
	  (insert "
<html>
 <head>
  <title>The Emacs-W3 Team</title>
  <link rel=\"stylesheet\" href=\"about:style\">
 </head>
 <body>
  <hr width=\"85%\">
  <hr width=\"75%\" label=\" The Emacs-W3 Team \">
  <hr width=\"65%\" label=\" 1993 - 1999 \">
  <hr width=\"55%\">
  <dl>
   <dt> Author
   <dd> William Perry &lt;wmperry+w3@cs.indiana.edu&gt; (Hey, that's <i>me</i>)!
        <br>
        The main author of Emacs-W3.  Currently unsure what to put in here
        about his employer. :)
   <dt> Supporting Cast
   <dd>
    <dl>
     <dt> Chuck Thompson &lt;cthomp@xemacs.org&gt;
     <dd> The master of the XEmacs display code, which makes many of
          the really cool things in Emacs-w3 possible.  I owe him
          copious amounts of beer.
     <dt> Ben Wing &lt;wing@666.com&gt;
     <dd> Another main XEmacs developer, and the man behind the
          PearlSoft port of XEmacs to windows.  I owe him his choice
          of alcoholic beverage as well.
     <dt> Jamie Zawinski &lt;jwz@netscape.com&gt;
     <dd> The main Lucid Emacs developer, now working for netscape on
          Mozilla.  Helped me do the initial integration with Lucid
          Emacs by telling me about stupid mistakes I made. :)
     <dt> Marc Andreesen, Lou Montulli &lt;marca/montulli@netscape.com&gt;
     <dd> The inspiration behind my efforts to create Emacs-W3
          initially, because I hated the interfaces to Mosaic
          and Lynx so much.
    </dl>
  </dl>
  <p><cookie href=\"about:quotes\"></p>
 </body>
</html>
"))

	 ((string= "cGhvdG8=" (base64-encode-string node))
	  (insert "
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
oTW7tBCrAr/JoNOKGGwLByEAADs=
")
	  (setq url-current-mime-viewer (mm-mime-info "image/gif" nil 5)
		url-current-mime-type "image/gif"
		url-current-mime-headers
		'(("content-transfer-encoding" . "base64")
		  ("content-type" . "image/gif"))))
	 ((string= "bW96aWxsYQ==" (base64-encode-string node))
	  (insert
	   "
PGh0bWw+CiAgPGhlYWQ+CiAgICA8dGl0bGU+VGhlIEJvb2sgb2YgTW96aWxsYSwgMTg6Mjc8L3Rp
dGxlPgogIDwvaGVhZD4KICA8Ym9keSBiZ2NvbG9yPSIjODAwMDAwIiB0ZXh0PSIjRkZGRkZGIj4K
ICAgIDxociBhbGlnbj1jZW50ZXIgd2lkdGg9Ijc1JSI+CiAgICA8cD4KICAgICBBbmQgSSBzYXcg
YW4gU0dNTCBwdXJpc3QgY29tZSBkb3duIGZyb20gTUlULCBoYXZpbmcgdGhlIGtleSBvZgogICAg
IHRoZSBib3R0b21sZXNzIHBpdCBhbmQgYSBncmVhdCBjaGFpbiBpbiBoaXMgaGFuZC4KICAgIDwv
cD4KICAgIDxwPgogICAgIEFuZCBoZSBsYWlkIGhvbGQgb24gTW96aWxsYSwgdGhhdCBvbGQgc2Vy
cGVudCwgd2hpY2ggaXMgdGhlCiAgICAgPHNlY3JldD5NYXJjQTwvc2VjcmV0PiwgYW5kIHRoZSA8
c2VjcmV0PkppbUM8L3NlY3JldD4sIGFuZAogICAgIGJvdW5kIGhpbSBhIHRob3VzYW5kIHllYXJz
LgogICAgPC9wPgogICAgPHA+CiAgICAgQW5kIGNhc3QgaGltIGludG8gdGhlIGJvdHRvbWxlc3Mg
cGl0LCBhbmQgc2h1dCBoaW0gdXAsIGFuZCBzZXQgYQogICAgIHNlYWwgdXBvbiBoaW0sIHRoYXQg
aGUgc2hvdWxkIGRlY2VpdmUgdGhlIG5ldCBubyBtb3JlLCB0aWxsIHRoZQogICAgIHRob3VzYW5k
IHllYXJzIHNob3VsZCBiZSBmdWxmaWxsZWQ6ICBhbmQgYWZ0ZXIgdGhhdCBoZSBtdXN0IGJlCiAg
ICAgbG9vc2VkIGEgbGl0dGxlIHNlYXNvbi4KICAgIDwvcD4KICAgIDxociB3aWR0aD0iNzUlIj4K
ICAgIDxiPjxociB3aWR0aD0iODUlIiBsYWJlbD0iIGZyb20gYGBUaGUgQm9vayBvZiBNb3ppbGxh
JycsIDE4OjI3ICI+PC9iPgogICAgPGhyIHdpZHRoPSI3NSUiPgogIDwvYm9keT4KPC9odG1sPgo=
")
	  (setq url-current-mime-headers
		'(("content-transfer-encoding" . "base64"))))
	 (t
	  (insert "
<html>
 <head>
  
 </head>
 <body>
  <p>
    I don't know what you are talking about.  What about " node "?
  </p>
  <hr width=\"75%\">
  <hype>
 </body>
</html>
")))))))

(provide 'w3-about)
