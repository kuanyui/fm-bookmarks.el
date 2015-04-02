;;; fm-bookmark.el --- Access existed FM bookmark in Dired  -*- lexical-binding: t; -*-

;; Author: hiroko <azazabc123@gmail.com>
;; Keywords: files, convenience

;; The MIT License (MIT)
;; Copyright (C) 2015  hiroko
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

;;; Commentary:

;; Use existed bookmarks of file managers (e.g. Dolphin, Nautilus,
;; PCManFM) in Dired.

;;; Code:

(require 'xml)
(require 'cl)
(require 'dired)

;; ======================================================
;; Major Mode
;; ======================================================

(defgroup fm-bookmark nil
  "Access existed FM bookmark in Dired"
  :prefix "fm-bookmark-"
  :link '(url-link "http://github.com/kuanyui/fm-bookmark.el"))

(defgroup fm-bookmark-faces nil
  "Faces used in fm-bookmark"
  :group 'fm-bookmark
  :group 'faces)

(defcustom fm-bookmark-mode-hook nil
  "Normal hook run when entering fm-bookmark-mode."
  :type 'hook
  :group 'fm-bookmark)

(defvar fm-bookmark-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Element insertion
    (define-key map (kbd "q") '(lambda ()
				 (interactive)
				 (delete-window (selected-window))
				 ))
    (define-key map (kbd "h") 'describe-mode)
    (define-key map (kbd "RET") 'fm-bookmark-open-this)
    map)
  "Keymap for Moedict major mode.")   ;document

(define-derived-mode fm-bookmark-mode nil "FM Bookmarks"
  "Major mode for looking up Chinese vocabulary via Moedict API."
  (set (make-local-variable 'buffer-read-only) t)
  (hl-line-mode t)
  )

;; ======================================================
;; Variables
;; ======================================================

(defvar fm-bookmark-buffer-name "*FM Bookmarks*"
  "Name of the buffer.")

(defvar fm-bookmark-buffer-width 25
  "Width of buffer"
  )

(defvar fm-bookmark-enabled-file-managers '(kde4 gnome3 pcmanfm)
  "Enabled file managers")

(defvar fm-bookmark-enable-mounted-media t
  "[Experimental feature]
If nil, don't use to save time.
If t and system-type is Unix-like, show mounted media.
")

(defvar fm-bookmark-supported-file-managers
  '((kde4	.	"~/.kde4/share/apps/kfileplaces/bookmarks.xml")
    (gnome3	.	"~/.config/gtk-3.0/bookmarks")
    (pcmanfm	.	"~/.gtk-bookmarks"))
  "
gnome3 : Nautilus
kde4 : Dolphin
pcmanfm : PCManFM")

;; ======================================================
;; External Media (Experimental, Linux Only)
;; ======================================================

(defun fm-bookmark-get-and-parse-media-list ()
  "Get raw list from `mount` command and parse.
Output is like:
((\"/dev/sdb1\" . \"/var/run/media/kuanyui/kuanyui\")
 (\"/dev/sdb2\" . \"/var/run/media/kuanyui/windows\")
 (\"/dev/sdc1\" . \"/var/run/media/kuanyui/kuanyui 1G\"))"
(remove-duplicates
 (mapcar (lambda (line)
	   (save-match-data
	     (string-match "^\\([^ ]+\\) on \\(.+\\) type [^ ]+ [^ ]+$" line)
	     (cons (match-string 1 line)
		   (match-string 2 line)
		   )
	     ))
	 (split-string
	  (substring (shell-command-to-string "mount | grep 'media'") 0 -1) ;fuck you elisp
	  "\n"))
 :test (lambda (a b) (equal (car a) (car b)))))

(defun fm-bookmark-generate-media-list ()
  (if (member system-type '(gnu gnu/linux gnu/kfreebsd cygwin))
      (concat
       "\n"
       (propertize (make-string (1- fm-bookmark-buffer-width) ?=)
		   'face 'font-lock-comment-face)
       "\n"
       (mapconcat
	(lambda (item)
	  (propertize (file-name-base (cdr item))
		      'face 'dired-symlink
		      'href (cdr item)))
	(fm-bookmark-get-and-parse-media-list)
	"\n"
	))))

;; ======================================================
;; Main
;; ======================================================

(defun fm-bookmark--set-width (window n)
  "Make window N columns width."
  (let ((w (max n window-min-width)))
    (unless (null window)
      (if (> (window-width) w)
	  (shrink-window-horizontally (- (window-width) w))
	(if (< (window-width) w)
	    (enlarge-window-horizontally (- w (window-width))))))))
(defalias 'fm-bookmark #'fm-bookmark-open-buffer)

(defun fm-bookmark-open-buffer ()
  (interactive)
  (split-window-horizontally)
  (switch-to-buffer fm-bookmark-buffer-name)
  (kill-all-local-variables)
  (fm-bookmark--set-width (selected-window) fm-bookmark-buffer-width)
  (set-window-dedicated-p (selected-window) t)
  (let (buffer-read-only)
    (erase-buffer)
    (insert (fm-bookmark-generate-list))
    (if fm-bookmark-enable-mounted-media
	(insert (fm-bookmark-generate-media-list)))
    )
  (fm-bookmark-mode)
  ;; Disable linum
  (when (and (boundp 'linum-mode)
	     (not (null linum-mode)))
    (linum-mode -1))
  )

(defun fm-bookmark-generate-list ()
  "Generate a formatted dir list with text propertized.
kde4
  dir1
  dir2
gnome3
  dir1
  dir2
 "
  (mapconcat
   (lambda (fm-symbol)		;kde4, gnome3...etc
     (concat (propertize (symbol-name fm-symbol)
			 'face 'font-lock-comment-face)
	     "\n"
	     (mapconcat
	      (lambda (item)
		(propertize (concat "  " (car item))
			    'face 'dired-directory
			    'href (replace-regexp-in-string "^file://" "" (cdr item))))
	      (cond ((eq fm-symbol 'kde4)
		     (fm-bookmark-kde4-parser))
		    ((eq fm-symbol 'gnome3)
		     (fm-bookmark-gtk-parser fm-symbol))
		    ((eq fm-symbol 'pcmanfm)
		     (fm-bookmark-gtk-parser fm-symbol)))
	      "\n")))
   fm-bookmark-enabled-file-managers
   "\n"))

(defun fm-bookmark-open-this ()
  (interactive)
  (let ((link (get-text-property (point) 'href)))
    (if link
	(progn (delete-window (selected-window))
	       (kill-buffer fm-bookmark-buffer-name)
	       (find-file-other-window link)
	       )
      (message "There's no link"))
    ))

;; ======================================================
;; Parser
;; ======================================================

(defun fm-bookmark-kde4-parser ()
  (let* ((root (xml-parse-file (cdr (assoc 'kde4 fm-bookmark-supported-file-managers))))
	 (bookmarks (xml-get-children (car root) 'bookmark)))
    (remove-if
     #'null
     (mapcar (lambda (bookmark)
	       (unless (let ((metadata (apply #'append (xml-get-children (assoc 'info bookmark) 'metadata))))
			 (or (assoc 'isSystemItem metadata) ;No add if exist
			     (assoc 'OnlyInApp metadata)))  ;No add if exist
		 (cons
		  (nth 2 (car (xml-get-children bookmark 'title))) ;title
		  (decode-coding-string ;link
		   (url-unhex-string
		    (cdr (assoc 'href (nth 1 bookmark))))
		   'utf-8)
		  )
		 ))
	     bookmarks
	     )
     )))


(defun fm-bookmark-gtk-parser (symbol)
  "Available arg: 'gnome3 'pcmanfm"
  (with-temp-buffer
    (insert-file-contents (cdr (assoc symbol fm-bookmark-supported-file-managers)))
    (mapcar
     (lambda (str)
       (let* ((line (split-string str " " t))
	      (link (decode-coding-string (url-unhex-string (car line)) 'utf-8))
	      (title (if (> (length line) 1)
			 (mapconcat #'identity (cdr line) " ")
		       (file-name-base link))
		     ))

	 (cons title link))
       )
     (split-string (buffer-string) "\n" t))
    ))



(provide 'fm-bookmark)
;;; fm-bookmark.el ends here
