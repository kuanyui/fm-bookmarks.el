;;; sys-bookmark.el --- Access existed FM bookmark in Dired  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  kuanyui

;; Author: kuanyui <azazabc123@gmail.com>
;; Keywords: files, convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'xml)

;; ======================================================
;; Major Mode
;; ======================================================

(defgroup sys-bookmark nil
  "Access existed FM bookmark in Dired"
  :prefix "sys-bookmark-"
  :link '(url-link "http://github.com/kuanyui/sys-bookmark.el"))

(defgroup sys-bookmark-faces nil
  "Faces used in sys-bookmark"
  :group 'sys-bookmark
  :group 'faces)

(defcustom sys-bookmark-mode-hook nil
  "Normal hook run when entering sys-bookmark-mode."
  :type 'hook
  :group 'sys-bookmark)

(defvar sys-bookmark-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Element insertion
    (define-key map (kbd "q") '(lambda ()
				 (interactive)
				 (delete-window (selected-window))
				 ))
    (define-key map (kbd "h") 'describe-mode)
    (define-key map (kbd "RET") 'sys-bookmark-open-this)
    map)
  "Keymap for Moedict major mode.")   ;document

(define-derived-mode sys-bookmark-mode nil "SysBookmarks"
  "Major mode for looking up Chinese vocabulary via Moedict API."
  (set (make-local-variable 'buffer-read-only) t)
  (hl-line-mode t)
  )

;; ======================================================
;; Variables
;; ======================================================

(defvar sys-bookmark-buffer-name "*SysBookmarks*"
  "Name of the buffer.")

(defvar sys-bookmark-enabled-file-manager '(kde4 gnome3 pcmanfm)
  "Enabled file managers")

(defvar sys-bookmark-supported-file-managers
  '((kde4	.	"~/.kde4/share/apps/kfileplaces/bookmarks.xml")
    (gnome3	.	"~/.config/gtk-3.0/bookmarks")
    (pcmanfm	.	"~/.gtk-bookmarks"))
  "
gnome3 : Nautilus
kde4 : Dolphin
pcmanfm : PCManFM")

;; ======================================================
;; Main
;; ======================================================

(defun sys-bookmark--set-width (window n)
  "Make window N columns width."
  (let ((w (max n window-min-width)))
    (unless (null window)
      (if (> (window-width) w)
          (shrink-window-horizontally (- (window-width) w))
        (if (< (window-width) w)
            (enlarge-window-horizontally (- w (window-width))))))))
(defalias 'sys-bookmark #'sys-bookmark-open-buffer)

(defun sys-bookmark-open-buffer ()
  (interactive)
  (split-window-horizontally)
  (switch-to-buffer sys-bookmark-buffer-name)
  (kill-all-local-variables)
  (sys-bookmark--set-width (selected-window) 25)
  (let (buffer-read-only)
    (erase-buffer)
    (set-window-dedicated-p (selected-window) t)
    (insert (sys-bookmark-generate-list))
    )
  (sys-bookmark-mode)
  ;; Disable linum
  (when (and (boundp 'linum-mode)
             (not (null linum-mode)))
    (linum-mode -1))
  )

(defun sys-bookmark-generate-list ()
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
		     (sys-bookmark-kde4-parser))
		    ((eq fm-symbol 'gnome3)
		     (sys-bookmark-gtk-parser fm-symbol))
		    ((eq fm-symbol 'pcmanfm)
		     (sys-bookmark-gtk-parser fm-symbol)))
	      "\n")))
   sys-bookmark-enabled-file-manager
   "\n"))

(defun sys-bookmark-open-this ()
  (interactive)
  (let ((link (get-text-property (point) 'href)))
    (if link
	(progn (delete-window (selected-window))
	       (kill-buffer sys-bookmark-buffer-name)
	       (find-file-other-window link)
	       )
      (message "There's no link"))
    ))

;; ======================================================
;; Parser
;; ======================================================

(defun sys-bookmark-kde4-parser ()
  (let* ((root (xml-parse-file (cdr (assoc 'kde4 sys-bookmark-supported-file-managers))))
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


(defun sys-bookmark-gtk-parser (symbol)
  "Available arg: 'gnome3 'pcmanfm"
  (with-temp-buffer
    (insert-file-contents (cdr (assoc symbol sys-bookmark-supported-file-managers)))
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



(provide 'sys-bookmark)
;;; sys-bookmark.el ends here
