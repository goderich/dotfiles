;;; org-moar.el --- Poor man's org-roam -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 郭育賢 Andre Goderich
;;
;; Author: 郭育賢 Andre Goderich <https://github.com/goderich>
;; Maintainer: 郭育賢 Andre Goderich <goderich@gm.ncue.edu.tw>
;; Created: August 12, 2021
;; Modified: August 12, 2021
;; Version: 0.0.1
;; Keywords: outlines org zettelkasten
;; Homepage: https://github.com/iwaka/org-moar
;; Package-Requires: ((emacs "27.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Poor man's org-roam
;;
;;; Code:
(require 'f)
(require 'ivy)
(require 'org)

(defcustom org-moar-directory "~/Dropbox/org/notes/"
  "The main directory where org-moar searches for files and creates new ones."
  :type 'directory
  :group 'org-moar)

(defun org-moar-get-filename-and-title-pair (str)
  "Split STR into a cons cell of (title . filename).
The function expects ripgrep's output format."
  (let ((title (-second-item (s-split-up-to " " str 1)))
        (filename (car (s-split-up-to ":" str 1))))
    (cons title filename)))

(defun org-moar-get-candidates ()
  "Return an alist of titles and filenames in `org-moar-directory'.
Depends on ripgrep.

Currently the command is synchronous and will choke on
a large enough number of files."
  (->>
   (shell-command-to-string
    (format "rg -i --type org -F '#+title:' %s" org-moar-directory))
   (s-chomp)
   (s-lines)
   (-map #'org-moar-get-filename-and-title-pair)))

(defun org-moar-create-note (title)
  "Create new note with the TITLE property."
  (let ((filename (f-join org-moar-directory (format-time-string "%Y%m%d%H%M%S%2N.org"))))
    (f-touch filename)
    (f-write-text (format "#+TITLE: %s\n" title) 'utf-8 filename)
    (find-file filename)))

(defun org-moar-find-create-dispatch (selection)
  "Open an existing file or create a new one, based on SELECTION."
  (if (stringp selection)
      (org-moar-create-note selection)
    (find-file (cdr selection))))

;;;###autoload
(defun org-moar-open-note ()
  "Open an existing note or create a new one."
  (interactive)
  (ivy-read "Note title: "
            (org-moar-get-candidates)
            :action #'org-moar-find-create-dispatch
            :preselect "Index"))

(defun org-moar-insert-link (selection)
  "Insert a link to the file in SELECTION."
  (let* ((filename (f-short (cdr selection)))
         (link-text (read-string "Link text: " (car selection)))
         (link-string (org-link-make-string filename link-text)))
    (unless (bolp)
      (insert " "))
    (insert link-string)))

;;;###autoload
(defun org-moar-link-note ()
  "Create a link to an existing note."
  (interactive)
  (ivy-read "Note to link to: "
            (org-moar-get-candidates)
            :require-match t
            :action #'org-moar-insert-link))

(provide 'org-moar)
;;; org-moar.el ends here
