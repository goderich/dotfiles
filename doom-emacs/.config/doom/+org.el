;;; ~/dotfiles/doom-emacs/.config/doom/+org.el -*- lexical-binding: t; -*-

;; Setup org agenda
(custom-set-variables
 '(org-directory "~/Dropbox/org")
 '(org-agenda-files (list org-directory)))

;; Use blank lines between texts and following headings
(setq org-blank-before-new-entry
      '((heading . t) (plain-list-item . t)))

(defun open! () (interactive)
       "A function that tries to open a link of the type

        file:/path/to/file.pdf::NN

        in zathura on the correct page, and in the event
        of any other link calls org-open-at-point instead."
       (let ((str (plist-get (cadr (org-element-context)) :raw-link)))
       (if (and (string-match-p "\.pdf::[[:digit:]]+$" str)
                (string-match-p "^file" str))
           (shell-command
            (apply #'format "zathura %s -P %s"
                   (split-string (replace-regexp-in-string "file:" "" str) "::")))
      (org-open-at-point))))
