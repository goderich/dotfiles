;;; ~/dotfiles/doom-emacs/.config/doom/+org.el -*- lexical-binding: t; -*-

;; Setup org agenda
(custom-set-variables
 '(org-directory "~/Dropbox/org")
 '(org-agenda-files (list org-directory)))

;; Use blank lines between texts and following headings
(setq org-blank-before-new-entry
      '((heading . t) (plain-list-item . t)))
