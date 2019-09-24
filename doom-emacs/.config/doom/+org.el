;;; ~/dotfiles/doom-emacs/.config/doom/+org.el -*- lexical-binding: t; -*-

;; Setup org agenda
(custom-set-variables
 '(org-directory "~/Dropbox/org")
 '(org-agenda-files (list org-directory)))

;; Use blank lines between texts and following headings
(setq org-blank-before-new-entry
      '((heading . t) (plain-list-item . t)))

;; Set org file associations
(after! org
    (setq org-file-apps
        '((auto-mode . emacs)
        ("\\.pdf::\\([0-9]+\\)?\\'" . "zathura %s -P %1")
        ("\\.pdf\\'" . "zathura %s")
        ("\\.x?html?\\'" . "qutebrowser %s")
        (directory . emacs))))
