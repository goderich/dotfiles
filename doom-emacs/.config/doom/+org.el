;;; ~/dotfiles/doom-emacs/.config/doom/+org.el -*- lexical-binding: t; -*-

;; Setup org agenda
(custom-set-variables
 '(org-directory "~/Dropbox/org")
 '(org-agenda-files (list org-directory)))

(after! org

  ;; Set org file associations
  (setq org-file-apps
      '((auto-mode . emacs)
      ("\\.pdf::\\([0-9]+\\)?\\'" . "zathura %s -P %1")
      ("\\.pdf\\'" . "zathura %s")
      (directory . emacs)))

  ;; browser needs to be set with a separate function
  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program "qutebrowser")

  ;; Use blank lines between texts and following headings
  (setq org-blank-before-new-entry
        '((heading . t) (plain-list-item . t)))

  ;; Org todo keywords and colours
  (setq org-todo-keywords
        '((sequence "TODO(t)" "|" "DONE(d)")
          (sequence "NEXT(n)" "WAITING(w)" "LATER(l)" "|" "CANCELLED(c)")))

  (setq org-todo-keyword-faces
        '(("LATER" :inherit (warning bold))
          ("NEXT" :inherit (bold default))
          ("WAITING" :inherit (warning bold))
          ("CANCELLED" :inherit (error bold)))))
