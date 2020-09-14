;;; ~/dotfiles/doom-emacs/.config/doom/+org.el -*- lexical-binding: t; -*-

;; Setup org agenda
(custom-set-variables
 '(org-directory my/org-directory)
 '(org-agenda-files (list org-directory)))

;; Fix ox-pandoc source blocks
(add-hook 'org-export-before-parsing-hook #'tb/ox-pandoc-fix-export-blocks)

;; Workaround for Return key not working in org-mode, see:
;; https://github.com/hlissner/doom-emacs/issues/3172
(add-hook 'org-mode-hook
          (lambda () (electric-indent-local-mode -1)))

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
        '((heading . t) (plain-list-item . nil)))

  ;; Org todo keywords and colours
  (setq org-todo-keywords
        '((sequence "TODO(t)" "|" "DONE(d)")
          (sequence "NEXT(n)" "WAITING(w)" "LATER(l)" "|" "CANCELLED(c)")))

  (setq org-todo-keyword-faces
        '(("LATER" :inherit (warning bold))
          ("NEXT" :inherit (bold default))
          ("WAITING" :inherit (warning bold))
          ("CANCELLED" :inherit (error bold)))))

;; org-roam config
(setq org-roam-directory my/org-roam-directory)

(use-package org-roam-bibtex
  :after org-roam
  :hook (org-roam-mode . org-roam-bibtex-mode))

(setq +org-roam-open-buffer-on-find-file nil)
