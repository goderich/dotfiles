;;; ~/dotfiles/doom-emacs/.config/doom/+org.el -*- lexical-binding: t; -*-

;; Setup org agenda
(custom-set-variables
 '(org-directory my/org-directory)
 '(org-agenda-files (list org-directory)))

(after! org

  ;; Set org file associations
  (setq org-file-apps
      `((auto-mode . emacs)
        (,(rx ".pdf::" (group (one-or-more digit)) string-end) . "zathura %s -P %1")
        (,(rx ".pdf" string-end) . "zathura %s")
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
        '(("LATER"     :inherit (warning bold))
          ("NEXT"      :inherit (bold default))
          ("WAITING"   :inherit (warning bold))
          ("CANCELLED" :inherit (error bold))))

  ;; org-books
  (after! f

    (defvar org-books-file
      (f-join (f-slash org-directory) "books.org")
      "File to use with org-books mode")

    ;; Derive a separate mode for org-books-specific keybinds
    (define-derived-mode org-books-mode org-mode "Org books mode")
    ;; Autostart this mode when opening the org-books file
    (add-to-list 'auto-mode-alist `(,org-books-file . org-books-mode))
    ;; Add new books at the bottom of the chosen subtree.
    (setq org-books-add-to-top nil))

  ;; org-agenda settings
  ;; Display one week starting from last Monday
  ;; (this forces the agenda to always start on a Monday).
  (setq org-agenda-span 'week)
  (add-hook 'org-agenda-mode-hook #'gd/set-org-agenda-start-day)

  ;; Align org tables with CJK characters
  (add-hook 'org-mode-hook #'valign-mode)
  ) ; end of after! block

;; org-roam config
(setq org-roam-directory my/org-roam-directory)

(setq +org-roam-open-buffer-on-find-file nil)
