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

  ;; Don't use blank lines between text and the following heading
  (setq org-blank-before-new-entry
        '((heading . nil) (plain-list-item . nil)))

  ;; Don't show empty lines between collapsed headings
  (setq org-cycle-separator-lines 0)

  ;; Org todo keywords and colours
  (setq org-todo-keywords
        '((sequence "TODO(t)" "|" "DONE(d)")
          (sequence "NEXT(n)" "WAITING(w)" "LATER(l)" "LOOP(p)" "|" "CANCELLED(c)")))

  ;; Use LOOP keyword for repeating tasks
  ;; (after marking them as done)
  (setq org-todo-repeat-to-state "LOOP")

  ;; My custom faces for todo items. I'm reusing most from:
  ;; ~/.emacs.d/modules/lang/org/config.el
  (with-no-warnings
    (custom-declare-face '+org-todo-important '((t (:inherit (bold default)))) ""))

  (setq org-todo-keyword-faces
        '(("LATER"     +org-todo-onhold)
          ("NEXT"      +org-todo-important)
          ("WAITING"   +org-todo-onhold)
          ("LOOP"      +org-todo-onhold)
          ("CANCELLED" +org-todo-cancel)))

  ;; In the datetime prompt, if entering a time that has already
  ;; passed today, interpret it as a time for tomorrow.
  (setq org-read-date-prefer-future 'time)

  ;; org-books
  (after! f

    (defvar org-books-file
      (f-join org-directory "books.org")
      "File to use with org-books mode")

    ;; Derive a separate mode for org-books-specific keybinds
    (define-derived-mode org-books-mode org-mode "Org books mode"
      "Major mode for managing prose using org.")

    ;; Autostart this mode when opening the org-books file
    (add-to-list 'auto-mode-alist `(,org-books-file . org-books-mode))
    ;; Add new books at the bottom of the chosen subtree
    (setq org-books-add-to-top nil)
    ;; Allow adding books under level 3 headings
    (setq org-books-file-depth 3)

    (defun org-books-setup+ ()
      (make-local-variable 'org-cycle-hook)
      (remove-hook 'org-cycle-hook #'org-cycle-hide-drawers 'local))

    (add-hook 'org-books-mode-hook #'org-books-setup+)

    ;;; org-books automatic genre tags settings
    (after! org-books
      ;; Here I have tags that need to be manually mapped to genres:
      (map-put-many! org-books-genre-tag-associations
                     "Humor" "funny"
                     "Dark Fantasy" "dark")

      ;; And here is a whitelist of tags that will simply be
      ;; transformed via `s-snake-case'.
      (let ((org-books-genre-tag-whitelist
             '("Mystery" "Urban Fantasy" "Vampires"
               "Crime" "Dark" "Horror")))
        (cl-loop for genre in org-books-genre-tag-whitelist
                 do (map-put! org-books-genre-tag-associations
                              genre
                              (s-snake-case genre)))))

    ) ; end of after! block

  ;; org-agenda settings
  ;; Display one week, always starting from Monday.
  (setq org-agenda-span 'week
        org-agenda-start-on-weekday 1
        org-agenda-start-day ".")

  ) ; end of after! block
