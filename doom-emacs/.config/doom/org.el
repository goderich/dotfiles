;;; ~/dotfiles/doom-emacs/.config/doom/+org.el -*- lexical-binding: t; -*-

;; Setup org agenda
(custom-set-variables
 '(org-directory my/org-directory)
 '(org-agenda-files (list org-directory)))

;; Bugfix for org-id not working properly
;; TODO: submit bug report
(setq org-id-locations-file (doom-path doom-cache-dir ".org-id-locations"))

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
      '((heading . auto) (plain-list-item . auto)))

;; Don't show empty lines between collapsed headings
(setq org-cycle-separator-lines 0)

;; Org todo keywords and colours
(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d)")
        (sequence "NEXT(n)" "WAITING(w)" "LATER(l)" "LOOP(p)" "|" "CANCELLED(c)")))

;; Use LOOP keyword for repeating tasks
;; (after marking them as done)
(setq org-todo-repeat-to-state "LOOP")

;; Archive everything from org files in one directory
;; into a single hidden file.
(setq org-archive-location ".archive.org::")

;; My custom faces for todo items. I'm reusing most from:
;; ~/.emacs.d/modules/lang/org/config.el
(with-no-warnings
  (custom-declare-face '+org-todo-important '((t (:inherit (bold default)))) ""))

(setq org-todo-keyword-faces
      '(("LATER"     +org-todo-onhold)
        ("NEXT"      +org-todo-important)
        ("WAITING"   +org-todo-onhold)
        ("LOOP"      +org-todo-onhold)
        ("CANCELLED" org-done)))

;; In the datetime prompt, if entering a time that has already
;; passed today, interpret it as a time for tomorrow.
(setq org-read-date-prefer-future 'time)

;; org-agenda settings
;; Display one week, always starting from Monday.
(setq org-agenda-span 'week
      org-agenda-start-on-weekday 1
      org-agenda-start-day ".")

;; Show full context after switching to an item from agenda
(map-put-many! org-fold-show-context-detail
               'agenda  'tree
               'default 'tree)

;; Load org-books config
(after! f (load! "org-books"))

(add-hook 'gd/pandoc-org->pdf-hook #'gd/org-check-duplicates-all)
