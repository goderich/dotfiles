;;; ~/dotfiles/doom-emacs/.config/doom/+ui.el -*- lexical-binding: t; -*-

;; Set the theme
(setq doom-theme 'doom-solarized-light)

;; Setup the startup dashboard
(setq +doom-dashboard-banner-padding '(6 . 2))
(setq +doom-dashboard-banner-file (expand-file-name "emacs-e.svg" doom-user-dir))
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)
(add-hook! '+doom-dashboard-mode-hook (hide-mode-line-mode 1) (hl-line-mode -1))
(setq-hook! '+doom-dashboard-mode-hook evil-normal-state-cursor (list nil))

;; Soft wrap long lines
(global-visual-line-mode 1)

;; Maintain a small margin when scrolling
(setq scroll-margin 2)

;; Start the week on Mondays
(setq calendar-week-start-day 1)

;; Disable auto-fill-mode
(add-hook! '(markdown-mode-hook
             org-mode-hook
             mu4e-compose-mode-hook)
           #'turn-off-auto-fill)

;; Disable company-mode in the following modes
(setq company-global-modes
      '(not org-mode
            org-msg-edit-mode
            org-books-mode
            markdown-mode))

;; Add detailed diffs in magit
(setq magit-diff-refine-hunk 'all)

;; Disable smartparens-mode in org and markdown
(add-hook! '(org-mode-hook markdown-mode-hook)
           #'turn-off-smartparens-mode)

;; Do not display line numbers
(remove-hook! '(prog-mode-hook text-mode-hook conf-mode-hook)
              #'display-line-numbers-mode)

;; Make evil commands operate on logical, not visual lines
(after! evil
  (setq evil-respect-visual-line-mode nil))

;; Make substitution with :s global by default
(after! evil-vars
  (setq evil-ex-substitute-global t))

;; Show diff when committing in Magit
(after! magit-commit
  (setq magit-commit-show-diff t))

;; Do not automatically fullscreen a frame on entering writeroom mode
(after! writeroom-mode
  (setq writeroom-fullscreen-effect 'maximized))

;; Banish the pointer to the upper-right corner on any keypress
(mouse-avoidance-mode 'banish)

;; Center screen on various search functions
(advice-add! '(evil-ex-search-next evil-ex-search-previous
               evil-ex-search-forward evil-ex-search-backward
               evil-goto-mark evil-goto-mark-line
               better-jumper-jump-backward better-jumper-jump-forward
               evil-insert-resume ebib-goto-last-field
               git-gutter:next-hunk git-gutter:previous-hunk
               +lookup/definition)
             :after
             (lambda (&rest _) (recenter)))

;; Scroll line to top after some search functions
(advice-add! '(gd/consult-goto-org-heading org-books-jump-to-reading
               org-books-add-book)
             :after
             (lambda (&rest _) (evil-scroll-line-to-top nil)))

;; Disable popup windows by default
;; (make things open in new buffers instead).
;; I have also added a keybinding to toggle this mode (SPC t p).
(after! popup
  (+popup-mode -1))

;; Show Racket xp information in the echo area instead of a tooltip
(after! racket-xp
  (setq racket-show-functions '(racket-show-echo-area)))

;; Racket: indent all lines in a `do' block to the same level.
;; For some reason I couldn't make this work with Doom's `after!'
;; macros,so I used a hook instead.
(add-hook 'racket-xp-mode-hook #'gd/set-racket-do-indent)

(after! markdown-mode
  (setq markdown-list-indent-width 2))

;; Enter Info-mode with default emacs keybindings
(after! info
  (evil-set-initial-state 'Info-mode 'emacs))
(after! racket-stepper
  (evil-set-initial-state 'racket-stepper-mode 'emacs))

;; Use mixed pitch in the following modes
(add-hook! '(Info-mode-hook org-mode-hook markdown-mode-hook)
           #'mixed-pitch-mode)

;; Automatically make scripts executable on save
;; (checks for shebang at the beginning of the file).
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

;; Set which-key delay to near instant.
;; (NB. Setting to zero is currently not recommended.)
(setq which-key-idle-delay 0.1)

;; Use babashka REPL outside Clojure projects.
(setq cider-jack-in-default 'babashka)
