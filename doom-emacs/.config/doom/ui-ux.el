;;; ~/dotfiles/doom-emacs/.config/doom/+ui.el -*- lexical-binding: t; -*-

;; Soft wrap long lines
(global-visual-line-mode 1)

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
            markdown-mode))

;; Add detailed diffs in magit
(setq magit-diff-refine-hunk 'all)

;; Disable smartparens-mode in org and markdown
(add-hook! '(org-mode-hook markdown-mode-hook)
           #'turn-off-smartparens-mode)

;; Do not display line numbers
(remove-hook! '(prog-mode-hook text-mode-hook conf-mode-hook)
              #'display-line-numbers-mode)

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
(center-screen-after '(evil-ex-search-next
                       evil-ex-search-previous
                       evil-ex-search-forward
                       evil-ex-search-backward
                       git-gutter:next-hunk
                       git-gutter:previous-hunk))

;; Disable popup windows by default
;; (make things open in new buffers instead).
;; I have also added a keybinding to toggle this mode (SPC t p).
(after! popup
  (+popup-mode -1))

;; Show Racket xp information in the echo area instead of a tooltip
(after! racket-xp
  (setq racket-show-functions '(racket-show-echo-area)))
