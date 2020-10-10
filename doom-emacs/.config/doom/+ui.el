;;; ~/dotfiles/doom-emacs/.config/doom/+ui.el -*- lexical-binding: t; -*-

;; Soft wrap long lines
(global-visual-line-mode 1)

;; Disable auto-fill-mode
(add-hook! '(markdown-mode-hook
             org-mode-hook
             mu4e-compose-mode-hook)
           #'turn-off-auto-fill)

;; Do not display line numbers
(remove-hook! '(prog-mode-hook text-mode-hook conf-mode-hook)
              #'display-line-numbers-mode)

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

;; Center screen on consecutive searches (n/N)
(advice-add 'evil-ex-search-next :after
            (lambda (&rest _)
              (evil-scroll-line-to-center (line-number-at-pos))))
(advice-add 'evil-ex-search-previous :after
            (lambda (&rest _)
              (evil-scroll-line-to-center (line-number-at-pos))))
