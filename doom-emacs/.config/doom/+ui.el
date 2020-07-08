;;; ~/dotfiles/doom-emacs/.config/doom/+ui.el -*- lexical-binding: t; -*-

;; Soft wrap long lines
(global-visual-line-mode 1)

;; Disable auto-fill-mode
(remove-hook 'org-mode-hook #'auto-fill-mode)
(remove-hook 'markdown-mode-hook #'auto-fill-mode)

(defun my-markdown-mode-hook ()
  (auto-fill-mode -1))
(add-hook 'markdown-mode-hook #'my-markdown-mode-hook)

;; Do not display line numbers
(remove-hook! '(prog-mode-hook text-mode-hook conf-mode-hook)
              #'display-line-numbers-mode)

;; Center screen on consecutive searches (n/N)
(advice-add 'evil-ex-search-next :after
            (lambda (&rest _)
              (evil-scroll-line-to-center (line-number-at-pos))))
(advice-add 'evil-ex-search-previous :after
            (lambda (&rest _)
              (evil-scroll-line-to-center (line-number-at-pos))))
