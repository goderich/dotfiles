;;; ~/dotfiles/doom-emacs/.config/doom/+ui.el -*- lexical-binding: t; -*-

;; Soft wrap long lines
(global-visual-line-mode 1)

;; Disable auto-fill-mode
(remove-hook 'org-mode-hook #'auto-fill-mode)
(remove-hook 'markdown-mode-hook #'auto-fill-mode)

(defun my-markdown-mode-hook ()
  (auto-fill-mode -1))
(add-hook 'markdown-mode-hook #'my-markdown-mode-hook)
