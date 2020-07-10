;;; package ---- my own config
;;; private/iwaka/config.el -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(load! "+fonts")
(load! "+org")
(load! "+ui")
(load! "functions")
(load! "+keybindings")
(load! "+referencing")

;; Disable company-mode in org and markdown
(setq company-global-modes '(not org-mode markdown-mode))

;; Add detailed diffs in magit
(setq magit-diff-refine-hunk 'all)

;; Disable smartparens-mode in org and markdown
(add-hook 'org-mode-hook #'turn-off-smartparens-mode)
(add-hook 'markdown-mode-hook #'turn-off-smartparens-mode)

;;; config.el ends here
