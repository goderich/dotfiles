;;; package ---- my own config
;;; private/iwaka/config.el -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(load! "+fonts")
(load! "+org")
(load! "+ui")
(load! "+keybindings")
(load! "+referencing")

;; Disable company-mode in org and markdown
(setq company-global-modes '(not org-mode markdown-mode))

;; Disable smartparens-mode in org and markdown
(add-hook 'org-mode-hook #'turn-off-smartparens-mode)
(add-hook 'markdown-mode-hook #'turn-off-smartparens-mode)

;; Use external applications to open pdfs
(after! org
  (add-to-list 'org-file-apps '("\\.pdf\\'" . "xdg-open %s")))

;;; config.el ends here
