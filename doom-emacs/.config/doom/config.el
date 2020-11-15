;;; package ---- my own config
;;; private/iwaka/config.el -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(load! "+vars")
(load! "+fonts")
(load! "+org")
(load! "+ui")
(load! "functions")
(load! "+keybindings")
(load! "+referencing")
(if (string= (system-name) "iwaka-thinkpad")
    (load! "+mail"))

;; Disable company-mode in the following modes
(setq company-global-modes
      '(not org-mode
            org-msg-edit-mode
            markdown-mode))

;; Add detailed diffs in magit
(setq magit-diff-refine-hunk 'all)

;; TAB always indents
;; (to insert an actual TAB, use "C-v TAB")
(setq tab-always-indent t)

;; Disable smartparens-mode in org and markdown
(add-hook 'org-mode-hook #'turn-off-smartparens-mode)
(add-hook 'markdown-mode-hook #'turn-off-smartparens-mode)

;;; config.el ends here
