;; -*- no-byte-compile: t; -*-
;;; private/iwaka/packages.el

;; No-distraction writing
(package! writeroom-mode)

;; Bibliography management
(package! ebib)

;; Nested alist manipulation
(package! let-alist)

;; jetbrains darcula theme
(package! jetbrains-darcula-theme
  :recipe (:host github :repo "ianpan870102/jetbrains-darcula-emacs-theme"))
