;; -*- no-byte-compile: t; -*-
;;; private/iwaka/packages.el

;; No-distraction writing
(package! writeroom-mode)

;; Bibliography management
(package! ebib)

;; Nested alist manipulation
(package! let-alist)

;; Book management
(package! org-books :recipe (:host github
                             :repo "goderich/org-books"))

;; Using pandoc within Emacs
(package! pandoc-mode)

;; Properly align tables with CJK in them
(package! valign)
