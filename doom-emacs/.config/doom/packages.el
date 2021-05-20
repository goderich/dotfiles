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

(package! ox-pandoc :recipe (:host github
                             :repo "goderich/ox-pandoc"))

(package! pandoc-mode)
