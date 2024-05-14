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

;; Properly align tables with CJK in them
(package! valign)

;; Do not show email notifications in bar
(package! mu4e-alert :disable t)

;; Don't use Doom's snippets
(package! doom-snippets :ignore t)
;; Replace with yasnippet's default snippets
(package! yasnippet-snippets)

;; Use variable pitch in some environments
(package! mixed-pitch)

;; Trying out mystery package
(package! hyperbole)

;; Odin lang
(package! odin-mode :recipe (:host github
                             :repo "mattt-b/odin-mode"))
