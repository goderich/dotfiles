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

;; Do not show email notifications in bar
(package! mu4e-alert :disable t)

;; Don't use Doom's snippets
(package! doom-snippets :ignore t)
;; Replace with yasnippet's default snippets
(package! yasnippet-snippets)

;; Use variable pitch in some environments
(package! mixed-pitch)

;; Use newest evil-collection for mu4e keybindings,
;; see https://github.com/emacs-evil/evil-collection/issues/695
(unpin! evil-collection)
(package! evil-collection
  :recipe (:repo "emacs-evil/evil-collection" :branch "master"))
