;;; ../../dotfiles/doom-emacs/.config/doom/+vars.el -*- lexical-binding: t; -*-

;; Define variables for various machines

;; Laptop
(when (string= (system-name) "iwaka-thinkpad")
  (setq my/org-directory "~/org"
        my/default-bibliography "~/files/academic/bib/bibliography.bib"
        my/ebib-notes "~/org/literature-notes"
        my/ebib-file-search-dir "~/files/syncthing/papers/ebib"
        my/ebib-import-directory "~/files/syncthing/papers/papers/articles"))

;; Office PC
(when (string= (system-name) "goderich-ncue")
  (setq my/org-directory "~/org"
        my/default-bibliography "~/files/academic/bib/bibliography.bib"
        my/ebib-notes "~/org/literature-notes"
        my/ebib-file-search-dir "~/files/papers/ebib"
        my/ebib-import-directory "~/files/papers/papers/articles"))
