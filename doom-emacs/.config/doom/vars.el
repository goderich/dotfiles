;;; ../../dotfiles/doom-emacs/.config/doom/+vars.el -*- lexical-binding: t; -*-

;; Define variables for various machines

(when (string= (system-name) "iwaka-thinkpad")
  (setq my/org-directory "~/Dropbox/org"
        my/default-bibliography "~/Dropbox/pap.bib"
        my/ebib-notes "~/Dropbox/org/literature-notes"
        my/ebib-file-search-dir "~/files/syncthing/papers/ebib"))
