;;; ../../dotfiles/doom-emacs/.config/doom/+vars.el -*- lexical-binding: t; -*-

;; Define variables for various machines

(defmacro choose-var (var x y)
  "Use X if on laptop, otherwise Y."
  `(defvar ,var
        (if (string= (system-name) "iwaka-thinkpad")
            ,x ,y)))

(choose-var my/org-directory "~/Dropbox/org"  "~/Documents/org")
(choose-var my/org-roam-directory "" "~/Documents/org/roam")
(choose-var my/default-bibliography "" "~/Documents/bibliography.bib")
(choose-var my/ebib-notes "" "~/Documents/org/literature-notes")
