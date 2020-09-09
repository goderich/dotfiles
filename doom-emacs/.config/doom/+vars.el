;;; ../../dotfiles/doom-emacs/.config/doom/+vars.el -*- lexical-binding: t; -*-

;; Define variables for various machines

(defmacro choose-var (var x y)
  "Use X if on laptop, otherwise Y."
  `(defvar ,var
        (if (string= (system-name) "iwaka-thinkpad")
            ,x ,y)))

(choose-var my/org-directory "~/Dropbox/org"  "~/Documents/org")
(choose-var my/default-bibliography "~/Dropbox/pap.bib" "~/Documents/bibliography.bib")

;; Variables with relative paths
(defvar my/org-roam-directory (f-join my/org-directory "roam"))
(defvar my/ebib-notes (f-join my/org-directory "literature-notes"))
