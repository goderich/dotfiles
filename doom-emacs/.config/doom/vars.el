;;; ../../dotfiles/doom-emacs/.config/doom/+vars.el -*- lexical-binding: t; -*-

;; Define variables for various machines

(defun choose-var-helper (vars)
  "Comb VARS for three variables: VAR, X, and Y.
Assign X to VAR if on laptop, otherwise assign Y to VAR."
  (let ((var (cl-first vars))
        (x (cl-second vars))
        (y (cl-third vars)))
    `(defvar ,var
       (pcase (system-name)
         ("iwaka-thinkpad" ,x)
         (_ ,y)))))

(defmacro choose-vars (lst)
  "Use X if on laptop, otherwise Y.
Takes a list of (VAR X Y) lists as an argument."
  (macroexp-progn (mapcar #'choose-var-helper lst)))

(setq my/org-directory "~/Dropbox/org"
      my/default-bibliography "~/Dropbox/pap.bib"
      my/org-roam-directory "~/Dropbox/org/roam"
      my/ebib-notes "~/Dropbox/org/literature-notes"
      my/ebib-file-search-dir "~/Dropbox/ebib")
