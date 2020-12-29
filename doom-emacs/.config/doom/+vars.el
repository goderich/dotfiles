;;; ../../dotfiles/doom-emacs/.config/doom/+vars.el -*- lexical-binding: t; -*-

;; Define variables for various machines

(defun choose-var-helper (vars)
  "Comb VARS for three variables: VAR, X, and Y.
Assign X to VAR if on laptop, otherwise assign Y to VAR."
  (let ((var (car vars))
        (x (cadr vars))
        (y (caddr vars)))
    `(defvar ,var
       (if (string= (system-name) "iwaka-thinkpad")
           ,x ,y))))

(defmacro choose-vars (lst)
  "Use X if on laptop, otherwise Y.
Takes a list of (VAR X Y) lists as an argument."
  (macroexp-progn (mapcar #'choose-var-helper lst)))

(choose-vars ((my/org-directory "~/Dropbox/org"  "~/Documents/org")
              (my/default-bibliography "~/Dropbox/pap.bib" "~/Documents/bibliography.bib")
              (my/org-roam-directory "~/Dropbox/org/roam"  "~/Documents/org/roam")
              (my/ebib-notes "~/Dropbox/org/literature-notes"  "~/Documents/org/literature-notes")))
