;;; ../../dotfiles/doom-emacs/.config/doom/+vars.el -*- lexical-binding: t; -*-

;; Define variables for various machines

(defmacro choose-var (var x y)
  "Use X if on laptop, otherwise Y."
  `(defvar ,var
        (if (string= (system-name) "iwaka-thinkpad")
            ,x ,y)))

(choose-var my/org-directory "~/Dropbox/org" "")
