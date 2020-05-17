;;; ~/dotfiles/doom-emacs/.config/doom/+referencing.el -*- lexical-binding: t; -*-

(setq reftex-default-bibliography "~/pap.bib")
(setq org-ref-default-bibliography reftex-default-bibliography)
(setq bibtex-completion-bibliography reftex-default-bibliography)

; Formatting of helm-bibtex results
(after! org-ref-helm-bibtex
  (setq bibtex-completion-display-formats
        '((t . "${author:18} ${year:5} ${title:*}"))))
