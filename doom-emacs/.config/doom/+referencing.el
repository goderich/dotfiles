;;; ~/dotfiles/doom-emacs/.config/doom/+referencing.el -*- lexical-binding: t; -*-

(setq reftex-default-bibliography "~/pap.bib")
(setq org-ref-default-bibliography reftex-default-bibliography)
(setq bibtex-completion-bibliography reftex-default-bibliography)

; Activate Yasnippets in BibTeX files
(add-hook 'bibtex-mode-hook #'yas-minor-mode-on)

; Formatting of helm-bibtex results
(after! bibtex-completion
  (setq bibtex-completion-display-formats
        '((t . "${author:18} ${year:5} ${title:*}"))))
